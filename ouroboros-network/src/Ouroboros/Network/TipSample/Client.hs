{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.TipSample.Client
  ( -- * 'TipSampleClient'
    TipSamplePolicy (..)
  , TipSampleActions (..)
  , tipSampleClient

    -- * Errors & traces
  , TipSampleClientTrace (..)
  , TipSampleClientValidationError (..)
  ) where

import           Control.Arrow (second)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime

import           Control.Tracer (Tracer, traceWith)


import           Control.Exception (Exception (..))
import           Data.Foldable (traverse_)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           System.Random

import           Network.TypedProtocol.Pipelined ( N (..)
                                                 , Nat (Succ, Zero)
                                                 , unsafeIntToNat )

import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Network.Block (Tip (..), StandardHash)
import           Ouroboros.Network.Mux (ScheduledStop, RunOrStop (..))
import           Ouroboros.Network.TipSample.TipFragment ( TipFragment
                                                         , TipFragment' ((:>), Empty)
                                                         , Timed (..)
                                                         )
import qualified Ouroboros.Network.TipSample.TipFragment as TF
import           Ouroboros.Network.Protocol.TipSample.Client




-- | Policies which drive the 'TipSample' client.
--
data TipSamplePolicy = TipSamplePolicy {
      -- | Slot range; The client will keep only that oldest slot that many
      -- slots behind the last received slot.
      --
      tspRange                    :: Word64,

      -- | Range from which we randomly choose to a number of slots ahead of
      -- the current slot.
      --
      tspSlotRange                :: (Word64, Word64),

      -- | Weight for asking a single tip.
      --
      tspFollowSingleTipWeight    :: Rational,

      -- | Weight for asking a multiple tips.
      tspFollowMultipleTipsWeight :: Rational,

      -- | Range from which we randomly choose how many tips to follow.  The
      -- minimum range should be greater than @1@.
      --
      tspMultipleTipsRange        :: (Word64, Word64)
    }


data TipSampleDecision =
      -- | Ask for a single tip after that many slots of the current slot.
      FollowSingleTip Word64

      -- | Ask for multiple tips
    | FollowMultipleTips
         Word64 -- ^ after that many slots of the current slot.
         Word64 -- ^ number of tips to receive


randomTipSampleDecision
    :: RandomGen g
    => TipSamplePolicy
    -> g
    -> (TipSampleDecision, g)
randomTipSampleDecision
    TipSamplePolicy {
      tspFollowSingleTipWeight,
      tspFollowMultipleTipsWeight,
      tspSlotRange,
      tspMultipleTipsRange
    }
    g =
      case randomR (0 :: Int, 100) g of
        (a, g') | toRational a <= 100 * tspFollowSingleTipWeight / total
                -> case randomR tspSlotRange g' of
                    (n, g'') -> (FollowSingleTip n , g'')

                | otherwise
                -> case randomR tspSlotRange g' of
                    (n, g'') -> case randomR tspMultipleTipsRange g'' of
                      (r, g''') ->  (FollowMultipleTips n r, g''')
  where
    total = tspFollowSingleTipWeight + tspFollowMultipleTipsWeight


data TipSampleState header = TipSampleState {
    tssLastTip :: !(Tip header),
    tssGen     :: !StdGen
  }


data TipSampleClientValidationError header =
    TipSampleWrongSlot SlotNo (Tip header)
  | TipSampleWrongTip (Tip header) (Tip header)
  deriving Show

instance (Typeable header, StandardHash header)
    => Exception (TipSampleClientValidationError header)

afterSlotNo :: Tip header
            -> Word64
            -> SlotNo
afterSlotNo (Tip (SlotNo slotNo) _ _) x = SlotNo (slotNo + x)
afterSlotNo TipGenesis                x = SlotNo x


compareTipsBySlotNo :: Tip header
                    -> Tip header
                    -> Ordering
compareTipsBySlotNo (Tip slotNo _ _) (Tip slotNo' _ _) = slotNo `compare` slotNo'
compareTipsBySlotNo Tip{}            TipGenesis        = GT
compareTipsBySlotNo TipGenesis       Tip{}             = LT
compareTipsBySlotNo TipGenesis       TipGenesis        = EQ


data TipSampleActions m header = TipSampleActions {
    -- | Modify 'TipFragment' and return trace of the operation.
    --
    tsaModifyTipFragment :: forall a. (TipFragment header -> (a, TipFragment header)) -> STM m a,

    -- | Get current 'SlotNo'.  The client maintains 'TipFragment' only for
    -- a 'tspRange' of slots.
    --
    tsaGetCurrentSlotNo  :: STM m SlotNo,

    -- | 'tipSampleClient' tracer.
    --
    tsaTracer            :: Tracer m (TipSampleClientTrace header)
  }


-- | 'TipSample' client application.  It sends requests according to
-- 'TipSamplePolicy', and records received tips.
--
-- It maintains the invariant that the slots increase monotonically in
-- 'TipFragment'.  When we request multiple tips, the 'SlotNo' can go backward
-- (if the remote peer rollbacked its chain).  In this case we rollback the
-- 'TipFragment' too.
--
tipSampleClient :: forall header m.
                   ( MonadMonotonicTime m
                   , MonadSTM           m
                   , MonadThrow         m
                   , StandardHash header
                   , Typeable     header
                   )
                => TipSamplePolicy
                -> TipSampleActions m header
                -> ScheduledStop m
                -> StdGen
                -> TipSampleClient (Tip header) m ()
tipSampleClient tipSamplePolicy@TipSamplePolicy { tspRange }
                TipSampleActions {
                  tsaModifyTipFragment,
                  tsaGetCurrentSlotNo,
                  tsaTracer
                }
                scheduledStop g =
    TipSampleClient $ pure $ clientStIdle (TipSampleState TipGenesis g)
  where
    -- We only validate 'SlotNo'; at this point we cannot trust the received
    -- hash or 'BlockNo'.  Its validation is deffered until we actually use the
    -- tip.
    --
    -- Note: we only validate the 'Tip' if we expect it to move forward.
    validateTip :: Tip header
                -> SlotNo
                -> m ()
    validateTip tip@(Tip tipSlotNo _ _) requestedSlotNo =
      if tipSlotNo >= requestedSlotNo
        then pure ()
        else do
          traceWith tsaTracer (TipSampleClientProtocolValidation requestedSlotNo tip)
          throwM (TipSampleWrongSlot requestedSlotNo tip)
    validateTip tip@TipGenesis requestedSlotNo = do
      traceWith tsaTracer (TipSampleClientProtocolValidation requestedSlotNo tip)
      throwM (TipSampleWrongSlot requestedSlotNo tip)


    clientStIdle :: TipSampleState header
                 -> ClientStIdle (Tip header) m ()
    clientStIdle st@TipSampleState { tssLastTip, tssGen } =

      case randomTipSampleDecision tipSamplePolicy tssGen of
        (FollowSingleTip n, tssGen') ->
          let requestedSlotNo = afterSlotNo tssLastTip n
              r = Succ Zero
          in SendMsgFollowTip
              r requestedSlotNo
              (receiveTips (Just requestedSlotNo) r st { tssGen = tssGen' })

        (FollowMultipleTips n r, tssGen') ->
          let requestedSlotNo = afterSlotNo tssLastTip n
              r' = unsafeIntToNat (fromIntegral r)
          in SendMsgFollowTip
              r' requestedSlotNo
              (receiveTips (Just requestedSlotNo) r' st { tssGen = tssGen' })


    receiveTips :: Maybe SlotNo
                -- used to validate the first tip
                -> Nat (S n)
                -> TipSampleState header
                -> HandleTips (S n) (Tip header) m ()

    receiveTips mbSlotNo (Succ Zero) st =
      ReceiveLastTip $ \tip -> do
        t <- getMonotonicTime
        -- validate the tip but only if 'mvSlotNo' is 'Just'
        traverse_ (validateTip tip) mbSlotNo
        (traceMessage, ss) <-
          atomically $ do
            traceMessage <- rollbackOrRollforward st (Timed t tip)
            (traceMessage,) <$> scheduledStop
        traceWith tsaTracer traceMessage

        case ss of
          Run ->
            pure $ clientStIdle st { tssLastTip = tip }
          Stop ->
            pure $ SendMsgDone ()

    receiveTips mbSlotNo (Succ n@Succ {}) st =
      ReceiveTip $ \tip -> do
        t <- getMonotonicTime
        -- validate the tip but only if 'mvSlotNo' is 'Just'
        traverse_ (validateTip tip) mbSlotNo
        traceMessage <- atomically (rollbackOrRollforward st (Timed t tip))
        traceWith tsaTracer traceMessage
        pure $ receiveTips Nothing n st { tssLastTip = tip }


    -- Trim the 'TipFragment' whenever we modify the shared value.  We only
    -- keep tips 'tpsRange' from the current 'SlotNo'.
    modifyTipFragment
      :: forall a.
        (TipFragment header -> (a, TipFragment header))
      -> STM m a
    modifyTipFragment f = do
      currentSlotNo <- tsaGetCurrentSlotNo
      let initialSlotNo = case currentSlotNo of
            SlotNo a | a >= tspRange -> SlotNo (a - tspRange)
                     | otherwise     -> SlotNo 0
      tsaModifyTipFragment (second (TF.dropOldestUntilSlotNo initialSlotNo) . f)


    rollbackOrRollforward
        :: TipSampleState header
        -> Timed (Tip header)
        -> STM m (TipSampleClientTrace header)
    rollbackOrRollforward TipSampleState { tssLastTip } tt =
      modifyTipFragment $ \tf ->
        case tssLastTip `compareTipsBySlotNo` timedData tt of
          GT -> ( TipSampleClientAddTip (timedData tt)
                , tf :> tt
                )

            -- There was a rollback.
            -- TODO: Currently we rollback our state, but in the future we
            -- could have a more clever solution.
          _ -> case timedData tt of
                  Tip slotNo _ _ -> ( TipSampleClientRollback (timedData tt)
                                    , TF.dropNewestUntilSlotNo (pred slotNo) tf :> tt
                                    )
                  TipGenesis     -> ( TipSampleClientRollback (timedData tt)
                                    , Empty
                                    )


--
-- Trace
--

data TipSampleClientTrace header =
    TipSampleClientProtocolValidation SlotNo !(Tip header)
  | TipSampleClientAddTip   !(Tip header)
  | TipSampleClientRollback !(Tip header)
  deriving (Eq, Show)
