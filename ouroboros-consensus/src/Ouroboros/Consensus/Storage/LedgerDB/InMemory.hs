{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module Ouroboros.Consensus.Storage.LedgerDB.InMemory (
     -- * LedgerDB proper
     LedgerDB
   , LedgerDbParams(..)
   , ledgerDbDefaultParams
   , ledgerDbWithAnchor
   , ledgerDbFromGenesis
     -- ** ChainSummary
   , ChainSummary(..)
   , encodeChainSummary
   , decodeChainSummary
     -- ** Queries
   , ledgerDbCurrent
   , ledgerDbTip
   , ledgerDbAnchor
     -- ** Past ledger states
   , ledgerDbPast
     -- ** Running updates
   , Ap(..)
   , AnnLedgerError(..)
   , ResolveBlock
   , ResolvesBlocks(..)
   , ThrowsLedgerError(..)
   , defaultThrowLedgerErrors
   , defaultResolveBlocks
   , defaultResolveWithErrors
     -- ** Updates
   , ExceededRollback(..)
   , ledgerDbPush
   , ledgerDbSwitch
     -- * Exports for the benefit of tests
     -- ** Additional queries
   , ledgerDbChainLength
   , ledgerDbToList
   , ledgerDbMaxRollback
   , ledgerDbOldStates
   , ledgerDbIsSaturated
   , ledgerDbCountToPrune
     -- ** Pure API
   , ledgerDbPush'
   , ledgerDbPushMany'
   , ledgerDbSwitch'
   ) where

import           Prelude hiding (mod, (/))

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import qualified Codec.Serialise.Encoding as Enc
import           Control.Monad.Except hiding (ap)
import           Control.Monad.Reader hiding (ap)
import           Data.Foldable (toList)
import           Data.Functor.Identity
import           Data.Kind (Constraint)
import           Data.Proxy
import           Data.Sequence.Strict (StrictSeq ((:|>), Empty), (|>))
import qualified Data.Sequence.Strict as Seq
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.CBOR (decodeWithOrigin,
                     encodeWithOrigin)

{-------------------------------------------------------------------------------
  Ledger DB types
-------------------------------------------------------------------------------}

-- | Internal state of the ledger DB
--
-- The ledger DB looks like
--
-- > anchor |> snapshots <| current
--
-- where @anchor@ records the oldest known snapshot and @current@ the most
-- recent. The anchor is the oldest point we can rollback.
--
-- We take snapshots after each block is applied and keep in memory a window
-- of the last k snapshots. We have verified empirically that the overhead of
-- keeping k snapshots in memory is small and doesn't exceed 9MiB. That's
-- because there is a lot of sharing between consecutive snapshots.
--
-- As an example, suppose we have @k = 6@. The ledger DB grows as illustrated
-- below, where we indicate the anchor number of blocks, the stored snapshots,
-- and the current ledger.
--
-- > anchor |> #   [ snapshots                      <| tip
-- > ---------------------------------------------------------------------------
-- > G     |> (0) [ ]                             <| G
-- > G     |> (1) [ L1]                           <| L1
-- > G     |> (2) [ L1,  L2]                      <| L2
-- > G     |> (3) [ L1,  L2,  L3]                 <| L3
-- > G     |> (4) [ L1,  L2,  L3,  L4]            <| L4
-- > G     |> (5) [ L1,  L2,  L3,  L4,  L5]       <| L5
-- > G     |> (6) [ L1,  L2,  L3,  L4,  L5,  L6]  <| L6
-- > L1    |> (6) [ L2,  L3,  L4,  L5,  L6,  L7]  <| L7
-- > L2    |> (6) [ L3,  L4,  L5,  L6,  L7,  L8]  <| L8
-- > L3    |> (6) [ L4,  L5,  L6,  L7,  L8,  L9]  <| L9   (*)
-- > L4    |> (6) [ L5,  L6,  L7,  L8,  L9,  L10] <| L10
-- > L5    |> (6) [*L6,  L7,  L8,  L9,  L10, L11] <| L11
-- > L6    |> (6) [ L7,  L8,  L9,  L10, L11, L12] <| L12
-- > L7    |> (6) [ L8,  L9,  L10, L12, L12, L13] <| L13
-- > L8    |> (6) [ L9,  L10, L12, L12, L13, L14] <| L14
--
-- The ledger DB must guarantee we must at all times be able to roll back @k@
-- blocks. For example, if we are on line (*), and roll back 6 blocks, we get
--
-- > L3 |> []
data LedgerDB l r = LedgerDB {
      -- | The ledger state at the tip of the chain
      ledgerDbCurrent   :: !l

      -- | Older ledger states
    , ledgerDbSnapshots :: !(StrictSeq (Checkpoint l r))

      -- | Information about the state of the ledger /before/
    , ledgerDbAnchor    :: !(ChainSummary l r)

      -- | Ledger DB parameters
    , ledgerDbParams    :: !LedgerDbParams
    }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

newtype LedgerDbParams = LedgerDbParams {
    -- | Security parameter (maximum rollback)
    ledgerDbSecurityParam :: SecurityParam
    }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

-- | Default parameters
ledgerDbDefaultParams :: SecurityParam -> LedgerDbParams
ledgerDbDefaultParams (SecurityParam k) = LedgerDbParams {
    ledgerDbSecurityParam = SecurityParam k
    }

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

-- | Ticking the ledger DB just ticks the current state
--
-- We don't push the new state into the DB until we apply a block.
data instance Ticked (LedgerDB l r) = TickedLedgerDB {
      tickedLedgerDbTicked :: Ticked l
    , tickedLedgerDbOrig   :: LedgerDB l r
    }

{-------------------------------------------------------------------------------
  Internal: checkpoints
-------------------------------------------------------------------------------}

-- | Checkpoint with a ledger state
data Checkpoint l r = CpSShot !r !l
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

cpToPair :: Checkpoint l r -> (r, l)
cpToPair (CpSShot r l) = (r, l)

cpBlock :: Checkpoint l r -> r
cpBlock (CpSShot r _) = r

{-------------------------------------------------------------------------------
  Chain summary
-------------------------------------------------------------------------------}

-- | Summary of the chain at a particular point in time
data ChainSummary l r = ChainSummary {
      -- | The tip of the chain
      csTip    :: !(WithOrigin r)

      -- | Length of the chain
    , csLength :: !Word64

      -- | Ledger state
    , csLedger :: !l
    }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

genesisChainSummary :: l -> ChainSummary l r
genesisChainSummary l = ChainSummary Origin 0 l

{-------------------------------------------------------------------------------
  LedgerDB proper
-------------------------------------------------------------------------------}

-- | Ledger DB starting at the specified ledger state
ledgerDbWithAnchor :: LedgerDbParams -> ChainSummary l r -> LedgerDB l r
ledgerDbWithAnchor params anchor = LedgerDB {
      ledgerDbCurrent   = csLedger anchor
    , ledgerDbSnapshots = Seq.empty
    , ledgerDbAnchor    = anchor
    , ledgerDbParams    = params
    }

ledgerDbFromGenesis :: LedgerDbParams -> l -> LedgerDB l r
ledgerDbFromGenesis params = ledgerDbWithAnchor params . genesisChainSummary

{-------------------------------------------------------------------------------
  Compute signature

  Depending on the parameters (apply by value or by reference, previously
  applied or not) we get different signatures.
-------------------------------------------------------------------------------}

-- | Resolve a block
--
-- Resolving a block reference to the actual block lives in @m@ because
-- it might need to read the block from disk (and can therefore not be
-- done inside an STM transaction).
--
-- NOTE: The ledger DB will only ask the 'ChainDB' for blocks it knows
-- must exist. If the 'ChainDB' is unable to fulfill the request, data
-- corruption must have happened and the 'ChainDB' should trigger
-- validation mode.
type ResolveBlock m r b = r -> m b

-- | Annotated ledger errors
data AnnLedgerError l r = AnnLedgerError {
      -- | The ledger DB just /before/ this block was applied
      annLedgerState  :: LedgerDB l r

      -- | Reference to the block that had the error
    , annLedgerErrRef :: r

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

-- | Monads in which we can resolve blocks
--
-- To guide type inference, we insist that we must be able to infer the type
-- of the block we are resolving from the type of the monad.
class Monad m => ResolvesBlocks r b m | m -> b where
  resolveBlock :: r -> m b

instance Monad m => ResolvesBlocks r b (ReaderT (ResolveBlock m r b) m) where
  resolveBlock r = ReaderT $ \f -> f r

defaultResolveBlocks :: ResolveBlock m r b
                     -> ReaderT (ResolveBlock m r b) m a
                     -> m a
defaultResolveBlocks = flip runReaderT

-- Quite a specific instance so we can satisfy the fundep
instance Monad m
      => ResolvesBlocks r b (ExceptT e (ReaderT (ResolveBlock m r b) m)) where
  resolveBlock = lift . resolveBlock

class Monad m => ThrowsLedgerError l r m where
  throwLedgerError :: LedgerDB l r -> r -> LedgerErr l -> m a

defaultThrowLedgerErrors :: ExceptT (AnnLedgerError l r) m a
                         -> m (Either (AnnLedgerError l r) a)
defaultThrowLedgerErrors = runExceptT

defaultResolveWithErrors :: ResolveBlock m r b
                         -> ExceptT (AnnLedgerError l r)
                                    (ReaderT (ResolveBlock m r b) m)
                                    a
                         -> m (Either (AnnLedgerError l r) a)
defaultResolveWithErrors resolve =
      defaultResolveBlocks resolve
    . defaultThrowLedgerErrors

instance Monad m => ThrowsLedgerError l r (ExceptT (AnnLedgerError l r) m) where
  throwLedgerError l r e = throwError $ AnnLedgerError l r e

-- | 'Ap' is used to pass information about blocks to ledger DB updates
--
-- The constructors serve two purposes:
--
-- * Specify the various parameters
--   a. Are we passing the block by value or by reference?
--   b. Are we applying or reapplying the block?
--
-- * Compute the constraint @c@ on the monad @m@ in order to run the query:
--   a. If we are passing a block by reference, we must be able to resolve it.
--   b. If we are applying rather than reapplying, we might have ledger errors.
data Ap :: (* -> *) -> * -> * -> * -> Constraint -> * where
  ReapplyVal :: r -> b -> Ap m l r b ()
  ApplyVal   :: r -> b -> Ap m l r b (                       ThrowsLedgerError l r m)
  ReapplyRef :: r      -> Ap m l r b (ResolvesBlocks  r b m)
  ApplyRef   :: r      -> Ap m l r b (ResolvesBlocks  r b m, ThrowsLedgerError l r m)

  -- | 'Weaken' increases the constraint on the monad @m@.
  --
  -- This is primarily useful when combining multiple 'Ap's in a single
  -- homogeneous structure.
  Weaken :: (c' => c) => Ap m l r b c -> Ap m l r b c'

{-------------------------------------------------------------------------------
  Internal utilities for 'Ap'
-------------------------------------------------------------------------------}

apRef :: Ap m l r b c -> r
apRef (ReapplyVal r _) = r
apRef (ApplyVal   r _) = r
apRef (ReapplyRef r  ) = r
apRef (ApplyRef   r  ) = r
apRef (Weaken     ap)  = apRef ap

-- | Apply block to the current ledger state
--
-- We take in the entire 'LedgerDB' because we record that as part of errors.
applyBlock :: forall m c l r b. (ApplyBlock l b, Monad m, c)
           => FullBlockConfig l b
           -> Ap m l r b c
           -> LedgerDB l r -> m l
applyBlock cfg ap db = case ap of
    ReapplyVal _r b ->
      return $
        tickThenReapply cfg b l
    ApplyVal r b ->
      either (throwLedgerError db r) return $ runExcept $
        tickThenApply cfg b l
    ReapplyRef r  -> do
      b <- resolveBlock r
      return $
        tickThenReapply cfg b l
    ApplyRef r -> do
      b <- resolveBlock r
      either (throwLedgerError db r) return $ runExcept $
        tickThenApply cfg b l
    Weaken ap' ->
      applyBlock cfg ap' db
  where
    l :: l
    l = ledgerDbCurrent db

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Total length of the chain (in terms of number of blocks)
ledgerDbChainLength :: LedgerDB l r -> Word64
ledgerDbChainLength LedgerDB{..} =
    csLength ledgerDbAnchor + fromIntegral (Seq.length ledgerDbSnapshots)

-- | References to blocks and corresponding ledger state (from old to new)
ledgerDbToList :: LedgerDB l r -> [(r, l)]
ledgerDbToList LedgerDB{..} = map cpToPair $ toList ledgerDbSnapshots

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also include the anchor. For each snapshot we also return the distance
-- from the tip.
ledgerDbOldStates :: forall l r. LedgerDB l r -> [(Word64, l)]
ledgerDbOldStates LedgerDB{..} = go 0 ledgerDbSnapshots
  where
    go :: Word64 -> StrictSeq (Checkpoint l r) -> [(Word64, l)]
    go !offset Empty                = [(offset, csLedger ledgerDbAnchor)]
    go !offset (ss :|> CpSShot _ l) = (offset, l) : go (offset + 1) ss

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: LedgerDB l r -> Word64
ledgerDbMaxRollback LedgerDB{..} = fromIntegral (Seq.length ledgerDbSnapshots)

-- | Reference to the block at the tip of the chain
ledgerDbTip :: LedgerDB l r -> WithOrigin r
ledgerDbTip LedgerDB{..} =
    case ledgerDbSnapshots of
      Empty    -> csTip ledgerDbAnchor
      _ :|> cp -> NotOrigin (cpBlock cp)

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: LedgerDB l r -> Bool
ledgerDbIsSaturated LedgerDB{..} =
    fromIntegral (Seq.length ledgerDbSnapshots) >= k
  where
    LedgerDbParams{..} = ledgerDbParams
    SecurityParam k    = ledgerDbSecurityParam

{-------------------------------------------------------------------------------
  Internal updates
-------------------------------------------------------------------------------}

-- | Internal: shift the anchor given a bunch of checkpoints.
shiftAnchor :: forall r l. HasCallStack
            => StrictSeq (Checkpoint l r) -> ChainSummary l r -> ChainSummary l r
shiftAnchor toRemove ChainSummary{..} = ChainSummary {
      csTip    = NotOrigin csTip'
    , csLength = csLength + fromIntegral (Seq.length toRemove)
    , csLedger = csLedger'
    }
  where
    csTip'    :: r
    csLedger' :: l
    (csTip', csLedger') =
        case toRemove of
          Empty             -> error "shiftAnchor: empty list"
          _ :|> CpSShot r l -> (r, l)

-- | Internal: count number of blocks to prune, given total number of blocks
--
-- This is exposed for the benefit of tests only.
ledgerDbCountToPrune :: LedgerDbParams -> Int -> Int
ledgerDbCountToPrune LedgerDbParams{..} curSize'
  | curSize <= k = 0
  | otherwise    = fromIntegral $ curSize - k
  where
    SecurityParam k = ledgerDbSecurityParam
    curSize = fromIntegral curSize'

-- | Internal: drop unneeded snapshots from the head of the list
prune :: HasCallStack => LedgerDB l r -> LedgerDB l r
prune db@LedgerDB{..} =
    if toPrune == 0
      then db
      else let (removed, kept) = Seq.splitAt toPrune ledgerDbSnapshots
               anchor'         = shiftAnchor removed ledgerDbAnchor
           in db { ledgerDbAnchor    = anchor'
                 , ledgerDbSnapshots = kept
                 }
  where
    -- Number of blocks to remove (assuming curSize > maxSize)
    toPrune :: Int
    toPrune = ledgerDbCountToPrune ledgerDbParams (Seq.length ledgerDbSnapshots)

-- | Push an updated ledger state
pushLedgerState :: l  -- ^ Updated ledger state
                -> r  -- ^ Reference to the applied block
                -> LedgerDB l r -> LedgerDB l r
pushLedgerState current' ref db@LedgerDB{..}  = prune $ db {
      ledgerDbCurrent   = current'
    , ledgerDbSnapshots = snapshots
    }
  where
    LedgerDbParams{..} = ledgerDbParams

    snapshots = ledgerDbSnapshots |> CpSShot ref current'

{-------------------------------------------------------------------------------
  Internal: rolling back
-------------------------------------------------------------------------------}

-- | Reconstruct ledger DB from a list of checkpoints
reconstructFrom :: forall l r.
                   LedgerDbParams
                -> ChainSummary l r
                -> StrictSeq (Checkpoint l r)
                -> LedgerDB l r
reconstructFrom params anchor snapshots =
    LedgerDB {
        ledgerDbCurrent   = current
      , ledgerDbSnapshots = snapshots
      , ledgerDbParams    = params
      , ledgerDbAnchor    = anchor
      }
  where
    current = case snapshots of
      Empty              -> csLedger anchor
      _  :|> CpSShot _ l -> l

-- | Generalization of rollback using a function on the checkpoints
rollbackTo :: (   ChainSummary l r
               -> StrictSeq (Checkpoint l r)
               -> Maybe (StrictSeq (Checkpoint l r))
              )
           -> LedgerDB l r
           -> Maybe (LedgerDB l r)
rollbackTo f (LedgerDB _current blocks anchor params) =
    reconstructFrom params anchor <$> f anchor blocks

-- | Rollback
--
-- Returns 'Nothing' if maximum rollback is exceeded.
rollback :: forall l r.
            Word64
         -> LedgerDB l r
         -> Maybe (LedgerDB l r)
rollback 0 db = Just db
rollback n db = rollbackTo (\_anchor -> go) db
  where
    go :: StrictSeq (Checkpoint l r) -> Maybe (StrictSeq (Checkpoint l r))
    go blocks =
        if Seq.length blocks >= fromIntegral n
          then Just $ Seq.take (Seq.length blocks - fromIntegral n) blocks
          else Nothing

{-------------------------------------------------------------------------------
  Get past ledger states
-------------------------------------------------------------------------------}

-- | Get past ledger state
ledgerDbPast :: forall l r.
                Eq r
             => WithOrigin r
             -> LedgerDB l r
             -> Maybe l
ledgerDbPast tip db
  | ledgerDbTip db == tip = Just (ledgerDbCurrent db)
  | otherwise             = ledgerDbCurrent <$> rollbackTo go db
  where
    go :: ChainSummary l r
       -> StrictSeq (Checkpoint l r)
       -> Maybe (StrictSeq (Checkpoint l r))
    go anchor blocks =
        case blocks' of
          Empty | csTip anchor /= tip -> Nothing
          _otherwise                  -> Just blocks'
      where
        blocks' :: StrictSeq (Checkpoint l r)
        blocks' = Seq.dropWhileR (\cp -> NotOrigin (cpBlock cp) /= tip) blocks

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

-- | Exceeded maximum rollback supported by the current ledger DB state
--
-- Under normal circumstances this will not arise. It can really only happen
-- in the presence of data corruption (or when switching to a shorter fork,
-- but that is disallowed by all currently known Ouroboros protocols).
--
-- Records both the supported and the requested rollback.
data ExceededRollback = ExceededRollback {
      rollbackMaximum   :: Word64
    , rollbackRequested :: Word64
    }

ledgerDbPush :: forall m c l r b. (ApplyBlock l b, Monad m, c)
             => FullBlockConfig l b
             -> Ap m l r b c -> LedgerDB l r -> m (LedgerDB l r)
ledgerDbPush cfg ap db =
    (\current' -> pushLedgerState current' (apRef ap) db) <$>
      applyBlock cfg ap db

-- | Push a bunch of blocks (oldest first)
ledgerDbPushMany :: (ApplyBlock l b, Monad m, c)
                 => FullBlockConfig l b
                 -> [Ap m l r b c] -> LedgerDB l r -> m (LedgerDB l r)
ledgerDbPushMany = repeatedlyM . ledgerDbPush

-- | Switch to a fork
ledgerDbSwitch :: (ApplyBlock l b, ResolvesBlocks r b m, c)
               => FullBlockConfig l b
               -> Word64          -- ^ How many blocks to roll back
               -> [Ap m l r b c]  -- ^ New blocks to apply
               -> LedgerDB l r
               -> m (Either ExceededRollback (LedgerDB l r))
ledgerDbSwitch cfg numRollbacks newBlocks db = do
    let mRolledBack = rollback numRollbacks db
    case mRolledBack of
      Nothing ->
        return $ Left $ ExceededRollback {
            rollbackMaximum   = ledgerDbMaxRollback db
          , rollbackRequested = numRollbacks
          }
      Just db' ->
        Right <$> ledgerDbPushMany cfg newBlocks db'

{-------------------------------------------------------------------------------
  The LedgerDB itself behaves like a ledger
-------------------------------------------------------------------------------}

type instance LedgerCfg (LedgerDB l r) = LedgerCfg l

type instance HeaderHash (LedgerDB l r) = HeaderHash l

instance IsLedger l => GetTip (LedgerDB l r) where
  getTip = castPoint . getTip . ledgerDbCurrent

instance IsLedger l => GetTip (Ticked (LedgerDB l r)) where
  getTip = castPoint . getTip . tickedLedgerDbOrig

instance ( IsLedger l
           -- Required superclass constraints of 'IsLedger'
         , Show               r
         , Eq                 r
         , NoUnexpectedThunks r
         ) => IsLedger (LedgerDB l r) where
  type LedgerErr (LedgerDB l r) = LedgerErr l

  applyChainTick cfg slot db@LedgerDB{..} = TickedLedgerDB {
        tickedLedgerDbTicked = applyChainTick cfg slot ledgerDbCurrent
      , tickedLedgerDbOrig   = db
      }

instance ApplyBlock l blk => ApplyBlock (LedgerDB l (RealPoint blk)) blk where
  applyLedgerBlock cfg blk TickedLedgerDB{..} =
      push <$> applyLedgerBlock
                 (castFullBlockConfig cfg)
                 blk
                 tickedLedgerDbTicked
   where
     push :: l -> LedgerDB l (RealPoint blk)
     push l = pushLedgerState l (blockRealPoint blk) tickedLedgerDbOrig

  reapplyLedgerBlock cfg blk TickedLedgerDB{..} =
      push $ reapplyLedgerBlock
               (castFullBlockConfig cfg)
               blk
               tickedLedgerDbTicked
   where
     push :: l -> LedgerDB l (RealPoint blk)
     push l = pushLedgerState l (blockRealPoint blk) tickedLedgerDbOrig

{-------------------------------------------------------------------------------
  Suppor for testing
-------------------------------------------------------------------------------}

pureBlock :: b -> Ap m l b b ()
pureBlock b = ReapplyVal b b

triviallyResolve :: forall b a. Proxy b
                 -> Reader (ResolveBlock Identity b b) a -> a
triviallyResolve _ = runIdentity . defaultResolveBlocks return

ledgerDbPush' :: ApplyBlock l b
              => FullBlockConfig l b -> b -> LedgerDB l b -> LedgerDB l b
ledgerDbPush' cfg b = runIdentity . ledgerDbPush cfg (pureBlock b)

ledgerDbPushMany' :: ApplyBlock l b
                  => FullBlockConfig l b -> [b] -> LedgerDB l b -> LedgerDB l b
ledgerDbPushMany' cfg bs = runIdentity . ledgerDbPushMany cfg (map pureBlock bs)

ledgerDbSwitch' :: forall l b. ApplyBlock l b
                => FullBlockConfig l b
                -> Word64 -> [b] -> LedgerDB l b -> Maybe (LedgerDB l b)
ledgerDbSwitch' cfg n bs db =
    case triviallyResolve (Proxy @b) $
           ledgerDbSwitch cfg n (map pureBlock bs) db of
      Left  ExceededRollback{} -> Nothing
      Right db'                -> Just db'

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance (Serialise l, Serialise r) => Serialise (ChainSummary l r) where
  encode = encodeChainSummary encode encode
  decode = decodeChainSummary decode decode

encodeChainSummary :: (l -> Encoding)
                   -> (r -> Encoding)
                   -> ChainSummary l r -> Encoding
encodeChainSummary encodeLedger encodeRef ChainSummary{..} = mconcat [
      Enc.encodeListLen 3
    , encodeWithOrigin encodeRef csTip
    , Enc.encodeWord64 csLength
    , encodeLedger csLedger
    ]

decodeChainSummary :: (forall s. Decoder s l)
                   -> (forall s. Decoder s r)
                   -> forall s. Decoder s (ChainSummary l r)
decodeChainSummary decodeLedger decodeRef = do
    Dec.decodeListLenOf 3
    csTip    <- decodeWithOrigin decodeRef
    csLength <- Dec.decodeWord64
    csLedger <- decodeLedger
    return ChainSummary{..}
