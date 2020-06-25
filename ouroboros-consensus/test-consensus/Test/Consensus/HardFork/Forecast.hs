{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wwarn #-}

module Test.Consensus.HardFork.Forecast (tests) where

import           Control.Monad.State
import           Data.Functor.Product
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.SOP.Strict hiding (Shape (..))
import           Data.Word

import           Cardano.Slotting.Slot

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.HardFork.History.Util
import           Ouroboros.Consensus.Util (Some (..), (.:))
import           Ouroboros.Consensus.Util.SOP

import           Test.Consensus.HardFork.Infra
import           Test.Util.QuickCheck

tests :: TestTree
tests = testGroup "Forecast" [
      testGroup "Sanity" [
          testProperty "Generator" $ checkGenerator prop_validTestSetup
        , testProperty "Shrinker"  $ checkShrinker  prop_validTestSetup
        ]
    ]

{-
mkHardForkForecast :: InPairs (Translate f) xs
                   -> Telescope (Past g) (Current (AnnForecast f)) xs
                   -> Forecast (HardForkLedgerView_ f xs)
-}

{-------------------------------------------------------------------------------
  Mock chain and ledger
-------------------------------------------------------------------------------}


{-
data LedgerView = LedgerView {
      value     :: Int
    , scheduled :: Scheduled
    }


newtype LedgerState = LedgerState {
      getLedgerView :: LedgerView
    }
-}

newtype Chain era = Chain { getBlocks :: [Block] }
  deriving (Show)

type Block       = (SlotNo, Scheduled)
type Scheduled   = Map SlotNo LedgerValue
type LedgerValue = Int

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data TestSetup xs = SListI xs => TestSetup {
      testShape :: NP MaxLookahead xs
    , testChain :: NP Chain xs
    }

newtype MaxLookahead era = MaxLookahead { getMaxLookahead :: Word64 }

allBlocks :: NP Chain xs -> [[Block]]
allBlocks np = npToSListI np $ hcollapse $ hmap (K . getBlocks) np

flatChain :: TestSetup xs -> [Block]
flatChain = concat . allBlocks . testChain

{-------------------------------------------------------------------------------
  Sanity checks
-------------------------------------------------------------------------------}

prop_validTestSetup :: Some TestSetup -> Property
prop_validTestSetup (Some setup@TestSetup{..}) =
    npToSListI testShape $ conjoin [
        counterexample "strictlyIncreasing" $
          strictlyIncreasing $ map fst (flatChain setup)
      , counterexample "obeysMaxLookahead" $
          conjoin $ hcollapse $
            hzipWith (K .: checkLookahead) testShape testChain
      ]
  where
    checkLookahead :: MaxLookahead era -> Chain era -> Property
    checkLookahead (MaxLookahead maxLookahead) (Chain bs) = conjoin [
          slotChange `ge` addSlots maxLookahead slotBlock
        | (slotBlock, scheduled) <- bs
        , (slotChange, _newValue) <- Map.toList scheduled
        ]

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

instance Arbitrary (Some TestSetup) where
  arbitrary = chooseEras $ \(Eras eras) -> do
      testShape <- hsequence' $ hmap (Comp . genMaxLookahead) eras
      testChain <- flip evalStateT 0 $
                      hsequence' $ hmap (Comp . genChain) testShape
      return $ Some TestSetup{..}
    where
      genMaxLookahead :: K Era era -> Gen (MaxLookahead era)
      genMaxLookahead _era = MaxLookahead <$> choose (0, 10)

      genChain :: MaxLookahead era -> StateT SlotNo Gen (Chain era)
      genChain maxLookahead = do
          numBlocks <- lift $ choose (0, 50)
          Chain <$> replicateM numBlocks (genBlock maxLookahead)

      genBlock :: MaxLookahead era -> StateT SlotNo Gen Block
      genBlock maxLookahead = do
          slot      <- genBlockSlot
          scheduled <- lift $ genScheduled maxLookahead slot
          return (slot, scheduled)

      genBlockSlot :: StateT SlotNo Gen SlotNo
      genBlockSlot = do
          skip <- lift $ choose (0, 5)
          state $ \nextSlot ->
            let slotBlock = addSlots skip nextSlot
            in (slotBlock, succ slotBlock)

      genScheduled :: MaxLookahead era -> SlotNo -> Gen Scheduled
      genScheduled maxLookahead slotBlock = do
          numChanges <- choose (0, 2)
          fmap Map.fromList $
            replicateM numChanges $ genChange maxLookahead slotBlock

      genChange :: MaxLookahead era -> SlotNo -> Gen (SlotNo, LedgerValue)
      genChange (MaxLookahead maxLookahead) slotBlock = do
          skip     <- choose (0, 10)
          newValue <- arbitrary
          -- If the maxLookahead is zero (no look ahead possible), the change
          -- is applied when we apply the block (i.e., in the same slot).
          let slotChange = addSlots (maxLookahead + skip) slotBlock
          return (slotChange, newValue)

  shrink (Some setup@TestSetup{..}) = map Some $ concat [
        [ setup { testChain = chain' }
        | chain' <- shrinkNP (\(Pair _ bs) -> bs) shrinkBlocks
                      (hzipWith Pair testShape testChain)
        ]
      ]
    where
      shrinkBlocks :: Product MaxLookahead Chain era -> [Chain era]
      shrinkBlocks (Pair maxLookahead (Chain bs)) = Chain <$>
          shrinkList (shrinkBlock maxLookahead) bs

      shrinkBlock :: MaxLookahead era -> Block -> [Block]
      shrinkBlock (MaxLookahead maxLookahead) (slotBlock, scheduled) =
          undefined

{-------------------------------------------------------------------------------
  Show instances
-------------------------------------------------------------------------------}

instance {-# OVERLAPPING #-} Show (NP Chain xs) where
  show = show . allBlocks

instance {-# OVERLAPPING #-} Show (NP MaxLookahead xs) where
  show np =
      npToSListI np $
        show . hcollapse . hmap (K . getMaxLookahead) $ np

deriving instance Show (MaxLookahead era)
deriving instance Show (TestSetup xs)
deriving instance Show (Some TestSetup)
