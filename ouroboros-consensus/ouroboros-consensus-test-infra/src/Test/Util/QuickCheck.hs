{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | QuickCheck utilities
module Test.Util.QuickCheck (
    -- * Comparison functions
    lt
  , le
  , gt
  , ge
  , strictlyIncreasing
  , expectRight
    -- * Improved variants
  , elements
  , (=:=)
    -- * SOP
  , cshrinkNP
  , shrinkNP
    -- * Convenience
  , collects
  ) where

import           Data.Proxy
import           Data.SOP.Strict
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.Condense (Condense, condense)
import           Ouroboros.Consensus.Util.SOP

import           Test.QuickCheck hiding (elements)
import qualified Test.QuickCheck as QC

{-------------------------------------------------------------------------------
  Comparison functions
-------------------------------------------------------------------------------}

infix 4 `lt`
infix 4 `le`
infix 4 `gt`
infix 4 `ge`

-- | Like '<', but prints a counterexample when it fails.
lt :: (Ord a, Show a) => a -> a -> Property
x `lt` y = counterexample (show x ++ " >= " ++ show y) $ x < y

-- | Like '<=', but prints a counterexample when it fails.
le :: (Ord a, Show a) => a -> a -> Property
x `le` y = counterexample (show x ++ " > " ++ show y) $ x <= y

-- | Like '>', but prints a counterexample when it fails.
gt :: (Ord a, Show a) => a -> a -> Property
x `gt` y = counterexample (show x ++ " <= " ++ show y) $ x > y

-- | Like '>=', but prints a counterexample when it fails.
ge :: (Ord a, Show a) => a -> a -> Property
x `ge` y = counterexample (show x ++ " < " ++ show y) $ x >= y

strictlyIncreasing :: forall a. (Show a, Ord a) => [a] -> Property
strictlyIncreasing xs =
    counterexample (show xs) $ go xs
  where
    go :: [a] -> Property
    go []       = property True
    go [_]      = property True
    go (x:y:zs) = x `lt` y .&&. go (y:zs)

-- | Check that we have the expected 'Right' value
--
-- @expectRight b ab@ is roughly equivalent to @Right b === ab@, but avoids an
-- equality constraint on @a@.
expectRight :: (Show a, Show b, Eq b) => b -> Either a b -> Property
expectRight b (Right b') = b === b'
expectRight _ (Left a)   = counterexample ("Unexpected left " ++ show a) $
                             False

{-------------------------------------------------------------------------------
  Improved variants
-------------------------------------------------------------------------------}

-- | Generates one of the given values. The input list must be non-empty.
--
-- NOTE unlike the standard @elements@, this variant has a 'HasCallStack'
-- constraint, which makes debugging the 'error' much easier.
elements :: HasCallStack => [a] -> Gen a
elements [] = error "Test.Util.QuickCheck.elements used with empty list"
elements xs = QC.elements xs

-- | Like '===', but uses 'Condense' instead of 'Show' when it fails.
infix 4 =:=
(=:=) :: (Eq a, Condense a) => a -> a -> Property
x =:= y =
    counterexample (condense x ++ interpret res ++ condense y) res
  where
    res = x == y
    interpret True  = " == "
    interpret False = " /= "

{-------------------------------------------------------------------------------
  SOP
-------------------------------------------------------------------------------}

cshrinkNP :: forall proxy c f g xs.
             All c xs
          => proxy c
          -> (forall a. c a => f a -> g a)    -- For elements we don't shrink
          -> (forall a. c a => f a -> [g a])
          -> NP f xs
          -> [NP g xs]
cshrinkNP p g f = go
  where
    go :: All c xs' => NP f xs' -> [NP g xs']
    go Nil       = [] -- Can't shrink the empty list
    go (x :* xs) = concat [
          -- Shrink the head of the list
          [ x' :* hcmap p g xs | x' <- f x ]

          -- Or shrink the tail of the list
        , [ g x :* xs' | xs' <- go xs ]
        ]

shrinkNP :: (forall a. f a -> g a)    -- For elements we don't shrink
         -> (forall a. f a -> [g a])
         -> NP f xs
         -> [NP g xs]
shrinkNP g f np = npToSListI np $ cshrinkNP (Proxy @Top) g f np

{-------------------------------------------------------------------------------
  Convenience
-------------------------------------------------------------------------------}

collects :: Show a => [a] -> Property -> Property
collects = repeatedly collect
