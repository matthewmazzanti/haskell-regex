{-# OPTIONS_GHC -W #-}
{-# LANGUAGE LambdaCase, BlockArguments, RecursiveDo #-}

module Data.NFA where

import Control.Monad.State
import qualified Data.Set as Set
import Data.Regex (Regex (..))
import Data.Maybe

data NFA
    = Split Int NFA NFA
    | Step Int Char NFA
    | Final Int
    deriving Show

instance Eq NFA where
  x == y = ident x == ident y

instance Ord NFA where
  compare x y = compare (ident x) (ident y)


-- Get the identifier of this NFA node
ident :: NFA -> Int
ident (Split i _ _) = i
ident (Step i _ _) = i
ident (Final i) = i


fromRegex :: Regex -> NFA
fromRegex re = let (nfa, (i, _)) = runState (conv re) (0, Final i) in nfa
  where
    -- Produce a unique id
    inc :: State (Int, NFA) Int
    inc = state \(i, nfa) -> (i, (i + 1, nfa))

    -- Get the next node
    getNext :: State (Int, NFA) NFA
    getNext = get >>= pure . snd

    -- Set the next node
    putNext :: NFA -> State (Int, NFA) ()
    putNext nfa = modify \(i, _) -> (i, nfa)

    -- Helper function to build the Star and Plus graph nodes. These are
    -- constructed in the same way, the difference being that Star starts at the
    -- the split, and Plus starts at the NFA. Return both, and differentiate in
    -- conv.
    mkStar :: Regex -> State (Int, NFA) (NFA, NFA)
    mkStar re = mdo
        split <- Split <$> inc <*> getNext <*> pure nfa
        nfa   <- putNext split >> conv re
        pure (split, nfa)

    conv :: Regex -> State (Int, NFA) NFA
    conv (Empty)     = getNext
    conv (Lit c)     = Step <$> inc <*> pure c <*> getNext
    conv (Or re re') = Split <$> inc <*> conv re <*> conv re'
    conv (Mark re)   = Split <$> inc <*> conv re <*> getNext
    conv (Star re)   = mkStar re >>= pure . fst
    conv (Plus re)   = mkStar re >>= pure . snd
    -- The And case is a bit tricky: in order to get the "intuitive" order of
    -- the ids, where the left regex has a smaller id than the right, we build
    -- the right node lazily, and pass it to the left.
    conv (And re re') = mdo
        next  <- getNext
        left  <- putNext right >> conv re -- Left conversion
        right <- putNext next >> conv re' -- Right conversion
        pure left

type Closure = Set.Set NFA


closure :: NFA -> Closure
closure = execClosure . closureST


execClosure :: State Closure a -> Closure
execClosure = flip execState Set.empty


closureST :: NFA -> State Closure ()
closureST nfa = do
    nfas <- get
    if Set.member nfa nfas then pure () else do
        modify $ Set.insert nfa
        recurse nfa
  where
    recurse (Split _ l r) = closureST l >> closureST r
    recurse _ = pure ()


step :: Char -> Closure -> Closure
step sym set = execClosure $ traverse closureST $ stepAll set
  where
    stepAll :: Closure -> [NFA]
    stepAll = catMaybes . (stepOne sym <$>) . Set.toList

    stepOne :: Char -> NFA -> Maybe NFA
    stepOne sym (Step _ sym' nfa)
      | sym == sym' = Just nfa
      | otherwise   = Nothing
    stepOne _ _ = Nothing


run :: String -> NFA -> Closure
run syms nfa = go syms $ closure nfa
  where
    go :: String -> Closure -> Closure
    go [] set         = set
    go (sym:syms) set = go syms $ step sym set


hasFinal :: Closure -> Bool
hasFinal set = go $ Set.toList set
  where
    go :: [NFA] -> Bool
    go [] = False
    go (Final _:_) = True
    go (_:nfas) = go nfas


match :: NFA -> String -> Bool
match nfa syms = hasFinal $ run syms nfa


matchRe :: Regex -> String -> Bool
matchRe = match . fromRegex
