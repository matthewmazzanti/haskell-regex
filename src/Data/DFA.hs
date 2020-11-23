{-# OPTIONS_GHC -W #-}
{-# LANGUAGE LambdaCase, BlockArguments, RecursiveDo #-}

module Data.DFA where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Regex (Regex)
import Data.NFA (NFA(..), Closure)
import qualified Data.NFA as NFA
import Data.Maybe (catMaybes)
import Control.Monad.State

data DFA = DFA Bool Edges deriving Show
type Edges = Map.Map Char DFA
type Cache = Map.Map Closure DFA


hasFinal :: DFA -> Bool
hasFinal (DFA final _) = final


fromNFA :: NFA -> DFA
fromNFA nfa = evalState (conv $ NFA.closure nfa) Map.empty
  where
    -- Convert an NFA to a DFA within a state cache. The cache allows us to
    -- create circular references to the next DFA by keeping references to the
    -- other DFAs as the graph is being constructed.
    conv :: Closure -> State Cache DFA
    conv set = do
        cache <- get
        case Map.lookup set cache of
            -- DFA fragment previously computed, simply return.
            Just dfa -> pure dfa
            -- DFA fragment not computed. Build the DFA, and add to cache.
            Nothing -> build set

    -- Get the edges from the NFAs for each available symbol. See stepAll.
    -- Traverse is used to map the resulting edges of closures to edges of DFAs,
    -- and runs in the state cache.
    build :: Closure -> State Cache DFA
    build set = mdo
        let dfa = DFA (NFA.hasFinal set) edges
        modify $ Map.insert set dfa

        edges <- traverse conv $ stepAll set

        pure dfa

    -- For the given closure, find all outward transitions to the next closures
    stepAll :: Closure -> Map.Map Char Closure
    stepAll set = Map.fromList $ symClosure set <$> symsOf set
      where
        -- All of the unique symbols within this closure. Done to avoid having
        -- to guess all available characters.
        symsOf :: Closure -> String
        symsOf = catMaybes . fmap symOf . Set.toList
          where
            symOf :: NFA -> Maybe Char
            symOf (Step _ sym _) = Just sym
            symOf _ = Nothing

        -- Where the work of creating the edges is done. For a given symbol and
        -- a closure, we calculate the closure of stepping with that symbol
        symClosure :: Closure -> Char -> (Char, Closure)
        symClosure set sym = (sym, NFA.step sym set)


fromRegex :: Regex -> DFA
fromRegex = fromNFA . NFA.fromRegex


run :: String -> DFA -> Maybe DFA
run [] dfa = Just dfa
run (sym:syms) (DFA _ edges) = Map.lookup sym edges >>= run syms


match :: DFA -> String -> Bool
match dfa syms = case run syms dfa of
    Just dfa -> hasFinal dfa
    Nothing  -> False


matchRe :: Regex -> String -> Bool
matchRe = match . fromRegex
