{-# OPTIONS_GHC -W #-}
{-# LANGUAGE LambdaCase, BlockArguments, RecursiveDo #-}

module Data.Regex where

import Data.List (find)
import Data.Maybe (isJust)

data Regex
    = Empty
    | Lit Char
    | And Regex Regex
    | Or Regex Regex
    | Mark Regex
    | Plus Regex
    | Star Regex
    deriving Show

matchStream :: Regex -> String -> [String]
matchStream (Empty) syms        = [syms]
matchStream (Lit _) []          = []
matchStream (Lit c) (sym:syms)  = if c == sym then [syms] else []
matchStream (And re re') syms   = matchStream re syms >>= matchStream re'
matchStream (Or re re')  syms   = matchStream re syms <> matchStream re' syms
matchStream (Mark re)    syms   = matchStream re syms <> [syms]
matchStream (Plus re)    syms   = matchStream re syms >>= matchStream (Star re)
matchStream star@(Star re) syms = (<> [syms]) do
    syms' <- matchStream re syms
    if syms' /= syms then matchStream star syms' else []

match :: Regex -> String -> Bool
match re syms = isJust $ find (== []) $ matchStream re syms
