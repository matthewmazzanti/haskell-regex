{-# OPTIONS_GHC -W #-}

import Criterion.Main
import Data.Regex (Regex(..))
import qualified Data.Regex as Regex
import qualified Data.DFA as DFA
import qualified Data.NFA as NFA

main :: IO ()
main = defaultMain
    [ bgroup "regex" $ (runMatch Regex.match) <$> nats
    , bgroup "nfa" $ (runMatch NFA.matchRe) <$> nats
    , bgroup "dfa" $ (runMatch DFA.matchRe) <$> nats
    ]
  where
    re :: Int -> Regex
    re n = foldr1 And (replicate n mark <> replicate n lit)
      where mark = Mark $ Lit 'a'
            lit = Lit 'a'

    nats :: [Int]
    nats = take 10 $ iterate (+1) 1

    runMatch :: (Regex -> String -> Bool) -> Int -> Benchmark
    runMatch match size = bench (show size) $ whnf match' str
      where str = replicate size 'a'
            match' = match $ re size

