{-# OPTIONS_GHC -W #-}

import Data.Regex (Regex(..))
import qualified Data.Regex as Regex
import qualified Data.DFA as DFA
import qualified Data.NFA as NFA
import Data.Foldable (traverse_)
import Test.QuickCheck


instance Arbitrary Regex where
  arbitrary = sized (genRegex 1)
    where
      genRegex depth size = frequency
          [ (depth, Lit <$> arbitraryASCIIChar)
          , (depth, pure Empty)
          , (size,  genTwo And)
          , (size,  genTwo Or)
          , (size,  genOne Mark) , (size,  genOne Star)
          , (size,  genOne Plus)
          ]
        where genRegex' = genRegex (depth * 2) size
              genOne fn = fn <$> genRegex'
              genTwo fn = fn <$> genRegex' <*> genRegex'

  shrink (Lit _)      = []
  shrink (Empty)      = []
  shrink (And re re') = shrinkTwo And re re'
  shrink (Or re re')  = shrinkTwo Or re re'
  shrink (Mark re)    = shrinkOne Mark re
  shrink (Star re)    = shrinkOne Star re
  shrink (Plus re)    = shrinkOne Plus re


shrinkOne :: Arbitrary a => (a -> a) -> a -> [a]
shrinkOne fn re = foldr1 (<>) [x, [re], (fn <$> x)]
  where x = shrink re


shrinkTwo :: Arbitrary a => (a -> a -> a) -> a -> a -> [a]
shrinkTwo fn re re' = foldr1 (<>) [x, [re], y, [re'], (fn <$> x <*> y)]
  where x = shrink re
        y = shrink re'


genString :: Regex -> Gen String
genString (Empty)      = pure []
genString (Lit c)      = pure [c]
genString (And re re') = (<>) <$> genString re <*> genString re'
genString (Or re re')  = oneof [genString re, genString re']
genString (Mark re)    = oneof [pure [], genString re]
genString (Star re)    = frequency [(1, pure []), (4, genString re)]
genString (Plus re)    = (<>) <$> genString re <*> genString (Star re)


test :: (Regex -> String -> Bool) -> Regex -> Property
test match re = conjoin (replicate 10 $ forAll (genString re) (match re))


testRegex = test Regex.match
testNFA = test NFA.matchRe
testDFA = test DFA.matchRe


main :: IO ()
main = traverse_ quickCheck
    [ testRegex
    , testNFA
    , testDFA
    ]
