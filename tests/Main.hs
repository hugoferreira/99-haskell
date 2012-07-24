module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

summinus :: Integer -> Bool
summinus d = (d + 2 - 2) == d

tests :: [TF.Test]
tests = [ 
	testGroup "Sanity Check" [ 
		testProperty "summinus" summinus 
	] ]