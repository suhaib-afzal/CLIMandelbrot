module Spec where

import Test.Hspec
import qualified LibSpec
import qualified ParsingSpec

main :: IO()
main = do
  hspec LibSpec.spec
  hspec ParsingSpec.spec
