module TerraformTest where

import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances()
import Test.Tasty
import Terraform
import Data.DeriveTH
import Data.HashMap.Strict

derive makeArbitrary ''TerraformStringPart

derive makeArbitrary ''TerraformVariableType

notNestedValue :: Gen TerraformValue
notNestedValue = oneof [ fmap TerraformNumber arbitrary
                       , fmap TerraformString arbitrary
                       , fmap TerraformIdentifier arbitrary
                       ]

capSize :: Int -> Int
capSize = min 10

nestedValue :: Int -> Gen TerraformValue
nestedValue maxDepth =
  let nested = oneof [ notNestedValue
                     , fmap (TerraformMap  . fromList) $ scale capSize $ listOf $ pure (,) <*> arbitrary <*> nestedValue (maxDepth - 1)
                     , fmap TerraformList $ scale capSize $ listOf $ nestedValue (maxDepth - 1)
                     ]
  in  if maxDepth > 1 then nested else notNestedValue

instance Arbitrary TerraformValue where
  arbitrary = nestedValue 4

derive makeArbitrary ''TerraformStatement

toHCLAndBack = testProperty "parseHCLStatement $ tfStatementToHCLStatement == Right" $ \statement ->
    let parsedResult = parseHCLStatement $ tfStatementToHCLStatement statement
    in  counterexample ("parsedResult = " ++ (show parsedResult)) (parsedResult == Right statement)

toHCLStringPartAndBack = testProperty "hclPartToTFPart $ tfPartToHCLPart == id" $ \stringPart ->
    let parsedResult = hclPartToTFPart $ tfPartToHCLPart stringPart
    in  counterexample ("parsedResult = " ++ (show parsedResult)) (parsedResult == stringPart)

test_Terraform = [
                    testGroup "Terraform"
                      [ toHCLAndBack
                      , toHCLStringPartAndBack
                      ]
                 ]