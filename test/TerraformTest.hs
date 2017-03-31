module TerraformTest where

import Data.Text
import Prelude hiding (print)
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances()
import Test.Tasty
import Terraform
import Data.DeriveTH
import Data.HashMap.Strict

derive makeArbitrary ''TerraformVariableType

identText :: Gen Text
identText = fmap pack $ listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['_', '-'])

objectKey :: Gen [Text]
objectKey = listOf1 identText

notNestedValue :: Gen TerraformValue
notNestedValue = oneof [ fmap TerraformNumber arbitrary
                       , fmap TerraformString arbitrary
                       , fmap TerraformBoolean arbitrary
                       ]

capSize :: Int -> Int
capSize = min 10

mapContent :: Int -> Gen TerraformMapContent
mapContent maxDepth = fmap fromList $ scale capSize $ listOf $ pure (,) <*> objectKey <*> nestedValue (maxDepth - 1)

cappedMapContent :: Gen TerraformMapContent
cappedMapContent = mapContent 4

nestedValue :: Int -> Gen TerraformValue
nestedValue maxDepth =
  let nested = oneof [ notNestedValue
                     , fmap TerraformMap $ mapContent maxDepth
                     , fmap TerraformList $ scale capSize $ listOf $ nestedValue (maxDepth - 1)
                     ]
  in  if maxDepth > 1 then nested else notNestedValue

instance Arbitrary TerraformValue where
  arbitrary = nestedValue 4

nonEmptyText :: Gen Text
nonEmptyText = fmap pack $ listOf1 arbitrary

instance Arbitrary TerraformStatement where
  arbitrary = oneof [ pure Resource <*> identText <*> identText <*> cappedMapContent
                    , pure DataSource <*> identText <*> identText <*> cappedMapContent
                    , pure Provider <*> identText <*> cappedMapContent
                    , pure Variable <*> identText <*> arbitrary <*> arbitrary <*> arbitrary
                    , pure Output <*> identText <*> arbitrary <*> arbitrary <*> arbitrary
                    , pure Module <*> identText <*> identText <*> cappedMapContent
                    , pure Terraform <*> cappedMapContent
                    , pure Atlas <*> cappedMapContent
                    ]

derive makeArbitrary ''TerraformConfig

toHCLAndBack = testProperty "parseHCLObject $ tfStatementToHCLObject == Right" $ \statement ->
    let parsedResult = parseHCLObject $ tfStatementToHCLObject statement
    in  counterexample ("parsedResult = " ++ (show parsedResult)) (parsedResult == Right statement)

printAndParseText = testProperty "TerraformConfigSyntax Text" $ \config ->
    let printedResult = print config :: Text
        parsedResult = parse printedResult
    in  counterexample ("printedResult = " ++ (show printedResult)) $
        counterexample ("parsedResult = " ++ (show parsedResult)) $
        (parsedResult == Right config)

test_Terraform = [
                    testGroup "Terraform"
                      [ toHCLAndBack
                      , printAndParseText
                      ]
                 ]