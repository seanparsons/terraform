module TerraformTest where

import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances()
import Test.Tasty
import Terraform
import Data.DeriveTH

derive makeArbitrary ''TerraformStringPart

derive makeArbitrary ''TerraformVariableType

derive makeArbitrary ''TerraformValue

derive makeArbitrary ''TerraformStatement

toHCLAndBack = testProperty "parseHCLStatement $ tfStatementToHCLStatement == Right" $
  (\statement -> (parseHCLStatement $ tfStatementToHCLStatement statement) == Right statement)

test_Terraform = [
                    testGroup "Terraform"
                      [ toHCLAndBack
                      ]
                 ]