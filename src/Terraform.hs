module Terraform where

import Data.Hashable
import Data.Text hiding (empty)
import Data.HCL
import Data.Scientific
import Data.HashMap.Strict
import Prelude hiding (lookup)
import Data.List (sortOn)
import GHC.Generics hiding (moduleName)

data TerraformVariableType = StringType
                           | ListType
                           | MapType
                           deriving (Eq, Ord, Show, Generic)

data TerraformStringPart = TerraformStringPlain Text
                         | TerraformStringInterpolation Text
                         deriving (Eq, Ord, Show, Generic)

type TerraformMapContent = HashMap [Text] TerraformValue

instance Ord TerraformMapContent where
  compare first second = compare (sortOn fst $ toList first) (sortOn fst $ toList second)

data TerraformValue = TerraformNumber Scientific
                    | TerraformString [TerraformStringPart]
                    | TerraformIdentifier Text
                    | TerraformMap TerraformMapContent
                    | TerraformList [TerraformValue]
                    deriving (Eq, Ord, Show, Generic)

data TerraformStatement = Resource
                        { resourceType          :: Text
                        , resourceName          :: Text
                        , resourceContent       :: TerraformMapContent
                        }
                        | DataSource
                        { dataSourceType        :: Text
                        , dataSourceName        :: Text
                        , dataSourceContent     :: TerraformMapContent
                        }
                        | Provider
                        { providerName          :: Text
                        , providerContent       :: TerraformMapContent
                        }
                        | Variable
                        { variableName          :: Text
                        , variableType          :: Maybe TerraformVariableType
                        , variableDefaultValue  :: Maybe TerraformValue
                        , variableDescription   :: Maybe TerraformValue
                        }
                        | Output
                        { outputName            :: Text
                        , outputValue           :: TerraformValue
                        , outputDependsOn       :: Maybe TerraformValue
                        , outputSensitive       :: Maybe Bool
                        }
                        | Module
                        { moduleName            :: Text
                        , moduleSource          :: Text
                        , moduleContent         :: TerraformMapContent
                        }
                        | Terraform
                        { terraformContent      :: TerraformMapContent
                        }
                        | Atlas
                        { atlasContent          :: TerraformMapContent
                        }
                        deriving (Eq, Ord, Show, Generic)

type HCLMapContent = HashMap [Text] HCLValue

deriving instance Ord HCLStringPart

deriving instance Ord HCLValue

instance Ord HCLMapContent where
  compare first second = compare (sortOn fst $ toList first) (sortOn fst $ toList second)

deriving instance Ord HCLStatement

data StatementParseFailure = UnexpectedValue HCLValue
                           | UnexpectedVariableContent HCLValue
                           | InvalidVariableType TerraformValue
                           | ObjectValueNotFound HCLMapContent Text
                           | NotABool TerraformValue
                           | NotAString TerraformValue
                           | UnexpectedStatement HCLStatement
                           deriving (Eq, Ord, Show, Generic)

hclPartToTFPart :: HCLStringPart -> TerraformStringPart
hclPartToTFPart (HCLStringPlain hclValue)      = TerraformStringPlain hclValue
hclPartToTFPart (HCLStringInterp hclValue)     = TerraformStringInterpolation hclValue

tfPartToHCLPart :: TerraformStringPart -> HCLStringPart
tfPartToHCLPart (TerraformStringPlain tfValue)          = HCLStringPlain tfValue
tfPartToHCLPart (TerraformStringInterpolation tfValue)  = HCLStringInterp tfValue

parseValue :: HCLValue -> Either StatementParseFailure TerraformValue
parseValue (HCLNumber hclValue)     = Right $ TerraformNumber hclValue
parseValue (HCLString parts)        = Right $ TerraformString $ fmap hclPartToTFPart parts
parseValue (HCLIdent hclValue)      = Right $ TerraformIdentifier hclValue
parseValue (HCLObject [] content)   = fmap TerraformMap $ traverse parseValue content
parseValue (HCLList elements)       = fmap TerraformList $ traverse parseValue elements
parseValue hclValue                 = Left $ UnexpectedValue hclValue

tfValueToHCLValue :: TerraformValue -> HCLValue
tfValueToHCLValue (TerraformNumber tfValue)       = HCLNumber tfValue
tfValueToHCLValue (TerraformString tfValue)       = HCLString $ fmap tfPartToHCLPart tfValue
tfValueToHCLValue (TerraformIdentifier tfValue)   = HCLIdent tfValue
tfValueToHCLValue (TerraformMap tfValue)          = HCLObject [] $ fmap tfValueToHCLValue tfValue
tfValueToHCLValue (TerraformList tfValue)         = HCLList $ fmap tfValueToHCLValue tfValue

tfMapToHCLMap :: TerraformMapContent -> HCLMapContent
tfMapToHCLMap = fmap tfValueToHCLValue

insertMaybe :: (Eq k, Hashable k) => k -> Maybe a -> HashMap k a -> HashMap k a
insertMaybe _ Nothing toUpdate              = toUpdate
insertMaybe mapKey (Just mapValue) toUpdate = insert mapKey mapValue toUpdate

tfStatementToHCLStatement :: TerraformStatement -> HCLStatement
tfStatementToHCLStatement (Resource typeOfResource nameOfResource content) =
  HCLStatementObject $ HCLObject ["resource", typeOfResource, nameOfResource] (tfMapToHCLMap content)
tfStatementToHCLStatement (DataSource typeOfDataSource nameOfDataSource content) =
  HCLStatementObject $ HCLObject ["data", typeOfDataSource, nameOfDataSource] (tfMapToHCLMap content)
tfStatementToHCLStatement (Provider nameOfProvider content) =
  HCLStatementObject $ HCLObject ["provider", nameOfProvider] (tfMapToHCLMap content)
tfStatementToHCLStatement (Variable nameOfVariable typeOfVariable defaultValue description) =
  HCLStatementObject $ HCLObject ["variable", nameOfVariable] $
    insertMaybe ["type"] (fmap variableTypeToHCLValue typeOfVariable) $
    insertMaybe ["default"] (fmap tfValueToHCLValue defaultValue) $
    insertMaybe ["description"] (fmap tfValueToHCLValue description) $
    empty
tfStatementToHCLStatement (Output nameOfOutput valueOfOutput dependsOn sensitive) =
  HCLStatementObject $ HCLObject ["output", nameOfOutput] $ 
    insert ["value"] (tfValueToHCLValue valueOfOutput) $
    insertMaybe ["depends_on"] (fmap tfValueToHCLValue dependsOn) $
    insertMaybe ["sensitive"] (fmap boolToHCLValue sensitive) $
    empty
tfStatementToHCLStatement (Module nameOfModule sourceOfModule content) =
  HCLStatementObject $ HCLObject ["module", nameOfModule] (insert ["source"] (HCLString [HCLStringPlain sourceOfModule]) $ tfMapToHCLMap content)
tfStatementToHCLStatement (Terraform content) =
  HCLStatementObject $ HCLObject ["terraform"] (tfMapToHCLMap content)
tfStatementToHCLStatement (Atlas content) =
  HCLStatementObject $ HCLObject ["atlas"] (tfMapToHCLMap content)

expectObjectValue :: HCLMapContent -> Text -> Either StatementParseFailure TerraformValue
expectObjectValue content objectKey = do
  parsedMaybeValue <- expectMaybeObjectValue content objectKey
  maybe (Left $ ObjectValueNotFound content objectKey) Right parsedMaybeValue

expectMaybeObjectValue :: HCLMapContent -> Text -> Either StatementParseFailure (Maybe TerraformValue)
expectMaybeObjectValue content objectKey = traverse parseValue $ lookup [objectKey] content

parseVariableType :: TerraformValue -> Either StatementParseFailure TerraformVariableType
parseVariableType (TerraformString [TerraformStringPlain "string"]) = Right StringType
parseVariableType (TerraformString [TerraformStringPlain "map"])    = Right MapType
parseVariableType (TerraformString [TerraformStringPlain "list"])   = Right ListType
parseVariableType terraformValue                                    = Left $ InvalidVariableType terraformValue

variableTypeToHCLValue :: TerraformVariableType -> HCLValue
variableTypeToHCLValue StringType = HCLString [HCLStringPlain "string"]
variableTypeToHCLValue MapType    = HCLString [HCLStringPlain "map"]
variableTypeToHCLValue ListType   = HCLString [HCLStringPlain "list"]

parseVariableAsBool :: TerraformValue -> Either StatementParseFailure Bool
parseVariableAsBool (TerraformIdentifier "true")    = Right True
parseVariableAsBool (TerraformIdentifier "false")   = Right False
parseVariableAsBool terraformValue                  = Left $ NotABool terraformValue

boolToHCLValue :: Bool -> HCLValue
boolToHCLValue True   = HCLIdent "true"
boolToHCLValue False  = HCLIdent "false"

stringPartAsText :: TerraformStringPart -> Text
stringPartAsText (TerraformStringPlain textValue)         = textValue
stringPartAsText (TerraformStringInterpolation textValue) = textValue

parseVariableAsText :: TerraformValue -> Either StatementParseFailure Text
parseVariableAsText (TerraformString parts)       = Right $ foldMap stringPartAsText parts
parseVariableAsText terraformValue                = Left $ NotAString terraformValue

parseMapContent :: HCLMapContent -> Either StatementParseFailure TerraformMapContent
parseMapContent content = traverse parseValue content

parseResource :: Text -> Text -> HCLMapContent -> Either StatementParseFailure TerraformStatement
parseResource typeOfResource nameOfResource content = do
  parsedContent <- parseMapContent content
  return $ Resource
            { resourceType          = typeOfResource
            , resourceName          = nameOfResource
            , resourceContent       = parsedContent
            }

parseDataSource :: Text -> Text -> HCLMapContent -> Either StatementParseFailure TerraformStatement
parseDataSource typeOfDataSource nameOfDataSource content = do
  parsedContent <- parseMapContent content
  return $ DataSource
            { dataSourceType        = typeOfDataSource
            , dataSourceName        = nameOfDataSource
            , dataSourceContent     = parsedContent
            }

parseProvider :: Text -> HCLMapContent -> Either StatementParseFailure TerraformStatement
parseProvider nameOfProvider content = do
  parsedContent <- parseMapContent content
  return $ Provider
            { providerName          = nameOfProvider
            , providerContent       = parsedContent
            }

parseVariable :: Text -> HCLMapContent -> Either StatementParseFailure TerraformStatement
parseVariable nameOfVariable content = do
  possibleTypeValue <- expectMaybeObjectValue content "type"
  possibleType <- traverse parseVariableType possibleTypeValue
  possibleDefaultValue <- expectMaybeObjectValue content "default"
  possibleDescription <- expectMaybeObjectValue content "description"
  return $ Variable
            { variableName          = nameOfVariable
            , variableType          = possibleType
            , variableDefaultValue  = possibleDefaultValue
            , variableDescription   = possibleDescription
            }

parseOutput :: Text -> HCLMapContent -> Either StatementParseFailure TerraformStatement
parseOutput nameOfOutput content = do
  outputValueValue <- expectObjectValue content "value"
  possibleDependsOn <- expectMaybeObjectValue content "depends_on"
  possibleSensitiveValue <- expectMaybeObjectValue content "sensitive"
  possibleSensitive <- traverse parseVariableAsBool possibleSensitiveValue
  return $ Output
            { outputName            = nameOfOutput
            , outputValue           = outputValueValue
            , outputDependsOn       = possibleDependsOn
            , outputSensitive       = possibleSensitive
            }

parseModule :: Text -> HCLMapContent -> Either StatementParseFailure TerraformStatement
parseModule nameOfModule content = do
  moduleSourceValue <- expectObjectValue content "source"
  sourceValue <- parseVariableAsText moduleSourceValue
  parsedContent <- parseMapContent content
  return $ Module
            { moduleName            = nameOfModule
            , moduleSource          = sourceValue
            , moduleContent         = delete ["source"] parsedContent
            }

parseTerraform :: HCLMapContent -> Either StatementParseFailure TerraformStatement
parseTerraform content = do
  parsedContent <- parseMapContent content
  return $ Terraform
            { terraformContent      = parsedContent
            }

parseAtlas :: HCLMapContent -> Either StatementParseFailure TerraformStatement
parseAtlas content = do
  parsedContent <- parseMapContent content
  return $ Atlas
            { atlasContent          = parsedContent
            }

parseHCLStatement :: HCLStatement -> Either StatementParseFailure TerraformStatement
parseHCLStatement (HCLStatementObject (HCLObject ["resource", typeOfResource, nameOfResource] content)) =
  parseResource typeOfResource nameOfResource content
parseHCLStatement (HCLStatementObject (HCLObject ["data", typeOfDataSource, nameOfDataSource] content)) =
  parseDataSource typeOfDataSource nameOfDataSource content
parseHCLStatement (HCLStatementObject (HCLObject ["provider", nameOfProvider] content)) =
  parseProvider nameOfProvider content
parseHCLStatement (HCLStatementObject (HCLObject ["variable", nameOfVariable] content)) =
  parseVariable nameOfVariable content
parseHCLStatement (HCLStatementObject (HCLObject ["output", nameOfOutput] content)) =
  parseOutput nameOfOutput content
parseHCLStatement (HCLStatementObject (HCLObject ["module", nameOfModule] content)) =
  parseModule nameOfModule content
parseHCLStatement (HCLStatementObject (HCLObject ["terraform"] content)) =
  parseTerraform content
parseHCLStatement (HCLStatementObject (HCLObject ["atlas"] content)) =
  parseAtlas content
parseHCLStatement hclStatement = Left $ UnexpectedStatement hclStatement