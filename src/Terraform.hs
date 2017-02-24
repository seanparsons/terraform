module Terraform where

import Data.Text
import Data.HCL
import Data.Scientific
import Data.HashMap.Strict
import Prelude hiding (lookup)

data TerraformVariableType = StringType
                           | ListType
                           | MapType
                           deriving (Eq, Show)

data TerraformStringPart = TerraformStringPlain Text
                         | TerraformStringInterpolation Text
                         deriving (Eq, Show)

type HCLMapContent = HashMap [Text] HCLValue

type TerraformMapContent = HashMap [Text] TerraformValue

data TerraformValue = TerraformNumber Scientific
                    | TerraformString [TerraformStringPart]
                    | TerraformIdentifier Text
                    | TerraformMap TerraformMapContent
                    | TerraformList [TerraformValue]
                    deriving (Eq, Show)

data TerraformStatement = Resource
                        { resourceType          :: Text
                        , resourceName          :: Text
                        , resourceContent       :: TerraformValue
                        }
                        | DataSource
                        { dataSourceType        :: Text
                        , dataSourceName        :: Text
                        , dataSourceContent     :: TerraformValue
                        }
                        | Provider
                        { providerName          :: Text
                        , providerContent       :: TerraformValue
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
                        , outputDependsOn       :: Maybe [TerraformValue]
                        , outputSensitive       :: Maybe Bool
                        }
                        | Module
                        { moduleName            :: Text
                        , moduleSource          :: Text
                        , moduleContent         :: TerraformValue
                        }
                        | Terraform
                        { terraformContent      :: TerraformValue
                        }
                        | Atlas
                        { atlasContent          :: TerraformValue
                        }
                        deriving (Eq, Show)

data StatementParseFailure = UnexpectedValue HCLValue
                           | UnexpectedVariableContent HCLValue
                           | InvalidVariableType TerraformValue
                           | ObjectValueNotFound HCLMapContent Text

mapStringPart :: HCLStringPart -> TerraformStringPart
mapStringPart (HCLStringPlain hclValue)      = TerraformStringPlain hclValue
mapStringPart (HCLStringInterp hclValue)     = TerraformStringInterpolation hclValue

parseValue :: HCLValue -> Either StatementParseFailure TerraformValue
parseValue (HCLNumber hclValue)     = Right $ TerraformNumber hclValue
parseValue (HCLString parts)        = Right $ TerraformString $ fmap mapStringPart parts
parseValue (HCLIdent hclValue)      = Right $ TerraformIdentifier hclValue
parseValue (HCLObject [] content)   = fmap TerraformMap $ traverse parseValue content
parseValue (HCLList elements)       = fmap TerraformList $ traverse parseValue elements
parseValue hclValue                 = Left $ UnexpectedValue hclValue

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

parseMapContent :: HCLMapContent -> Either StatementParseFailure TerraformMapContent
parseMapContent content = traverse parseValue content

parseResource :: Text -> Text -> HCLMapContent -> Either StatementParseFailure TerraformStatement
parseResource typeOfResource nameOfResource content = do
  parsedContent <- parseMapContent content
  return $ Resource
            { resourceType      = typeOfResource
            , resourceName      = nameOfResource
            , resourceContent   = TerraformMap parsedContent
            }

parseDataSource :: Text -> Text -> HCLMapContent -> Either StatementParseFailure TerraformStatement
parseDataSource typeOfDatasource nameOfDataSource content = do
  parsedContent <- parseMapContent content
  return $ DataSource
            { dataSourceType      = typeOfDatasource
            , dataSourceName      = nameOfDataSource
            , dataSourceContent   = TerraformMap parsedContent
            }

parseProvider :: Text -> HCLMapContent -> Either StatementParseFailure TerraformStatement
parseProvider nameOfProvider content = do
  parsedContent <- parseMapContent content
  return $ Provider
            { providerName      = nameOfProvider
            , providerContent   = TerraformMap parsedContent
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

parseHCLStatement :: HCLStatement -> Either StatementParseFailure TerraformStatement
parseHCLStatement (HCLStatementObject (HCLObject ["resource", typeOfResource, nameOfResource] content)) = parseResource typeOfResource nameOfResource content
parseHCLStatement (HCLStatementObject (HCLObject ["data", typeOfDataSource, nameOfDataSource] content)) = parseDataSource typeOfDataSource nameOfDataSource content
parseHCLStatement (HCLStatementObject (HCLObject ["provider", nameOfProvider] content))                 = parseProvider nameOfProvider content
parseHCLStatement (HCLStatementObject (HCLObject ["variable", nameOfVariable] content))                 = parseVariable nameOfVariable content
