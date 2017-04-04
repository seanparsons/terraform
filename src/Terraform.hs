module Terraform where

import Data.Bifunctor
import Data.Hashable
import Data.List (sortBy)
import Data.Text hiding (empty)
import Data.Text.IO
import HCL
import HCL.Types
import Data.Scientific
import Data.HashMap.Strict
import Prelude hiding (lookup, print, putStrLn)
import Data.List (sortOn)
import GHC.Generics hiding (moduleName)
import Text.Megaparsec.Error
import Control.Lens

data TerraformVariableType = StringType
                           | ListType
                           | MapType
                           deriving (Eq, Ord, Show, Generic)

type TerraformMapContent = HashMap [Text] TerraformValue

instance Ord TerraformMapContent where
  compare firstMap secondMap = compare (sortOn fst $ toList firstMap) (sortOn fst $ toList secondMap)

data TerraformValue = TerraformNumber Scientific
                    | TerraformString Text
                    | TerraformBoolean Bool
                    | TerraformMap TerraformMapContent
                    | TerraformList [TerraformValue]
                    deriving (Eq, Ord, Show, Generic)

makePrisms ''TerraformValue

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

makePrisms ''TerraformStatement

type MegaparsecError = ParseError Char Dec

deriving instance Ord MegaparsecError

data StatementParseFailure = UnexpectedValue HCLValue
                           | UnexpectedVariableContent HCLValue
                           | InvalidVariableType TerraformValue
                           | ObjectValueNotFound HCLMapContent Text
                           | NotABool TerraformValue
                           | NotAString TerraformValue
                           | UnexpectedStatement HCLObject
                           | HCLParseFailure MegaparsecError
                           deriving (Eq, Ord, Show, Generic)

newtype TerraformConfig = TerraformConfig { statements :: [TerraformStatement] }
                          deriving (Eq, Ord, Show, Generic)

makeLensesFor [("statements", "terraformConfigStatements")] ''TerraformConfig

hclParseErrorAsParseFailure :: ParseError Char Dec -> StatementParseFailure
hclParseErrorAsParseFailure parserError = HCLParseFailure parserError

class AsTerraformConfig a where
  asConfig :: a -> TerraformConfig

instance AsTerraformConfig TerraformConfig where
  asConfig = id

instance AsTerraformConfig TerraformStatement where
  asConfig = TerraformConfig . return

class TerraformConfigSyntax a where
  parse :: a -> Either StatementParseFailure TerraformConfig
  print :: TerraformConfig -> a

instance TerraformConfigSyntax Text where
  parse :: Text -> Either StatementParseFailure TerraformConfig
  parse parseInput = do
    (HCLDocument objects) <- first hclParseErrorAsParseFailure $ parseHCL "" parseInput
    configStatements <- traverse parseHCLObject objects
    return $ TerraformConfig configStatements
  print :: TerraformConfig -> Text
  print (TerraformConfig configStatements) = hclDocumentToText $ HCLDocument $ fmap tfStatementToHCLObject configStatements

putConfig :: AsTerraformConfig a => a -> IO ()
putConfig terraformConfig = putStrLn $ print $ asConfig terraformConfig

parseValue :: HCLValue -> Either StatementParseFailure TerraformValue
parseValue (HCLNumber hclValue)                     = Right $ TerraformNumber hclValue
parseValue (HCLString text)                         = Right $ TerraformString text
parseValue (HCLBoolean bool)                        = Right $ TerraformBoolean bool
parseValue (HCLObjectValue (HCLObject [] content))  = fmap TerraformMap $ traverse parseValue content
parseValue (HCLList listElems)                      = fmap TerraformList $ traverse parseValue listElems
parseValue hclValue                                 = Left $ UnexpectedValue hclValue

tfValueToHCLValue :: TerraformValue -> HCLValue
tfValueToHCLValue (TerraformNumber tfValue)       = HCLNumber tfValue
tfValueToHCLValue (TerraformString tfValue)       = HCLString tfValue
tfValueToHCLValue (TerraformBoolean tfValue)      = HCLBoolean tfValue
tfValueToHCLValue (TerraformMap tfValue)          = HCLObjectValue $ HCLObject [] $ fmap tfValueToHCLValue tfValue
tfValueToHCLValue (TerraformList tfValue)         = HCLList $ fmap tfValueToHCLValue tfValue

tfMapToHCLMap :: TerraformMapContent -> HCLMapContent
tfMapToHCLMap = fmap tfValueToHCLValue

insertMaybe :: (Eq k, Hashable k) => k -> Maybe a -> HashMap k a -> HashMap k a
insertMaybe _ Nothing toUpdate              = toUpdate
insertMaybe mapKey (Just mapValue) toUpdate = insert mapKey mapValue toUpdate

tfStatementToHCLObject :: TerraformStatement -> HCLObject
tfStatementToHCLObject (Resource typeOfResource nameOfResource content) =
  HCLObject ["resource", typeOfResource, nameOfResource] (tfMapToHCLMap content)
tfStatementToHCLObject (DataSource typeOfDataSource nameOfDataSource content) =
  HCLObject ["data", typeOfDataSource, nameOfDataSource] (tfMapToHCLMap content)
tfStatementToHCLObject (Provider nameOfProvider content) =
  HCLObject ["provider", nameOfProvider] (tfMapToHCLMap content)
tfStatementToHCLObject (Variable nameOfVariable typeOfVariable defaultValue description) =
  HCLObject ["variable", nameOfVariable] $
    insertMaybe ["type"] (fmap variableTypeToHCLValue typeOfVariable) $
    insertMaybe ["default"] (fmap tfValueToHCLValue defaultValue) $
    insertMaybe ["description"] (fmap tfValueToHCLValue description) $
    empty
tfStatementToHCLObject (Output nameOfOutput valueOfOutput dependsOn sensitive) =
  HCLObject ["output", nameOfOutput] $
    insert ["value"] (tfValueToHCLValue valueOfOutput) $
    insertMaybe ["depends_on"] (fmap tfValueToHCLValue dependsOn) $
    insertMaybe ["sensitive"] (fmap boolToHCLValue sensitive) $
    empty
tfStatementToHCLObject (Module nameOfModule sourceOfModule content) =
  HCLObject ["module", nameOfModule] (insert ["source"] (HCLString sourceOfModule) $ tfMapToHCLMap content)
tfStatementToHCLObject (Terraform content) =
  HCLObject ["terraform"] (tfMapToHCLMap content)
tfStatementToHCLObject (Atlas content) =
  HCLObject ["atlas"] (tfMapToHCLMap content)

expectObjectValue :: HCLMapContent -> Text -> Either StatementParseFailure TerraformValue
expectObjectValue content objectKey = do
  parsedMaybeValue <- expectMaybeObjectValue content objectKey
  maybe (Left $ ObjectValueNotFound content objectKey) Right parsedMaybeValue

expectMaybeObjectValue :: HCLMapContent -> Text -> Either StatementParseFailure (Maybe TerraformValue)
expectMaybeObjectValue content objectKey = traverse parseValue $ lookup [objectKey] content

parseVariableType :: TerraformValue -> Either StatementParseFailure TerraformVariableType
parseVariableType (TerraformString "string") = Right StringType
parseVariableType (TerraformString "map")    = Right MapType
parseVariableType (TerraformString "list")   = Right ListType
parseVariableType terraformValue             = Left $ InvalidVariableType terraformValue

variableTypeToHCLValue :: TerraformVariableType -> HCLValue
variableTypeToHCLValue StringType = HCLString "string"
variableTypeToHCLValue MapType    = HCLString "map"
variableTypeToHCLValue ListType   = HCLString "list"

parseVariableAsBool :: TerraformValue -> Either StatementParseFailure Bool
parseVariableAsBool (TerraformBoolean bool) = Right bool
parseVariableAsBool terraformValue          = Left $ NotABool terraformValue

boolToHCLValue :: Bool -> HCLValue
boolToHCLValue = HCLBoolean

parseVariableAsText :: TerraformValue -> Either StatementParseFailure Text
parseVariableAsText (TerraformString text)  = Right text
parseVariableAsText terraformValue          = Left $ NotAString terraformValue

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

parseHCLObject :: HCLObject -> Either StatementParseFailure TerraformStatement
parseHCLObject (HCLObject ["resource", typeOfResource, nameOfResource] content) =
  parseResource typeOfResource nameOfResource content
parseHCLObject (HCLObject ["data", typeOfDataSource, nameOfDataSource] content) =
  parseDataSource typeOfDataSource nameOfDataSource content
parseHCLObject (HCLObject ["provider", nameOfProvider] content) =
  parseProvider nameOfProvider content
parseHCLObject (HCLObject ["variable", nameOfVariable] content) =
  parseVariable nameOfVariable content
parseHCLObject (HCLObject ["output", nameOfOutput] content) =
  parseOutput nameOfOutput content
parseHCLObject (HCLObject ["module", nameOfModule] content) =
  parseModule nameOfModule content
parseHCLObject (HCLObject ["terraform"] content) =
  parseTerraform content
parseHCLObject (HCLObject ["atlas"] content) =
  parseAtlas content
parseHCLObject hclObject = Left $ UnexpectedStatement hclObject

prettySortOrdering :: TerraformStatement -> TerraformStatement -> Ordering
prettySortOrdering v1@ (Variable _ _ _ _) v2@ (Variable _ _ _ _)  = compare v1 v2
prettySortOrdering (Variable _ _ _ _) _                           = LT
prettySortOrdering (Output _ _ _ _) (Variable _ _ _ _)            = GT
prettySortOrdering o1@ (Output _ _ _ _) o2@ (Output _ _ _ _)      = compare o1 o2
prettySortOrdering (Output _ _ _ _) _                             = LT
prettySortOrdering ts1 ts2                                        = compare ts1 ts2

prettySort :: TerraformConfig -> TerraformConfig
prettySort = over terraformConfigStatements (sortBy prettySortOrdering)