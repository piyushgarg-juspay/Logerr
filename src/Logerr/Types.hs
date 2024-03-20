module Logerr.Types where
import Data.Aeson
import SrcLoc 
import Var
import Outputable as OP hiding ((<>))

data LogggingError = LogggingError
  {
    pkg_name :: String,
    mod_name :: String,
    err_msg :: String,
    src_span :: SrcSpan,
    violation :: Violation
  } deriving (Eq)

instance Show LogggingError where
  show (LogggingError _ _ err _ _) = err

type Rules = [Rule]
type ArgNo = Int
type FnsBlockedInArg = [String]
type TypesBlockedInArg = [String]
type TypesToCheckInArg = [String]

data Rule = Rule 
  {
    fn_name :: String,
    arg_no :: ArgNo,
    fns_blocked_in_arg :: FnsBlockedInArg,
    types_blocked_in_arg :: TypesBlockedInArg,
    types_to_check_in_arg :: TypesToCheckInArg
  } deriving (Show, Eq)  

data LocalVar = FnArg Var | FnWhere Var | FnLocal Var
  deriving (Eq)

instance Show LocalVar where
  show (FnArg x)   = "FnArg " Prelude.<> showS x
  show (FnWhere x) = "FnWhere " Prelude.<> showS x
  show (FnLocal x) = "FnLocal " Prelude.<> showS x

data Violation = 
    ArgTypeBlocked String Rule
  | FnBlockedInArg String Rule
  | FnUseBlocked Rule
  | NoViolation
  deriving (Eq)

instance Show Violation where
  show (ArgTypeBlocked typ rule) = "Use of '" <> (fn_name rule) <> "' on '" <> typ <> "' is not allowed."
  show (FnBlockedInArg fnName rule) = "Use of '" <> fnName <> "' inside argument of '" <> (fn_name rule) <> "' is not allowed."
  show (FnUseBlocked rule) = "Use of '" <> (fn_name rule) <> "' in the code is not allowed."
  show NoViolation = "NoViolation"

showS :: (Outputable a) => a -> String
showS = showSDocUnsafe . ppr

defaultRule :: Rule
defaultRule = Rule "NA" (-1) [] [] []

emptyLoggingError :: LogggingError
emptyLoggingError = LogggingError "" "" "$NA$" noSrcSpan NoViolation

data FunctionInfo = FunctionInfo
  { package_name :: String
  , module_name :: String
  , name    :: String
  , src_Loc    :: String
  } deriving (Show, Eq, Ord)

data Function = Function
  {  function_name    :: String
  , functions_called :: [Maybe FunctionInfo]
  , where_functions :: [Function]
  , src_loc    :: String
  } deriving (Show, Eq, Ord)

data MissingTopLevelBindsSignature = MissingTopLevelBindsSignature {
  srcSpan :: String
  , typeSignature :: String
} deriving (Show, Eq, Ord)

instance ToJSON MissingTopLevelBindsSignature where
  toJSON (MissingTopLevelBindsSignature srcSpan typeSignature) =
    object [ "srcSpan" .= srcSpan
           , "typeSignature"  .= typeSignature
           ]

instance ToJSON FunctionInfo where
  toJSON (FunctionInfo pkg modName funcName srcLoc) =
    object [ "package_name" .= pkg
           , "module_name"  .= modName
           , "name"         .= funcName
           , "src_loc"         .= srcLoc
           ]

instance ToJSON Function where
  toJSON (Function funcName funcsCalled whereFuncs srcLoc) =
    object [ "function_name"    .= funcName
           , "functions_called" .= funcsCalled
           , "where_functions"  .= whereFuncs
           , "src_loc"         .= srcLoc
           ]

instance FromJSON FunctionInfo where
  parseJSON = withObject "FunctionInfo" $ \v ->
    FunctionInfo <$> v .: "package_name"
                 <*> v .: "module_name"
                 <*> v .: "name"
                 <*> v .: "src_loc"

instance FromJSON Function where
  parseJSON = withObject "Function" $ \v ->
    Function <$> v .: "function_name"
             <*> v .: "functions_called"
             <*> v .: "where_functions"
             <*> v .: "src_loc"