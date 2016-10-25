module Data.CoreFn.Parser where

import Prelude
import Control.Alt ((<|>))
import Data.Array (head)
import Data.Either (Either)
import Data.Foreign (fail, ForeignError(..))
import Data.Foreign.Class (class IsForeign, readProp, read, readEitherR)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.Generic (class Generic, gShow, gEq)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)

data CoreFn = CoreFn String Module

derive instance genericCoreFn :: Generic CoreFn

instance showCoreFn :: Show CoreFn where
  show = gShow

instance isForeignCoreFn :: IsForeign CoreFn where
  read v = do
    ks <- keys v
    case head ks of
      Just name -> CoreFn name <$> readProp name v
      Nothing -> fail $ JSONError "missing module name"

data Module = Module { exports :: Array String
                     , builtWith :: String
                     , imports :: Array String
                     , foreigns :: Array String
                     , decls :: Array Bind
                     }


derive instance genericModule :: Generic Module

instance showModule :: Show Module where
  show = gShow

instance isForeignModule :: IsForeign Module where
  read v = do
    exports <- readProp "exports" v
    builtWith <- readProp "builtWith" v
    imports <- readProp "imports" v
    foreigns <- readProp "foreign" v
    decls <- readProp "decls" v
    pure $ Module { exports, builtWith, imports, foreigns, decls }

-- data ProperNameType = TypeName | ConstructorName | ClassName | Namespace
data TypeName
data ConstructorName
data ClassName
data Namespace

newtype ProperName a = ProperName String

derive instance genericProperName :: Generic (ProperName a)

instance showProperName :: Show (ProperName a) where
  show = gShow

instance isForeignProperName :: IsForeign (ProperName a) where
  read v = ProperName <$> read v


data Literal a
  = IntLiteral Int
  | NumberLiteral Number
  | StringLiteral String
  | CharLiteral Char
  | BooleanLiteral Boolean
  | ArrayLiteral (Array a)
  | ObjectLiteral (ObjectFields a)

derive instance genericLiteral :: Generic a => Generic (Literal a)

instance showLiteral :: (Show a, Generic a) => Show (Literal a) where
  show = gShow

instance isForeignLiteral :: IsForeign a => IsForeign (Literal a) where
  read v = do
    o <- v ! 0 >>= read
    case o of
      "StringLiteral" -> StringLiteral <$> (v ! 1 >>= read)
      "IntLiteral" -> IntLiteral <$> (v ! 1 >>= read)
      "NumberLiteral" -> NumberLiteral <$> (v ! 1 >>= read)
      "CharLiteral" -> CharLiteral <$> (v ! 1 >>= read)
      "BooleanLiteral" -> BooleanLiteral <$> (v ! 1 >>= read)
      "ArrayLiteral" -> ArrayLiteral <$> (v ! 1 >>= read)
      "ObjectLiteral" -> ObjectLiteral <$> (v ! 1 >>= read)
      l -> fail $ JSONError $ "unknown literal:" <> l


newtype ObjectFields a = ObjectFields (Array (PTuple String a))

derive instance genericObjectFields :: Generic a => Generic (ObjectFields a)

instance showObjectFields :: (Show a, Generic a) => Show (ObjectFields a) where
  show = gShow

instance isForeignObjectFields :: IsForeign a => IsForeign (ObjectFields a) where
  read v = do
    let readField k = PTuple k <$> readProp k v
    ks <- keys v
    fs <- sequence $ readField <$> ks
    pure $ ObjectFields fs

data Expr
  = App Expr Expr
  | Var (Qualified Ident)
  | Lit (Literal Expr)
  | Constructor (ProperName TypeName) (ProperName ConstructorName) (Array Ident)
  | Accesor String Expr
  | Abs Ident Expr
  | Case (Array Expr) (Array CaseAlternative)
  | Let (Array Bind) Expr
  | ObjectUpdate Expr (Array (PTuple String Expr))

derive instance genericExpr :: Generic Expr

instance showExpr :: Show Expr where
  show = gShow

instance isForeignExpr :: IsForeign Expr where
  read v = do
    o <- v ! 0 >>= read
    case o of
      "App" -> App <$> (v ! 1 >>= read) <*> (v ! 2 >>= read)
      "Var" -> Var <$> (v ! 1 >>= read)
      "Literal" -> Lit <$> (v ! 1 >>= read)
      "Constructor" -> Constructor <$> (v ! 1 >>= read) <*> (v ! 2 >>= read) <*> (v ! 3 >>= read)
      "Accessor" -> Accesor <$> (v ! 1 >>= read) <*> (v ! 2 >>= read)
      "Abs" -> Abs <$> (v ! 1 >>= read) <*> (v ! 2 >>= read)
      "Case" -> Case <$> (v ! 1 >>= read) <*> (v ! 2 >>= read)
      "Let" -> Let <$> (v ! 1 >>= read) <*> (v ! 2 >>= read)
      "ObjectUpdate" -> ObjectUpdate <$> (v ! 1 >>= read) <*> (v ! 2 >>= read)
      l -> fail $ JSONError $ "unknown expression:" <> l


newtype BindExpr = BindExpr Expr

derive instance genericBindExpr :: Generic BindExpr

instance showBindExpr :: Show BindExpr where
  show = gShow

instance isForeignBindExpr :: IsForeign BindExpr where
  read v = BindExpr <$> read v


data Bind
 = NonRec Ident Expr
 | Rec (Array (PTuple Ident Expr))

derive instance genericBind :: Generic Bind

instance showBind :: Show Bind where
  show = gShow

instance isForeignBind :: IsForeign Bind where
  read v = do
    ks <- keys v
    case head ks of
      Just x -> NonRec (Ident x) <$> readProp x v <|> Rec <$> readProp x v
      Nothing -> fail $ JSONError "missing key in bind"


newtype Ident = Ident String

derive instance genericIdent :: Generic Ident

instance eqIdent :: Eq Ident where
  eq = gEq

instance showIdent :: Show Ident where
  show = gShow

instance isForeignIdent :: IsForeign Ident where
  read v = Ident <$> read v
    
    

data Qualified a = Qualified (PMaybe ModuleName) a

derive instance genericQualified :: Generic a => Generic (Qualified a)

instance showQualified :: Generic a => Show (Qualified a) where
  show = gShow

instance isForeignQualified :: IsForeign a => IsForeign (Qualified a) where
  read v = Qualified <$> read v <*> read v


newtype ModuleName = ModuleName (Array (ProperName Namespace))

derive instance genericModuleName :: Generic ModuleName

instance showModuleName :: Show ModuleName where
  show = gShow

instance isForeignModuleName :: IsForeign ModuleName where
  read v = ModuleName <$> read v


data CaseAlternative = CaseAlternative (Array Binder) (Either (Array (PTuple Expr Expr)) Expr)

derive instance genericCaseAlternative :: Generic CaseAlternative

instance showCaseAlternative :: Show CaseAlternative where
  show = gShow

instance isForeignCaseAlternative :: IsForeign CaseAlternative where
  read v = CaseAlternative <$> (v ! 0 >>= read) <*> (v ! 1 >>= readEitherR)

data Binder
  = NullBinder
  | LiteralBinder (Literal Binder)
  | VarBinder Ident
  | ConstructorBinder (Qualified (ProperName TypeName)) (Qualified (ProperName ConstructorName)) (Array Binder)
  | NamedBinder Ident Binder


derive instance genericBinder :: Generic Binder

instance showBinder :: Show Binder where
  show = gShow

instance isForeignBinder :: IsForeign Binder where
  read v = do
    nb <- v ! 0 >>= read
    case nb of
      "NullBinder" -> pure NullBinder
      "LiteralBinder" -> LiteralBinder <$> (v ! 1 >>= read)
      "VarBinder" -> VarBinder <$> (v ! 1 >>= read)
      "ConstructorBinder" -> ConstructorBinder <$> (v ! 1 >>= read) <*> (v ! 2 >>= read) <*> (v ! 3 >>= read)
      "NamedBinder" -> NamedBinder <$> (v ! 1 >>= read) <*> (v ! 2 >>= read)
      l -> fail $ JSONError $ "binder:" <> l


data PTuple a b = PTuple a b

derive instance genericPTuple :: (Generic a, Generic b) => Generic (PTuple a b)

instance showPTuple :: (Generic a, Generic b, Show a, Show b) => Show (PTuple a b) where
  show = gShow

instance isForeignPTuple :: (IsForeign a, IsForeign b) => IsForeign (PTuple a b) where
  read v = PTuple <$> ((v ! 0) >>= read) <*> ((v ! 1) >>= read)


data PMaybe a = PJust a | PNothing

derive instance genericPMaybe :: Generic a => Generic (PMaybe a)

instance showPMaybe :: (Generic a, Show a) => Show (PMaybe a) where
  show = gShow

instance isForeignPMaybe :: (IsForeign a) => IsForeign (PMaybe a) where
  read v = PJust <$> read v <|> pure PNothing
