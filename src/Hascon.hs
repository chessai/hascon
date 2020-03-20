{-# language
    AllowAmbiguousTypes
  , DataKinds
  , KindSignatures
  , LambdaCase
  , OverloadedStrings
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , ViewPatterns
#-}

module Hascon
  (
    -- * Main API
    mkHasCon
  , HasConstructor
  , con

    -- * Re-exports
  , Symbol, KnownSymbol
  , TypeError
  , ErrorMessage(..)
  , Constraint
  , fromString
  ) where

import Data.Kind (Constraint)
import Data.String (IsString(..))
import GHC.TypeLits
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup.Foldable (Foldable1(..))
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Datatype
import qualified Data.Text as T

mkHasCon :: Name -> Q [Dec]
mkHasCon n = do
  d <- reifyDatatype n
  let typeName = datatypeName d
  let conNames = map constructorName (datatypeCons d)

  let tyFam = mk typeName conNames
  let hasCon = mkHasConstructor typeName

  pure [tyFam, hasCon]

mk :: ()
  => Name
     -- ^ type name
  -> [Name]
     -- ^ constructor names
  -> Dec
mk typeName conNames =
  let hd = mkHead typeName
      eqns = mkEqns typeName conNames
  in ClosedTypeFamilyD hd eqns

mkHead :: ()
  => Name
     -- ^ type name
  -> TypeFamilyHead
mkHead (cleanup -> n) =
  let symbolKind = ConT (mkName "Symbol")
      constraintKind = ConT (mkName "Constraint")

      name = mkName $ "HasConstructor" ++ n
      bndrs = [ KindedTV (mkName "name") symbolKind ]
      resultSig = KindSig constraintKind
      inj = Nothing
  in TypeFamilyHead name bndrs resultSig inj

cleanup :: Name -> String
cleanup = id
  . T.unpack
  . head
  . T.splitOn "_"
  . last
  . T.splitOn "."
  . T.pack
  . show

mkEqn :: ()
  => Name
     -- ^ type name
  -> Name
     -- ^ constructor name
  -> TySynEqn
mkEqn (cleanup -> typeName) (cleanup -> conName) =
  let famName = mkName $ "HasConstructor" ++ typeName
      bndrs = Nothing
      lhs = AppT (ConT famName) (LitT (StrTyLit conName))
      rhs = TupleT 0

  in TySynEqn bndrs lhs rhs

mkEqns :: Name -> [Name] -> [TySynEqn]
mkEqns t@(cleanup -> typeName) = go
  where
    go = \case
      [] ->
        let text = AppT (PromotedT (mkName "Text")) . LitT . StrTyLit
            wildcard = VarT (mkName "c")

            famName = mkName $ "HasConstructor" ++ typeName
            bndrs = Nothing
            lhs = AppT (ConT famName) wildcard
            rhs = ConT (mkName "TypeError")
              `AppT` ParensT (align (
                   (text "Constructor ")
                :| [ AppT (PromotedT (mkName "ShowType")) wildcard
                   , text " does not exist for type "
                   , text typeName
                   ]))
        in [TySynEqn bndrs lhs rhs]
      (c:cs) -> mkEqn t c : go cs

newtype ErrMsg = ErrMsg { getErrMsg :: Type }
instance Semigroup ErrMsg where
  ErrMsg t1 <> ErrMsg t2 = ErrMsg
    (AppT (AppT (PromotedT (mkName ":<>:")) t1) t2)

align :: NonEmpty Type -> Type
align = getErrMsg . foldMap1 ErrMsg

mkHasConstructor :: ()
  => Name
     -- ^ type name
  -> Dec
mkHasConstructor (cleanup -> typeName) =
  let nameKind = VarT (mkName "name")
      bndrs = Nothing
      lhs = AppT (AppT (ConT ''HasConstructor) (ConT (mkName typeName))) (VarT (mkName "name"))
      rhs = AppT (ConT (mkName $ "HasConstructor" ++ typeName)) nameKind

  in TySynInstD (TySynEqn bndrs lhs rhs)

type family HasConstructor a (name :: Symbol) :: Constraint

con :: forall a (name :: Symbol) s. (KnownSymbol name, HasConstructor a name, IsString s) => s
con = fromString (symbolVal (Proxy @name))
