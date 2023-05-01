{-# LANGUAGE AllowAmbiguousTypes #-}
module Qecs.Compile.ExtractField where

import Language.Haskell.TH
import Type.Reflection

-- | A data type with two fields.
data MyType = MyType
  { field1 :: Int
  , field2 :: String
  } deriving (Show)

-- | A function that extracts all values of a specified type from a data type.
-- extractFields :: forall a. Typeable a => Name -> ExpQ
-- extractFields name = do
--   info <- reify name
--   let fields = extractFieldsOfType (typeRep (Proxy @a)) info
--   let extractFieldExp field = [| $(varE field) x |]
--   let extractFieldsExp = listE (map extractFieldExp fields)
--   lam1E (varP (mkName "x")) extractFieldsExp

-- -- | A function that extracts all fields of a specified type from a data type info.
-- extractFieldsOfType :: TypeRep a -> Info -> [Name]
-- extractFieldsOfType t (TyConI (DataD _ _ _ _ constructors _)) =
--   concatMap extractFieldsFromConstructor constructors
--   where extractFieldsFromConstructor (RecC _ fields) =
--           [name | (name, _, fieldType) <- fields, typeRep fieldType == t]
--         extractFieldsFromConstructor _ = []
-- extractFieldsOfType _ _ = []

-- -- | Example usage of the 'extractFields' function.
-- example :: [String]
-- example = $$(extractFields @String 'MyType) (MyType 42 "hello")