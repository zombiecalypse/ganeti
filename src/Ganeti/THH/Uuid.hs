{-# LANGUAGE TemplateHaskell #-}
module Ganeti.THH.Uuid where

uuidType :: Name -> Q [Dec]
uuidType name = do
  (TyConI d) <- reify name
  (typeName, _, _, constructors) <- typeInfo (return d)
  jsonInstance <- gen_instance ''JSON (conT typeName) constructors
    [ (''readJSON, readJSON)
    , (''showJSON, showJSON)]
  hasStringReprInstance <- gen_instance ''HasStringRepr (conT typeName) constructors
    [ (''fromStringRepr, fromStringRepr)
    , (''toStringRepr, toStringRepr)]
  where
    s = mkName "s"
    uuidPattern = "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
    readJSON = [| \s -> do
      str <- readJSON :: Result String
      guard $ str Regex.=~ uuidPattern
      return $ Uuid str |]
    showJSON = [| \(Uuid s) -> s |]
    fromStringRepr = [| \s -> do
      guard $ s Regex.=~ uuidPattern
      return $ Uuid s |]
    toStringRepr = [| \(Uuid s) = s |]
