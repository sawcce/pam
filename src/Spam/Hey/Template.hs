{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Spam.Hey.Template where

import Control.Monad
import Language.Haskell.TH
import Spam.Hey (Convert, Evaluate)

curryN :: Int -> Q Exp
curryN n = do
  f <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f : xs)
      ntup = TupE (map (Just . VarE) xs)
  return $ LamE args (AppE (VarE f) ntup)

genCurries :: Int -> Q [Dec]
genCurries n = forM [1 .. n] mkCurryDec
  where
    mkCurryDec ith = do
      cury <- curryN ith
      let name = mkName $ "curry" ++ show ith
      return $ FunD name [Clause [] (NormalB cury) []]

-- test :: String -> Type -> Dec
-- test n t = DataD [] (mkName $ "test" ++ n) [] Nothing [t] []

-- test :: String -> Q Type -> Q [Dec]
-- test n qt = do
--   t <- qt
--   let conName = mkName $ "Con" ++ n
--       typeName = mkName $ "Test" ++ n
--       con = NormalC conName [(Bang NoSourceUnpackedness NoSourceStrictness, t)]
--       showC = DerivClause Nothing [ConT ''Show]
--   return [DataD [] typeName [] Nothing [con] [showC]]

showC = DerivClause Nothing [ConT ''Show]

model :: Q [Dec] -> Q [Dec]
model decsQ = do
  decsQ <> (concatMap makeTypes <$> decsQ)

makeTypes :: Dec -> [Dec]
makeTypes (DataD _ _ _ _ cons _) = do
  concatMap handleCons cons
makeTypes _ = []

handleCons :: Con -> [Dec]
handleCons (RecC name vbts) = concat $ concatMap (makeField name) vbts
handleCons _ = []

makeField :: Name -> VarBangType -> [[Dec]]
makeField cname (name, _, typ) = do
  let n = mkName $ nameBase cname <> "Arg" <> nameBase name
  let cc = mkName $ nameBase cname
  let con = NormalC n []
  let convertInstance =
        InstanceD
          Nothing
          []
          (AppT (AppT (ConT ''Convert) (ConT $ mkName $ nameBase cname)) (ConT n))
          [ FunD
              (mkName "convert")
              [ Clause
                  [ConP n [] []]
                  (NormalB (LitE (StringL (nameBase cname <> "." <> nameBase name))))
                  []
              ]
          ]
  let evalInstance =
        InstanceD
          Nothing
          []
          (AppT (AppT (AppT (ConT ''Evaluate) (ConT cc)) (ConT n)) typ)
          [ FunD
              (mkName "evaluate")
              [ Clause
                  [VarP $ mkName "ctx", ConP n [] []]
                  (NormalB (VarE (mkName $ nameBase name) `AppE` VarE (mkName "ctx")))
                  []
              ]
          ]

  return [DataD [] n [] Nothing [con] [showC], convertInstance, evalInstance]