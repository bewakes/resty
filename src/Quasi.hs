{-# LANGUAGE TemplateHaskell #-}
module Quasi where

import Debug.Trace
import Database.Persist
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Text(unpack)

import CoreFilters

strEqClause :: String -> String -> Clause
strEqClause key field = let x = mkName "x" in
    Clause
        [LitP (StringL key), VarP x]
        (NormalB (
            ListE [(InfixE (Just (ConE (mkName field)))
                (VarE '(Database.Persist.==.))
                (Just (AppE (VarE 'unpack) (VarE x)))
            )])
        )
        []

icontainsClause :: String -> String -> Clause
icontainsClause key field = let x = mkName "x" in
    Clause
        [LitP (StringL (key<>"__icontains")), VarP x]
        (NormalB (
            AppE (AppE (VarE 'icontains) (ConE (mkName field))) (VarE x)
        ))
        []

containsClause :: String -> String -> Clause
containsClause key field = let x = mkName "x" in
    Clause
        [LitP (StringL (key<>"__contains")), VarP x]
        (NormalB (
            AppE (AppE (VarE 'contains) (ConE (mkName field))) (VarE x)
        ))
        []

getStringClauses :: String -> String -> [Clause]
getStringClauses key field =
    [ strEqClause key field
    , icontainsClause key field
    , containsClause key field
    ]


intOpClause :: String -> String -> String -> Name -> Clause
intOpClause key field suffx op = let x = mkName "x" in
    Clause
        [LitP (StringL (key<>suffx)), VarP x]
        (NormalB
            (AppE
                (AppE
                    (AppE
                        (VarE 'intFilter)
                        (VarE x)
                    )
                    (ConE (mkName field))
                )
                (VarE op)
            )
        )
        []

getIntClauses :: String -> String -> [Clause]
getIntClauses key field =
    [ intOpClause key field "" '(==.)
    , intOpClause key field "__lt" '(<.)
    , intOpClause key field "__gt" '(>.)
    , intOpClause key field "__lte" '(<=.)
    , intOpClause key field "__gte" '(>=.)
    ]

instanceDecl :: String -> Q [Dec]
instanceDecl name = [d|
        instance Filterable $a
            -- mkFilter "color" x = [CarColor ==. (unpack x)]
    |]
    where a = conT (mkName name)

toInstanceDecl :: ObjFields -> Q [Dec]
toInstanceDecl (className, fieldsStrs) = do
    atype <- a
    pure $ [InstanceD Nothing [] (AppT (ConT ''Filterable) atype) [FunD (mkName "mkFilter") clauses]]
    where a = conT (mkName className)
          defaultClause = [Clause [WildP,WildP] (NormalB (ConE '[])) []]
          coreClauses = mconcat $ Prelude.map getClauses fieldsStrs
          clauses = coreClauses ++ defaultClause
          getClauses s = let (k:f:t:[]) = Prelude.words s in
                             case t of
                               "String" -> getStringClauses k f
                               "Int" -> getIntClauses k f

type ObjFields = (String, [String])

parseLines :: [String] -> Maybe [ObjFields]
parseLines [] = Nothing
parseLines lns = parseLines' lns
    where parseLines' [] = Just []
          parseLines' (x:xs) = do
            fields <- pure $ Prelude.takeWhile ((== ' ') . head) xs
            objFields <- parseLines' $ Prelude.dropWhile ((== ' ') . head) xs
            pure $ (x, fields):objFields

parseDeclaration :: String -> Q [Dec]
parseDeclaration s =
    let decLines = Prelude.filter (/= "") $ Prelude.lines s
     in case parseLines decLines of
          Nothing -> fail "unparseable quasi"
          Just xs -> mconcat <$> mapM toInstanceDecl xs

filterable :: QuasiQuoter
filterable = QuasiQuoter
    { quoteExp = undefined -- parseFilters
    , quotePat =
        error "filters can't be used as pattern"
    , quoteType =
        error "filters can't be used as type"
    , quoteDec = parseDeclaration -- instanceDecl . (unpack . strip . pack)
    }

testq = [d|
    instance Num String where
        "bibe" + y = y
        "bibek" + y = "bibek"
        x - y = x
    |]
