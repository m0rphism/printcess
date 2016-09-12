{-# LANGUAGE LambdaCase, UnicodeSyntax #-}

import Printcess.PrettyPrinting

type Ident = String

data Expr
  = EVar Ident
  | EAbs Ident Expr
  | EApp Expr Expr

instance Pretty Expr where
  pp = \case
    EVar i     → pp i
    EAbs i e   → assocR 0 $ "λ" +> i +> ". " +> R e
    EApp e1 e2 → assocL 9 $ L e1 +> " " +> R e2

expr0, expr1, expr2 :: Expr
expr0 = EApp (EVar "x") (EVar "y")
expr1 = EAbs "x" $ EAbs "y" $ EApp expr0 expr0
expr2 = foldl EApp expr0 (replicate 20 expr0)

main :: IO ()
main = do
  prettyPrint def $ "Printcess Pretty Printing Example"
  prettyPrint def expr1
  prettyPrint (def { configMaxLineWidth = 20 }) expr2
  -- prettyPrint def' (configMaxLineWidth .= 20) expr2
