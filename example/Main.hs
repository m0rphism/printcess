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

expr0 = EApp (EVar "x") (EVar "y")
expr1 = EAbs "x" $ EAbs "y" $ EApp expr0 expr0
test1 = putStrLn $ pretty expr1

main = putStrLn "Printcess Pretty Printing Example"
