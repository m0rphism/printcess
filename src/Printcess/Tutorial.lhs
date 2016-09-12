> {-# LANGUAGE LambdaCase, UnicodeSyntax #-}
>
> module Printcess.Tutorial where
>
> import Printcess.PrettyPrinting
>
> type Ident = String
>
> data Expr
>   = EVar Ident
>   | EAbs Ident Expr
>   | EApp Expr Expr
>
> instance Pretty Expr where
>   pp = \case
>     EVar i     → pp i
>     EAbs i e   → assocR 0 $ "λ" +> i +> ". " +> R e
>     EApp e1 e2 → assocL 9 $ L e1 +> " " +> R e2
>
> expr0, expr1, expr2 :: Expr
> expr0 = EApp (EVar "x") (EVar "y")
> expr1 = EAbs "x" $ EAbs "y" $ EApp expr0 expr0
> expr2 = foldl EApp expr0 (replicate 20 expr0)
>
> main0 :: IO ()
> main0 = do
>   prettyPrint $ "Printcess Pretty Printing Example"
>   prettyPrint expr1
>   prettyPrint' (defConfig { configMaxLineWidth = 20 }) expr2
>   -- prettyPrint' (configMaxLineWidth .= 20) expr2
