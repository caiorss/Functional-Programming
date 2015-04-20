{-
--
--  http://queue.acm.org/detail.cfm?id=2617811  
--
-- :set -XGADTsc
--


λ > let abstree = 1 + 2 * 3 :: Expr 
λ > abs
abs      abstree
λ > abstree 
Add (Lit 1) (Mul (Lit 2) (Lit 3))
λ > 
λ > run abs
abs      abstree
λ > run abstree 
7
λ > 

λ > show(f 4)
"Add (Lit 4) (Lit 1)"
λ > let app = show(f 4)

λ > read app :: Expr
Add (Lit 4) (Lit 1)
λ > 


-}

data Expr where
    Lit :: Integer -> Expr
    Add :: Expr -> Expr -> Expr
    Sub :: Expr -> Expr -> Expr
    Mul :: Expr -> Expr -> Expr
    Var :: String -> Expr -- new constructor 
    deriving (Eq, Show, Read)

instance Num Expr where
 fromInteger n = Lit n
 e1 + e2 = Add e1 e2
 e1 - e2 = Sub e1 e2
 e1 * e2 = Mul e1 e2 
 
 
run :: Expr -> Integer
run (Lit n) = n
run (Add a b) = run a + run b
run (Sub a b) = run a - run b
run (Mul a b) = run a * run b 

f :: Expr -> Expr
f x = x + 1 

g :: Expr -> Expr -> Expr
g x y = x * x + y + 2 
