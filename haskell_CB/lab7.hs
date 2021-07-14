{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

data Extended=Infinity | Value Integer

instance Eq Extended where
	Infinity == Infinity = True
	Value x == Value y = x==y
	n1 /= n2 = not (n1==n2)


data Formula a = Atom a |
                 Or (Formula a) (Formula a) |
                 And (Formula a) (Formula a) |
                 Not (Formula a)

instance Eq a => Eq (Formula a) where
	Atom x == Atom y = x==y
	Or a b == Or c d = a==c && b==d
	And a b == And c d = a==c && b==d
	Not a == Not b = a==b
	_ == _ =False
	f1/=f2 = not (f1==f2)

data Set a = F (a->Bool)


instance (Integral a,Num a) => Num (Set a) where
	(F x) + (F y) = F (\el -> (x el)||(y el))
	(F x) * (F y) = F (\el -> (x el)&&(y el))
	fromInteger x = F (\el -> if ((toInteger el)==x) then True else False)

type Dict=[(String,Integer)]


valueOf::String->Dict->Integer
valueOf x [] = -1
valueOf x (f:r) = if x==(fst f) then (snd f) else (valueOf x r)

ins::String->Integer->Dict->Dict
ins key value [] = [(key,value)]
ins key value (f:r) = if key==(fst f) then (key,value):r else f:(ins key value r)

data PExpr = Val Integer | Var String | PExpr :+: PExpr

eval_pexpr::Dict->PExpr->Integer
eval_pexpr d (Val x) = x
eval_pexpr d (Var x) = valueOf x d
eval_pexpr d (l:+:r) = (eval_pexpr d l)+(eval_pexpr d r)

data BExpr = PExpr :==: PExpr | 
            PExpr :<: PExpr |
            Not2 BExpr |
            BExpr :&&: BExpr 

eval_bexpr :: Dict -> BExpr -> Bool
eval_bexpr d (a:==:b) = (eval_pexpr d a)==(eval_pexpr d b)
eval_bexpr d (a:<:b) = (eval_pexpr d a)<(eval_pexpr d b)
eval_bexpr d (a:&&:b) = (eval_bexpr d a)&&(eval_bexpr d b)
eval_bexpr d (Not2 expr) = not (eval_bexpr d expr)

data Prog = PlusPlus String |       
            String :=: PExpr |     
            DeclareInt String |     
            Begin Prog Prog |     
            While BExpr Prog |     
            If BExpr Prog Prog      
 
eval_prog :: Dict -> Prog -> Dict
eval_prog d (DeclareInt x) = ins x 0 d
eval_prog d (PlusPlus x) = ins x ((valueOf x d)+1) d
eval_prog d (var :=: expr) = ins var (eval_pexpr d expr) d
eval_prog d (If bexpr a b) = if (eval_bexpr d bexpr)==True then eval_prog d a else eval_prog d b
eval_prog d (While bexpr a) = if (eval_bexpr d bexpr)==True then eval_prog d a else d

class Eval a b where
	eval::Dict->a->b

instance Eval PExpr Integer where
	eval=eval_pexpr

instance Eval BExpr Bool where
	eval=eval_bexpr

instance Eval Prog Dict where
	eval=eval_prog

