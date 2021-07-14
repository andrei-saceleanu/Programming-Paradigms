type Dict=[(String,Integer)]


valueOf::String->Dict->Integer
valueOf x [] = -1
valueOf x (f:r) = if x==(fst f) then (snd f) else (valueOf x r)


ins::String->Integer->Dict->Dict
ins key value [] = [(key,value)]
ins key value (f:r) = if key==(fst f) then (key,value):r else f:(ins key value r)

data PExpr = Val Integer | Var String | PExpr :+: PExpr
show_pexpr::PExpr->String
show_pexpr (Val x) = show x
show_pexpr (Var x) = x
show_pexpr (l:+:r)="("++(show_pexpr l)++"+"++(show_pexpr r)++")"


eval_pexpr::Dict->PExpr->Integer
eval_pexpr d (Val x) = x
eval_pexpr d (Var x) = valueOf x d
eval_pexpr d (l:+:r) = (eval_pexpr d l)+(eval_pexpr d r)

data BExpr = PExpr :==: PExpr | 
            PExpr :<: PExpr |
            Not BExpr |
            BExpr :&&: BExpr 
 
show_bexpr :: BExpr -> String
show_bexpr (a:==:b) = "("++(show_pexpr a)++"=="++(show_pexpr b)++")"
show_bexpr (a:<:b) = "("++(show_pexpr a)++"<"++(show_pexpr b)++")"
show_bexpr (a:&&:b) = "("++(show_bexpr a)++"&&"++(show_bexpr b)++")"
show_bexpr (Not expr) = "~"++(show_bexpr expr)

instance Show PExpr where
    show = show_pexpr
 
instance Show BExpr where
    show = show_bexpr 

eval_bexpr :: Dict -> BExpr -> Bool
eval_bexpr d (a:==:b) = (eval_pexpr d a)==(eval_pexpr d b)
eval_bexpr d (a:<:b) = (eval_pexpr d a)<(eval_pexpr d b)
eval_bexpr d (a:&&:b) = (eval_bexpr d a)&&(eval_bexpr d b)
eval_bexpr d (Not expr) = not (eval_bexpr d expr)













