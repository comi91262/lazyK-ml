# LazyK-ml
Lazy K (Combinator-calculus style) interpreter in SML# 

# Requirements
* SML# (2.0.0)

# Syntax

~~~
Program  ::= Expr                    Expr
Expr     ::= Expr Expr'　　　　　　　(Expr Expr')
             | epsilon               (\x.x)
Expr'    ::= "I"                     (\x.x)
           | "K"                     (\xy.x)
           | "S" 		     (\xyz.x z (y z))
           | "(" Expr ")"            Expr
~~~
