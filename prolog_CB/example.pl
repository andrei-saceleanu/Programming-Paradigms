len([],0).
len([_|T],R) :- len(T,R1), R is R1 + 1.

contains(E,[E|_]).
contains(E,[H|T]) :- E \= H, contains(E,T).

rev([],R,R).
rev([H|T],Acc,R) :- rev(T,[H|Acc],R). 
reverse(L,R) :- rev(L,[],R).
