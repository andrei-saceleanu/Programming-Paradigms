subsets([],[]).
subsets([H|T],[H|R]) :- subsets(T,R).
subsets([_|T],R) :- subsets(T,R).

sublist(L,R) :- append(_,R,Rp),append(Rp,_,L).


prefix(P,L) :- append(P,_,L).
sufix(S,L) :- append(_,S,L).
substring(L,R) :- prefix(P,R),sufix(L,P).

ksubsets([],0,[]).
ksubsets([H|T],K,R) :- K1 is K-1,ksubsets(T,K1,R1),R = [H|R1].
ksubsets([_|T],K,R) :- ksubsets(T,K,R).


ksubsetsp(L,K,R) :- length(Lp,K),subsets(L,Lp),R=Lp.

repeat(_,0,[]).
repeat(X,K,[X|R1]) :- K1 is K-1,repeat(X,K1,R1),!.

oddOnly([],[]).
oddOnly([H|T],[H|R1]) :- oddOnly(T,R1),mod(H,2) =\= 0.
oddOnly([H|T],R1) :- oddOnly(T,R1),mod(H,2) =:= 0. 

eqelem([]).
eqelem([H|T]) :- eqelem(T),not((member(X,T),H =\= X)).

eqelem2(L) :- not((member(X,L),member(Y,L),X =\= Y)).

pal(X) :- append(_,S,X),reverse(S,R),append(R,S,X).

endsWith(X,[X]).
endsWith(X,[_|T]) :- endsWith(X,T).

sameFirstLast([_]).
sameFirstLast([H|T]) :- endsWith(X,T),H =:= X.

inner(L1,L2) :- subsets(L2,X),X = L1.

prefixk(P,K,L) :- prefix(P,L),length(P,K).

find(L,1,L).
find(L,0,L1) :- length(L,X),length(L1,X1),X=X1,L \= L1.
find(L,K,[H|T]) :- length(L,X),prefixk(P,X,[H|T]),find(L,K1,T),P = L,K is K1+1.
find(L,K1,[H|T]) :- length(L,X),prefixk(P,X,[H|T]),find(L,K1,T),P \= L.

graph([[1,2,3,4,5],[[1,2],[2,3],[2,4],[3,4],[4,5]]]).
%graph([[1,2,3],[[1,2],[2,3],[1,3]]]).

edge(X,Y,[_,E]) :- member([X,Y],E) ; member([Y,X],E).

isolated(X,[V,E]) :- not((edge(A,B,[V,E]),(X = A;X = B))).


rem(_,[],[]).
rem(X,[H|T],[H|R]) :- X \= H , rem(X,T,R).
rem(X,[H|T],R) :- X = H,rem(X,T,R).

pairs(_,[],[]).
pairs(X,[[H1,H2]|T],[H2|R1]) :- pairs(X,T,R1),X = H1.
pairs(X,[[H1,H2]|T],[H1|R1]) :- pairs(X,T,R1),X = H2.
pairs(X,[[H1,H2]|T],R1) :- pairs(X,T,R1),X \= H1,X \= H2.

neighbors(X,L,[_,E]) :- pairs(X,E,L).

%neighbors(_,[],[_,[]]).
%neighbors(X,L,[_,E]) :- edge(X,Y,[_,E]),rem([X,Y],E,E1),rem([Y,X],E1,E2),neighbors(X,L1,[_,E2]),L = [Y|L1].

connected(X,Y,_,G) :- edge(X,Y,G).
connected(X,Y,V,G) :- edge(X,Z,G),not(member(Z,V)),connected(Z,Y,[Z|V],G).

path(X,Y,G,P) :- path(X,Y,[X],G,P).
path(Y,Y,_,_,[Y]).
path(X,Y,V,G,P) :- edge(X,Z,G),not(member(Z,V)),path(Z,Y,[Z|V],G,P1),P = [X|P1].

dfs(S,G,R) :- dfs(S,[S],G,R).
dfs(S,V,G,[S]) :- not((edge(S,Z,G),not(member(Z,V)))). 
dfs(S,V,G,R) :- edge(S,Z,G),not(member(Z,V)),dfs(Z,[Z|V],G,R1),R = [S|R1].

%setof(Y,connected(X,Y,[X],G),L).
%connected(X,Y,[X],G),Y\=X,append([Y],_,L).

accesible(X,L,G) :- accesible(X,[],L,G). 
accesible(X,Acc,L,G) :- connected(X,Y,[X],G),\+ member(Y,Acc), !,accesible(X,[Y|Acc],L,G).
accesible(_,L,L,_).

complete([V,E]) :- not((member(X,V),member(Y,V),X\=Y,not(edge(X,Y,[V,E])))).

%complement([V,E],[V,E1]) :- member(X,V),member(Y,V),X\=Y,not(edge(X,Y,[V,E])),append([X,Y],_,E1).
%complement([V,E],[V,E1]) :- setof(C,(member(X,V),member(Y,V),X\=Y,not(edge(X,Y,[V,E])),C = [X,Y]),E1).
complement(G,G1) :- complement(G,[],G1).
complement([V,E],Acc,[V,E1]) :- member(X,V),member(Y,V),X<Y,\+ edge(X,Y,[V,E]),\+ member([X,Y],Acc),!,complement([V,E],[[X,Y]|Acc],[V,E1]).
complement([_,_],E1,[_,E1]).

disjoint(L,L1) :- not((member(X,L),member(X,L1))).

%partition(L,X,Y) :- subsets(L,X),subsets(L,Y),disjoint(X,Y),X\=[],Y\=[],sum_list(X,A),sum_list(Y,B),A =:= B.
partition(S, [ItemL|L], [ItemR|R]):-
  partition1(S, [ItemL|L], [ItemR|R]),
  sumlist([ItemL|L], Sum),
  sumlist([ItemR|R], Sum).

partition1([], [], []).
partition1([Item|S], [Item|L], R):-
  partition1(S, L, R).
partition1([Item|S], L, [Item|R]):-
  partition1(S, L, R).


klist(1,[1]) :- !.
klist(K,R) :- K1 is K-1,klist(K1,R1),append(R1,[K],R).

coloring([],_,[]).
coloring([H|T],K,R) :- klist(K,L),coloring(T,K,R1),member(X,L),R = [[H,X]|R1].

%get_pair(_,[],-1).
get_pair(X,[[H1,H2]|_],H2) :- X = H1.
%get_pair(X,[[H1,H2]|_],H1) :- X = H2.
get_pair(X,[[H1,_]|T],R) :- X \=H1,get_pair(X,T,R).


check_coloring([V,E],C) :- not((edge(X,Y,[V,E]),X<Y,get_pair(X,C,P1),get_pair(Y,C,P2),P1 =:= P2)).
kColor([V,E],K,C) :- coloring(V,K,C),check_coloring([V,E],C).

cartesian(L,L1,R) :- setof([X,Y],(member(X,L),member(Y,L1)),R).

union(L,L1,R) :- setof(X,(member(X,L);member(X,L1)),R).

intersection(L,L1,R) :- setof(X,(member(X,L),member(X,L1)),R).

diff(L,L1,R) :- setof(X,(member(X,L),not(member(X,L1))),R).

pow(S,R) :- setof(X,subsets(S,X),R).

insert_elem(X,[],[X]).
insert_elem(X,[H|T],[X,H|T]).
insert_elem(X,[H|T],[H|R1]) :- insert_elem(X,T,R1).

perm([],[]).
perm([H|T],R) :- perm(T,R1),insert_elem(H,R1,R).


permutations(L,R) :- setof(P,perm(L,P),R).


kvertexcover(C,K,[V,E]) :- ksubsets(V,K,C),not((edge(X,Y,[V,E]),not(member(X,C)),not(member(Y,C)))).

kclique(C,K,[V,E]) :- ksubsets(V,K,C),not((member(X,C),member(Y,C),X\=Y,not(edge(X,Y,[V,E])))).

my_flatten([],[]).
my_flatten([H|T],R) :- my_flatten(T,R1) ,is_list(H),my_flatten(H,H1),append(H1,R1,R).
my_flatten([H|T],R) :- my_flatten(T,R1) ,not(is_list(H)),append([H],R1,R).

compress([],[]).
compress([H|T],Res) :- compress(T,[H],Res).
compress([H|T],Acc,Res) :- last(Acc,X),H\=X,append(Acc,[H],A),!,compress(T,A,Res).
compress([H|T],Acc,Res) :- last(Acc,X),H=X,!,compress(T,Acc,Res).
compress(_,Res,Res).


slice([H|_],A,A,[H]).
slice([H|T],A,B,R) :- A = 0 , B1 is B-1,slice(T,A,B1,R1),R = [H|R1].
slice([_|T],A,B,R) :- A \= 0, A1 is A-1,B1 is B-1,slice(T,A1,B1,R1),R = R1,!.

rotate(X,0,X) :- !.
rotate([H|T],N,R) :- append(T,[H],L),N1 is N-1,rotate(L,N1,R),!.


init_list([_],[]) :- !.
init_list([H|T],[H|R]) :- init_list(T,R),!.

pack([],[]).
pack([H|T],Res) :- pack(T,[[H]],Res).
pack([H|T],Acc,Res) :- last(Acc,X),last(X,E),H\=E,append(Acc,[[H]],A),!,pack(T,A,Res).
pack([H|T],Acc,Res) :- last(Acc,X),last(X,E),H=E,init_list(Acc,I),append(X,[H],R),append(I,[R],A),!,pack(T,A,Res).
pack(_,Res,Res).


%range(A,B,_,[]) :- A > B,!.
%range(A,B,S,[A|R]) :- A <= B, A1 is A+S,range(A1,B,S,R),!.

pair_elem(_,[],[]) :- !.
pair_elem(X,[H|T],R) :- pair_elem(X,T,R1),R = [[X,H]|R1],!.

p(L,R) :- member(X,L),pair_elem(X,L,R).