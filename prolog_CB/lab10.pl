% ?- student_info(_,X,Y,1990).
% ?- student_info(_,X,Y,Z),Z <= 1990.
% ?- student_info(I,X,Y,_),student(I,Z),Z<=2005.
% ?- student_info(X,Y,Z,T),student(X,A),A - T <= 18.

%student_info(Id,First_Name,Last_Name,BirthYear)
student_info(9,"John","Mike",1990).
student_info(10,"Andrew","Jackson",1990).
student_info(11,"Itachi","Carter",1991).
student_info(12,"Luna","Ethan",1989).
student_info(13,"Everly","James",1988).
 
%student(Id,AdmissionYear)
student(9,2004).
student(10,2005).
student(11,2006).
student(12,2003).
student(13,2007).
 
%studies(Id,Lecture)
studies(9,aa).
studies(10,pp).
studies(11,lfa).
 
%grade(Id,Lecture,Grade)
grade(10,pp,4).
grade(9,pp,6).
grade(10,aa,4).
grade(10,lfa,4).
grade(11,lfa,10).

under18(I) :- student_info(I,_,_,Z),student(I,A),A-Z =< 18.


%failAtMostTwo(I) :- (grade(I,_,A),A<5) ; (grade(I,X,B),grade(I,Y,C),X \= Y,B < 5,C < 5).

%failOne(I) :- grade(I,X,A),A<5,(\+ grade(I,Y,B);\+ B<5; \+ X\=Y).

failTwo(I) :- grade(I,X,A),grade(I,Y,B),A<5,B<5,X\=Y,!.

failAtLeastOne(I) :- grade(I,_,A),A<5.

failAtLeast3(I) :- grade(I,X,A),grade(I,Y,B),grade(I,Z,C),A < 5,B < 5,C < 5,X\=Y,X\=Z,Y\=Z.

failAtMostTwo(I) :- not((grade(I,X,A),grade(I,Y,B),grade(I,Z,C),A < 5,B < 5,C < 5,X\=Y,X\=Z,Y\=Z)).

%failAtMostTwo(I) :- not(failAtLeast3(I)). %grade(I,X,A),grade(I,Y,B),grade(I,Z,C),A < 5,B < 5,C < 5,((X==Y,X==Z); (X \= Y,Z == X , Z == Y)). 

passOne(L) :- grade(_,L,N),N >= 5.

nat(zero).
nat(X) :- X = succ(Y), nat(Y).

add(X,zero,X).
add(zero,X,X).
add(succ(X),succ(Y),S) :- add(X,Y,S1),S = succ(succ(S1)).

minus(X,zero,X).
minus(succ(X),succ(Y),M) :- minus(X,Y,M) , nat(M).

toNat(0,zero).
toNat(X,succ(Res)) :- X>0, Xp is X-1, toNat(Xp,Res). 
 
fromNat(zero,0).
fromNat(succ(X),R) :- fromNat(X,Rp), R is Rp + 1. 

min(_,zero,zero).
min(zero,_,zero).
min(succ(X),succ(Y),R) :- min(X,Y,R1),R = succ(R1).



max(X,zero,X).
max(zero,X,X).
max(succ(X),succ(Y),R) :- max(X,Y,R1),R = succ(R1).

grt(succ(_),zero).
grt(succ(X),succ(Y)) :- nat(X),nat(Y),grt(X,Y).

gte(succ(_),zero).
gte(zero,zero).
gte(succ(X),succ(Y)) :- nat(X),nat(Y),gte(X,Y).

leq(zero,zero).
leq(zero,succ(_)).
leq(succ(X),succ(Y)) :- nat(X),nat(Y),leq(X,Y).

lt(zero,succ(_)).
lt(succ(X),succ(Y)) :- nat(X),nat(Y),lt(X,Y).

isZero(zero).

div(zero,_,zero).
div(X,Y,R) :- (lt(X,Y),R = zero) ; (minus(X,Y,D),div(D,Y,R1),R = succ(R1)).

mod(X,Y,R) :- (gte(X,Y),minus(X,Y,R1),mod(R1,Y,R)); (R = X) .

gcd(X,Y,R) :- (isZero(Y),R = X) ; (mod(X,Y,M),gcd(Y,M,R)).

isList(void).
isList(cons(_,T)) :- isList(T).

head(void,void).
head(cons(H,_),H).

tail(void,void).
tail(cons(_,T),T).

size(void,0).
size(cons(_,T),R) :- size(T,R1),R is R1+1.

concat(void,X,X).
concat(cons(H,T),L,R) :- concat(T,L,R1),R = cons(H,R1).

rev(void,R,R).
rev(cons(H,T),Acc,R) :- rev(T,cons(H,Acc),R). 
reverse(L,R) :- rev(L,void,R).

fromList(void,[]).
fromList(cons(H,T),[H|R]) :- fromList(T,R).

kelem([H|_],1,H).
kelem([_|T],K,R) :- K > 1,K1 is K-1,kelem(T,K1,R).

rem([],[]).
rem([X],[X]).
rem([H|T],R) :- (rem(T,[H1|R1]),H\=H1,R = [H,H1|R1]) ; (rem(T,[H1|R1]),H = H1,R = [H1|R1]).

toList([],void).
toList([H|R],cons(H,R1)) :- toList(R,R1).

last3([3]).
last3([_|T]) :- last3(T).

last31(L) :- append(_,[3],L).

suffix(L,S) :- append(_,S,L).

sublist([H|T],R) :- append(R,_,[H|T]) ; sublist(T,R).