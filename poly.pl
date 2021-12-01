/*
Projet de Maxence Raux

Toutes les fonctionnalités demandées sont implémentées.
Pour bien effectuer les differentes opérations, un appel à la méthode "simplifier" est effectué en début d'opération
Cela a cependant un impact sur l'execution de l'opération.
Par exemple : 
    L'appel somme([[1,1]], [[1,1]], R) fonctionnera correctement
    Mais l'appel somme([[1,1]], B, [[2,1]]) echouera.

Quelques exemples d'applications:
    afficher([[-1,0],[2,1],[3,2],[3,1]]).
    P est simp [[-1,0],[2,1],[3,2],[3,1]].
    P est [ [1, 0] , [-1,1] , [0, 2] , [2,3] ] + [ [-1, 0] , [2,1] , [3, 2] ].
    P est [ [1, 0] , [-1,1] , [0, 2] , [2,3] ] - [ [-1, 0] , [2,1] , [3, 2] ].
    P est [ [1, 0] , [-1,1] , [0, 2] , [2,3] ] * [ [-1, 0] , [2,1] , [3, 2] ].
    P est deri [[-1,0],[2,1],[3,2]].

*/


%%%%% Fonction d'affichage %%%%%
afficherMonome([0, _]):-!.
afficherMonome([V, P]):-
    P > 0,
    afficherCoeff(V),
    afficherX(P),!.

afficherMonome([V, 0]):-
    print(V),!.

afficherCoeff(1):-!.
afficherCoeff(-1):-
    print(-),!.
afficherCoeff(V):-
    print(V).

afficher(Poly):-
    simplifier(Poly, Simpl),
    afficherPoly(Simpl),!.


afficherX(0).
afficherX(1):-
    print(x).
afficherX(V):-
    print(x),
    print(^),
    print(V).

afficherPoly([]).
afficherPoly([M]):-
    afficherMonome(M).
afficherPoly([M1, [V, P] | L]):-
    V > 0,!,
    afficherMonome(M1),
    print(+),
    afficher([[V, P] | L]).

afficherPoly([M1, [V, _] | L]):-
    V == 0,!,
    afficherMonome(M1),
    print(+),
    afficher(L).

afficherPoly([M1, [V, P] | L]):-
    V < 0,!,
    afficherMonome(M1),
    afficher([[V, P] | L]).


%%%%% Tri insertion sur les degres d'un polynome %%%%%%
triInser([],[]).
triInser([X|L1],L2):-
    triInser(L1,L3),
    insererTri(X,L3,L2).

insererTri(X,[],[X]).
insererTri([Coeff, Pow],[[Coeff2, Pow2]|L1],[[Coeff2, Pow2]|L2]):-
    Pow<Pow2,
    insererTri([Coeff, Pow],L1,L2).

insererTri([Coeff, Pow],[[Coeff2, Pow2]|L1],[[Coeff, Pow],[Coeff2, Pow2]|L1]):- Pow >= Pow2.

%%%%% Compactage des monomes consecutifs de meme degrés %%%%%
precompacter([[Coeff, Pow] | Poly], Res):-
    compacter([Coeff, Pow], Poly, Res).

compacter(A, [], [A]).

compacter([Coeff, Pow], [[Coeff2, Pow] | Poly], Res):-
    Coeff3 is +(Coeff , Coeff2),
    compacter([Coeff3, Pow], Poly, Res).

compacter([Coeff, Pow], [[Coeff2, Pow2] | Poly], [[Coeff, Pow] | Res]) :-
    Pow \== Pow2,
    compacter([Coeff2, Pow2], Poly, Res).

%%%%% Simplification %%%%%%

retireCoeffNul([], []).
retireCoeffNul([[Coeff, Pow] | Poly], [[Coeff, Pow] | Res]) :-
    Coeff \== 0,
    retireCoeffNul(Poly, Res).
retireCoeffNul([[0, _] | Poly], Res):-
    retireCoeffNul(Poly, Res).

simplifier(Poly, Simpl):-
    triInser(Poly, Tri),
    precompacter(Tri, Comp),!,
    retireCoeffNul(Comp, Simpl).

%%%%% evaluation de polynome %%%%%

evaluerMonome([Coeff, Pow], Val, Res):-
    Res is Coeff * Val ** Pow.

evaluer([], _, 0).
evaluer([Monome | Poly], Val, Res):-
    evaluerMonome(Monome, Val, Res2),
    evaluer(Poly, Val, Res3),
    Res is Res2 + Res3.

%%%%% dérivation de polynome %%%%%

deriveMonome([_, 0], [0, 0]):-!.
deriveMonome([Coeff, Pow], [Coeff2, Pow2]):-
    Pow2 is Pow - 1,
    Coeff2 is Coeff * Pow.

derivePoly([], []).
derivePoly([Monome | Poly], [MonomeD | PolyD]):-
    deriveMonome(Monome, MonomeD),
    derivePoly(Poly, PolyD).

derive(Poly, PolyRes):-
    derivePoly(Poly, PolyDerive),
    simplifier(PolyDerive, PolyRes).

%%%%% somme de polynome %%%%%

somme(Poly1, Poly2, Res):-
    append(Poly1, Poly2, Poly3),
    simplifier(Poly3, Res).

%%%%% soustraction de polynome %%%%%

soustraction(Poly1, Poly2, Res):-
    multiplieMonome([-1, 0], Poly2, Poly3),
    somme(Poly1, Poly3, Res).

%%%%% produit de polynome %%%%%

multiplieMonome(_, [], []).
multiplieMonome([Coeff, Pow], [[Coeff2, Pow2] | Poly], [[Coeff3, Pow3] | Res]):-
    Coeff3 is Coeff * Coeff2,
    Pow3 is Pow + Pow2,
    multiplieMonome([Coeff, Pow], Poly, Res).

produit([[Coeff,Pow]|Poly1],Poly2,Poly) :-
		multiplieMonome([Coeff,Pow],Poly2,Poly3),
		produit(Poly1,Poly2,Poly4),
		somme(Poly3,Poly4,Poly).
produit([],_,[]).

%%%%% detection de polynome %%%%%

polynome([]).
polynome([[_|_]|_]).

%%% PARTIE 2 %%%

%%%%% création des priorités %%%%%

:-op(600,xfy,[*]).
:-op(650,xfy,[+]).
:-op(700,yfx,[-]).
:-op(750,fy,[simp,deri]).
:-op(800,xfx,est).

%%%%% opérateur est %%%%%

est(X,+(A,B)):-
    est(X1, A),
    est(X2, B),
    somme(X1,X2,X),!.
est(X,-(A,B)):-
    est(X1, A),
    est(X2, B),
    soustraction(X1,X2,X),!.
est(X,*(A,B)):-
    est(X1, A),
    est(X2, B),
    produit(X1,X2,X),!.
est(X, simp(A)):-
    est(X1, A),
    simplifier(X1, X),!.
est(X, deri(A)):-
    est(X1, A),
    derive(X1, X),!.
est(X, X):-
    polynome(X).
