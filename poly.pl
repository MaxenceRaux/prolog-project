afficherMonome([0, _]).
afficherMonome([V, P]):-
    afficherCoeff(V),
    afficherX(P).

afficherCoeff(1).
afficherCoeff(-1):-
    print(-).
afficherCoeff(V):-
    print(V).

afficher(Poly):-
    simplifier(Poly, Simpl),
    afficherPoly(Simpl).


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

afficherPoly([M1, [V, P] | L]):-
    V == 0,!,
    afficherMonome(M1),
    print(+),
    afficher(L).

afficherPoly([M1, [V, P] | L]):-
    V < 0,!,
    afficherMonome(M1),
    afficher([[V, P] | L]).

% parcours 1 tout les elt
% parcours 2 elt de meme degres + somme facteur
% suppression des même facteurs
% trier +gd au +pt


%poly en param, et ordonné par ordre de coeff

% trier les puissances
% compacter
% virer coeff nuls

% Tri insertion :
triInser([],[]).
triInser([X|L1],L2):-
    triInser(L1,L3),
    insererTri(X,L3,L2).

insererTri(X,[],[X]).
insererTri([Coeff, Pow],[[Coeff2, Pow2]|L1],[[Coeff2, Pow2]|L2]):-
    Pow<Pow2,
    insererTri([Coeff, Pow],L1,L2).

insererTri([Coeff, Pow],[[Coeff2, Pow2]|L1],[[Coeff, Pow],[Coeff2, Pow2]|L1]):- Pow >= Pow2.


precompacter([[Coeff, Pow] | Poly], Res):-
    compacter([Coeff, Pow], Poly, Res).

compacter(A, [], [A]).

compacter([Coeff, Pow], [[Coeff2, Pow] | Poly], Res):-
    Coeff3 is +(Coeff , Coeff2),
    compacter([Coeff3, Pow], Poly, Res).

compacter([Coeff, Pow], [[Coeff2, Pow2] | Poly], [[Coeff, Pow] | Res]) :-
    Pow \== Pow2,
    compacter([Coeff2, Pow2], Poly, Res).

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


evaluerMonome([Coeff, Pow], Val, Res):-
    Res is Coeff * Val ** Pow.

evaluer([], _, 0).
evaluer([Monome | Poly], Val, Res):-
    evaluerMonome(Monome, Val, Res2),
    evaluer(Poly, Val, Res3),
    Res is Res2 + Res3.

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


somme(Poly1, Poly2, Res):-
    append(Poly1, Poly2, Poly3),
    simplifier(Poly3, Res).


soustraction(Poly1, Poly2, Res):-
    multiplieMonome([-1, 0], Poly2, Poly3),
    somme(Poly1, Poly3, Res).

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

%%% PARTIE 2 %%%

:-op(700,xfy,[+,-,*]).
:-op(900,fy,[simp,deri]).
:-op(800,xfx,est).

est(X,+(A,B)):-somme(A,B,R),write(R).
est(X,-(A,B)):-soustraction(A,B,R),write(R).
est(X,*(A,B)):-produit(A,B,R),write(R).
simp(X):-simplifier(X,XS),write(XS).
deri(X):-derivative(X,XS),write(XS).