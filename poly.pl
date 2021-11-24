afficherMonome([0, _]).
afficherMonome([V, P]):-
    afficherCoeff(V),
    afficherX(P).

afficherCoeff(1).
afficherCoeff(-1):-
    print(-).
afficherCoeff(V):-
    print(V).

afficherX(0).
afficherX(1):-
    print(x).
afficherX(V):-
    print(x),
    print(^),
    print(V).

afficher([]).
afficher([M]):-
    afficherMonome(M).
afficher([M1, [V, P] | L]):-
    V > 0,!,
    afficherMonome(M1),
    print(+),
    afficher([[V, P] | L]).

afficher([M1, [V, P] | L]):-
    V == 0,!,
    afficherMonome(M1),
    print(+),
    afficher(L).

afficher([M1, [V, P] | L]):-
    V < 0,!,
    afficherMonome(M1),
    afficher([[V, P] | L]).

% parcours 1 tout les elt
% parcours 2 elt de meme degres + somme facteur
% suppression des même facteurs
% trier +gd au +pt

simplifier().