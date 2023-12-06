:- op(20,xfy,?=).

% Prédicats d'affichage fournis

% set_echo: ce prédicat active l'affichage par le prédicat echo
set_echo :- assert(echo_on).

% clr_echo: ce prédicat inhibe l'affichage par le prédicat echo
clr_echo :- retractall(echo_on).

% echo(T): si le flag echo_on est positionné, echo(T) affiche le terme T
%          sinon, echo(T) réussit simplement en ne faisant rien.

echo(T) :- echo_on, !, write(T).
echo(_).

% ****************

% Term =.. List
% Exemple : f(a,b) =.. [f,a,b]

% E: Equation en cours.
% Es:list d'équation à traiter

% ****** unifie ***********
unifie(Equations) :-
    term_variables(Equations, Variables),
    unifie(Equations, Variables).

unifie([], Variables) :-
    print_val(Variables).
unifie([E|Es], Variables) :-
    echo('system: '), echo([E|Es]), nl,
    regle(E, R),
    echo(R), echo(': '), echo(E), nl,
    reduit(R, E, [E|Es], Q),
    unifie(Q, Variables).

print_val([]).
print_val([V|Vs]) :-
    echo(V), echo(' = '), write(V), echo('\n'),
    print_val(Vs).

% ***** regle *******
regle(X ?= Y, rename) :- var(X), var(Y).
% constant : atomic
regle(X ?= Y, simplify) :- var(X), atomic(Y).
regle(X ?= Y, expand) :- var(X), compound(Y), \+ occur_check(X, Y).
regle(X ?= Y, check) :- var(X), nonvar(Y), occur_check(X, Y).
regle(X ?= Y, orient) :- nonvar(X), var(Y).
regle(X ?= Y, decompose) :- compound(X), compound(Y), X =.. [F|ArgsX],
    Y =.. [F|ArgsY], length(ArgsX, N), length(ArgsY, N).
% regle(X ?= Y, decompose) :- compound(X), compound(Y), functor(X, F, N), functor(Y, F, N).
regle(X ?= Y, clash) :- compound(X), compound(Y), (functor(X, Fx, Nx), functor(Y, Fy, Ny), (Fx \= Fy; Nx \= Ny)).

% ********* autre regle ********
regle(X ?= Y, success) :- nonvar(X), nonvar(Y), X == Y.


% ***** occur_check *******
occur_check(V, T) :-
    var(T), V == T.
occur_check(V, T) :-
    nonvar(T), T =.. [_|Args], member(Arg, Args), occur_check(V, Arg).

% ***** reduit *******
reduit(rename, X ?= Y, P, Q) :-     % var(X), var(Y), X \== Y, !,
    select(X ?= Y, P, RestP),
    substitute(X, Y, RestP, Q).
reduit(simplify, X ?= Y, P, Q) :-   % var(X), nonvar(Y), !,
    select(X ?= Y, P, RestP),
    substitute(X, Y, RestP, Q).
reduit(expand, X ?= Y, P, Q) :-
    select(X ?= Y, P, RestP),
    substitute(X, Y, RestP, Q).
reduit(check, X ?= Y, _, _) :-
    occur_check(X, Y), !, fail.
reduit(orient, X ?= Y, P, Q) :-
    select(X ?= Y, P, RestP),
    append(RestP, [Y ?= X], Q).
reduit(decompose, X ?= Y, P, Q) :-
    X =.. [F|ArgsX], Y =.. [F|ArgsY],
    decompose(ArgsX, ArgsY, Decomposed),
    select(X ?= Y, P, RestP),
    append(Decomposed, RestP, Q).
reduit(clash, X ?= Y, _, _) :-
    compound(X), compound(Y),
    functor(X, Fx, Nx), functor(Y, Fy, Ny),
    (Fx \= Fy; Nx \= Ny), !, fail.

% ********* autre reduit ********
reduit(success, E, P, Q) :-
    select(E, P, Q).

% ******** decompose ************
decompose([], [], []).
decompose([X|Xs], [Y|Ys], [X ?= Y|Rest]) :- decompose(Xs, Ys, Rest).


% ******** substitute ************
substitute(_, _, [], []).
substitute(X, Y, [E|Es], [SubstitutedE|SubstitutedEs]) :-
    substituteInEquation(X, Y, E, SubstitutedE),
    substitute(X, Y, Es, SubstitutedEs).

substituteInEquation(X, Y, A ?= B, SubstitutedA ?= SubstitutedB) :-
    substituteInTerm(X, Y, A, SubstitutedA),
    substituteInTerm(X, Y, B, SubstitutedB).

substituteInTerm(X, Y, Term, Y) :-
    X == Term, !.
substituteInTerm(X, Y, Term, SubstitutedTerm) :-
    compound(Term), !,
    Term =.. [F|Args],
    substituteInTerms(X, Y, Args, SubstitutedArgs),
    SubstitutedTerm =.. [F|SubstitutedArgs].
substituteInTerm(_, _, Term, Term).

substituteInTerms(_, _, [], []).
substituteInTerms(X, Y, [T|Ts], [SubstitutedT|SubstitutedTs]) :-
    substituteInTerm(X, Y, T, SubstitutedT),
    substituteInTerms(X, Y, Ts, SubstitutedTs).
