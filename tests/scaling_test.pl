:- begin_tests(scaling).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).

%:- debug(search).

mk_ax(equivalentTo(X,Y),X,Y).
mk_ax(subClassOf(X,Y),X,Y).

% for N entities, make N^2 subClass axioms and N^2 equivalentTo axioms
% and assign random probabilities to each
simrun(N, Len) :-
        Init=[
             ],
        setof(X,between(1,N,X),Cs),
        findall(P-Ax,
                (   member(X,Cs),
                    member(Y,Cs),
                    mk_ax(Ax,X,Y),
                    random(P)),
                H),
        search(Init, H, Sols, [max(5000)]),
        length(Sols, Len),
        best(Sols,Kb),
        kb_P(Kb,Pr),
        format('~n[[ SOLS N=~w NUM=~w MPE=~w~n',[N,Len,Pr]),
        maplist(write_solution,Sols),
        format('~n   SOLS N=~w NUM=~w MPE=~w~n]]~n~n',[N,Len,Pr]).



test(n2) :-
        simrun(2,_).

test(n3) :-
        simrun(3,_).

test(n4) :-
        simrun(4,_).

test(n5) :-
        simrun(5,_).


:- end_tests(scaling).
    
