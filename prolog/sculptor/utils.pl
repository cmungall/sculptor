:- module(utils,
          [write_solution/1,
           solutions/3,
           best/2,
           '=~'/2,
           op(700,xfy,=~)
           ]
         ).

%! write_solution(+Kb)
%
% 
write_solution(kb(Axioms,_,On,Pr,_)) :-
        length(Axioms,Len),
        format('+~w // WORLD=~w // Pr=~w~n',[Len,On,Pr]).

:- op(700,xfy,=~).

%! (=~)/2
%
% approximate equality
% used for comparing floats
N1 =~ N2 :-
        N1x is N1,
        N2x is N2,
        D is abs(N1x-N2x),
        D < 0.01.

best([Kb|_],Kb).

:- module_transparent solutions/3.

%% solutions(+Template,+Goal,-Set)
%   deterministic setof (will not fail). Goal is existentially quantified
solutions(X,Goal,Xs):-
        (   setof(X,Goal^Goal,Xs)
        ->  true
        ;   Xs=[]).
