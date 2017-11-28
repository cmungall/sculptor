:- module(rl_reasoner,
          [saturate/2,
           abduce/3,
           abduce_all/3]).

:- use_module(library(settings)).

:- setting(is_obo_style,
           boolean,
           true,
           'If true assume OBO-style URIs with the ontology name preceding the "_" and all ontologies following unique ID assumption').

/*

  DEDUCTIVE REASONING

  TODO:

  consider an efficient indexing of axioms list
  
*/

saturate(AxiomsIn, AxiomsOut) :-
        list_to_ord_set(AxiomsIn,AxiomsInSet),
        saturate1(AxiomsInSet, AxiomsOut).

saturate1(AxiomsIn, AxiomsOut) :-
        setof(A,entails(AxiomsIn,A),As),
        !,
        increment_counter(AxiomsIn),
        %append(As, AxiomsIn, AxiomsNext),
        ord_union(As, AxiomsIn, AxiomsNext),
        saturate1(AxiomsNext, AxiomsOut).
saturate1(Axioms, Axioms).

%! entailed(+Axiom, +Axioms:list, ?AxiomEntailed) is nondet
%
% true if AxiomEntailed can be inferred from A+Axioms
entails(AxiomsIn, EntailedAxiom) :-
        rule(EntailedAxiom, [BodyAxiom|BodyAxioms]),
        member(BodyAxiom,AxiomsIn),
        % consider testing if already exists here, if already ground
        all_true(BodyAxioms, AxiomsIn),
        assertion( ground(EntailedAxiom) ),
        \+ memberchk(EntailedAxiom, AxiomsIn).


all_true([],_).
all_true([A|As],AxiomsIn) :-
        ground_axiom(A,AxiomsIn),    % grounds Fact
        all_true(As,AxiomsIn).


% todo: other operations; this is the only native one required so far
ground_axiom(A\=B,_) :- A\=B.
ground_axiom(forall(X,L,A),AxiomsIn) :- !,L=[X|_],ground_axiom(A,AxiomsIn),forall(member(X,L),ground_axiom(A,AxiomsIn)).
%ground_axiom(forall(X,L,A),AxiomsIn) :- !,forall(member(X,L),ground_axiom(A,AxiomsIn)).
%ground_axiom(forall(X,L,A),AxiomsIn) :- !,ground_all_axioms(L, AxiomsIn)
ground_axiom(exists(X,L,A),AxiomsIn) :- !,memberchk(X,L),ground_axiom(A,AxiomsIn).
ground_axiom(A,AxiomsIn) :- member(A,AxiomsIn).
ground_axiom(in(C,O),_) :- setting(is_obo_style,true),!,concat_atom(Toks,'_',C),reverse(Toks,[_,O|_]).
ground_axiom(all_unique(_),_) :- setting(is_obo_style,true),!.


/*
ground_all_axioms([], AxiomsIn).
ground_all_axioms([A|AxiomsUnbound], AxiomsIn) :-
        ground_axiom(A, AxiomsIn),
        ground_all_axioms(AxiomsUnbound, AxiomsIn).
*/
        
        

%! abduce(+AxiomsIn:list, +AxiomsNew:list, ?AxiomExplained:list) is nondet
%
% finds axioms that are explained by AxiomsNew
abduce(AxiomsIn, AxiomsNew, AxiomExplained) :-
        select(AxiomExplained, AxiomsIn, AxiomsRest),
        \+ member(AxiomExplained, AxiomsNew),
        append(AxiomsNew, AxiomsRest, AxiomsIn2),
        saturate(AxiomsIn2, AxiomsOut),
        member(AxiomExplained, AxiomsOut).

abduce_all(AxiomsIn, AxiomsNew, AxiomsExplained) :-
        setof(A,abduce(AxiomsIn, AxiomsNew, A),AxiomsExplained).


increment_counter(L) :-
        nb_current(report_chunk_size,Size),
        !,
        (   nb_current(s,N)
        ->  N2 is N+1
        ;   N2=1),
        nb_setval(s,N2),
        M is N2 mod Size,
        (   M=1
        ->  length(L,Len),
            L=[A|_],
            format('% Iteration: ~w Size=~w Last=~w~n',[N2,Len,A])
        ;   true).
increment_counter(_).



/*

  RULES

  TODO: make these extensible

*/

rule(subClassOf(A,B),[subClassOf(A,Z),subClassOf(Z,B)]). % trans
rule(subClassOf(A,B),[equivalentTo(A,B)]).

rule(equivalentTo(A,A),[class(A)]).
rule(equivalentTo(A,B),[equivalentTo(B,A)]). % symmetry of equiv
rule(equivalentTo(A,B),[subClassOf(A,B),subClassOf(B,A)]). % anti-symm of sc
rule(equivalentTo(A,B),[equivalentTo(A,Z),equivalentTo(Z,B)]). % trans

rule(class(A),[subClassOf(A,_)]).
rule(class(A),[subClassOf(_,A)]).

rule(unsat(A),[subClassOf(A,B),subClassOf(A,C),disjointWith(B,C)]).
rule(unsat(A),[equivalentTo(A,B),in(A,S),in(B,S),A\=B,all_unique(S)]).
rule(unsat(Ax),[not(Ax),Ax]).

% note that we do not infer properSubClassOf
% used for initial assertions only
rule(subClassOf(A,B),[properSubClassOf(A,B)]).
rule(unsat(A),[properSubClassOf(A,B),equivalentTo(A,B)]).


%rule(subClassOf(A,and([X])),[subClassOf(A,X)]).
%rule(subClassOf(A,and([X|Ops])), [subClassOf(A,X),subClassOf(A,and(Ops))]).

rule(subClassOf(A,Ixn), [is_intersection(Ixn,Ops),forall(X,Ops,subClassOf(A,X))]).
rule(subClassOf(Ixn,A), [is_intersection(Ixn,Ops),exists(X,Ops,subClassOf(X,A))]).







        
        
