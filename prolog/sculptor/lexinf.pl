:- module(lexinf,
          [ann/3,
           ann/4,
           ann/6,

           lmatch/2,
           lmatch/8,

           index_inferences/0,

           predict_relation/3,
           predict_relation/4,
           training_instance/3,

           suggest_hypothetical_axiom/1,
           suggest_hypothetical_axiom/2,

           index_ann/0,
           index_ann/1,
           index_lmatch/0,
           index_lmatch/1,
           index_inferences/0,
           index_inferences/1,
           index_inferhyp/0,
           index_inferhyp/1

           ]).

:- use_module(library(porter_stem)).
:- use_module(library(semweb/rdf11)).
:- use_module(index_util).

:- use_module(learner).

:- rdf_meta pmap(-,r).
:- rdf_register_ns(oio,'http://www.geneontology.org/formats/oboInOwl#').

:- module_transparent index_wrap/2.
index_wrap(Index,none) :-
        !,
        materialize_index(Index).
index_wrap(Index,Dir) :-
        Index=..[N|Args],
        length(Args,Arity),
        concat_atom([Dir,/,N,Arity,'.pro'],File),
        materialize_index_to_file(Index,File).


% TODO: make easier to extend/plugin
pmap(label, rdfs:label).
pmap(related, oio:hasRelatedSynonym).
pmap(exact, oio:hasExactSynonym).
pmap(broad, oio:hasBroadSynonym).
pmap(narrow, oio:hasNarrowSynonym).
     
ann(X,P,V) :-
        ann(X,P,V,_).
ann(X,P,V,T) :-
        pmap(P,P1),
        T=rdf(X,P1,O),
        O=S^^_,
        T,
        atom_string(V,S).

ann(X,P,V2,T,F,V) :-
        ann(X,P,V,T),
        mutate(F,P,V,V2).
ann(X,P,V,T,null,V) :-
        ann(X,P,V,T).

mutate(stem,_,V,V2) :-
        custom_porter_stem(V,V2).
mutate(downcase,_,V,V2) :-
        downcase_atom(V,V2).

custom_porter_stem(T,S) :-
        atom_concat(X,eous,T),
        atom_concat(X,eus,T2),
        !,
        porter_stem(T2,S).
custom_porter_stem(T,S) :-
        porter_stem(T,S).


lmatch(C1,C2,P1,P2,V,T1,T2,F) :-
        ann(C1,P1,V,T1,F,_),
        ann(C2,P2,V,T2,F,_),
        C1\=C2.
lmatch(C1,C2) :- lmatch(C1,C2,_,_,_,_,_,_).

% TODO: use this in training
triple_pv(rdf(S,P,O),AP,AV) :-
        rdf(Ax,owl:annotatedSource,S),
        rdf(Ax,owl:annotatedProperty,P),
        rdf(Ax,owl:annotatedTarget,O),
        rdf(Ax,AP,AV).

class_pair_match(C1,C2,M) :-
        lmatch(C1,C2,P1,P2,_V,_T1,_T2,F),
        M=m(O1,O2,P1,P2,F),
        belongs(C1,O1),
        belongs(C2,O2).

training_instance(Rel,M,C1-C2) :-
        class_pair_match(C1,C2,M),
        rel_det(C1,C2,Rel).

predict_relation(Rel,M,Pr) :-
        predict_relation(Rel,M,Pr,_).
predict_relation(Rel,M,Pr,PVal) :-
        setof(instance(I,M,Rel),training_instance(Rel,M,I),Insts),
        learn(Rel,M,Insts,Pr,PVal),
        debug(xlearn,'~w <- ~w Pr=~w P-val=~w',[Rel,M,Pr,PVal]).


%! class_pair_rel_prob(?C1, ?C2, ?Rel, ?Pr:number) is nondet
%
% estimate probability for a class pair
suggest_hypothetical_axiom(PA) :-
        suggest_hypothetical_axiom(PA,[]).
suggest_hypothetical_axiom(PrFinal-Axiom, _Opts) :-
        % groups by C1,C2 pairs
        setof(M,class_pair_match(C1,C2,M),Ms),
        aggregate(max(Pr),M^(member(M,Ms),predict_relation(Rel,M,Pr)),MaxPr),
        aggregate(min(Pr),M^(member(M,Ms),predict_relation(Rel,M,Pr)),MinPr),
        PrFinal is (MaxPr + MinPr)/2,
        PrFinal > 0.05,
        mk_axiom(Rel,C1,C2,Axiom).

suggest_hypothetical_axiom(Pr-Axiom, Opts) :-
        setof(M,class_pair_match(C1,C2,M),Ms),
        \+ ((member(M,Ms),predict_relation(_,M,_))),
        Rel=equivalentTo,
        option(default_probability(Pr),Opts),
        mk_axiom(Rel,C1,C2,Axiom).

mk_axiom(Rel,C1,C2,Axiom) :-
        Rel\=relatedTo,
        % remove equivalent inverse
        \+ ((Rel=equivalentTo,
             C2@<C1)),
        Axiom =.. [Rel,C1,C2].
        

rel_det(C1,C2,equivalentTo) :- rel(C1,C2,equivalentTo),!.
rel_det(C1,C2,properSubClassOf) :- rel(C1,C2,subClassOf),!.
rel_det(C1,C2,properSuperClassOf) :- rel(C1,C2,superClassOf),!.
rel_det(C1,C2,R) :- rel(C1,C2,R),!.

rel(C1,C2,R) :-
        inf_rel(C1,R,C2).
rel(C1,C2,relatedTo) :-
        inf_rel(C1,equivalentTo,_),
        inf_rel(C2,equivalentTo,_).
xxxrel(C1,C2,relatedTo) :-
        inf_rel(C1,subClassOf,Z),
        inf_rel(C2,subClassOf,Z).

cls(C) :-
        rdf(C, rdf:type, owl:'Class').
%cls(C) :-
%        rdf(C, rdfs:subClassOf, _).
        
inf_rel(X,R,Y) :-
        cls(X),
        inf([equivalentTo-X],[],L),
        %writeln(inf(X=L)),
        member(R-Y,L).

inf([],L,L).
inf([R1-H|T],Vs,Out) :-
        (   setof(R-P, adj(R1,H,Vs,R,P), Next)
        ->  true
        ;   Next=[]),
        !,
        ord_union(Next,T,Next2),
        ord_union(Next,Vs,Vs2),
        inf(Next2,Vs2,Out).
xxxinf([_|T],Vs,Out) :-
        !,
        inf(T,Vs,Out).

adj(R1,H,Vs,R,P) :-
        owl_rel(H,R2,P),
        compose_rel(R1,R2,R),
        \+member(R-P,Vs).


compose_rel(equivalentTo,equivalentTo,equivalentTo) :- !.
compose_rel(subClassOf,_,subClassOf) :- !.
compose_rel(_,subClassOf,subClassOf) :- !.
compose_rel(R1,R2,_) :- throw(compose(R1,R2)).



owl_rel(A,subClassOf,B) :- rdf(A, rdfs:subClassOf, B).
owl_rel(A,equivalentTo,B) :- rdf(A, owl:equivalentClass, B).
owl_rel(A,equivalentTo,B) :- rdf(B, owl:equivalentClass, A).




% TODO: adapt for other ontology URI forms
belongs(C,O) :-
        concat_atom([O,_],'_',C),
        !.
belongs(_,global).



        
index_ann :-
        index_ann(none).
index_ann(Dir) :-
        index_wrap(ann(+,-,+,-,-,-),Dir),
        index_wrap(ann(+,-,+,-),Dir),
        index_wrap(ann(+,-,+),Dir).

    
%:- module_transparent index_inferences/1.

index_inferences :-
        index_inferences(none).
index_inferences(Dir):-
        %materialize_index(cls(+)),
        index_wrap(cls(+),Dir),
        index_wrap(inf_rel(+,-,+),Dir).

index_lmatch :-
        index_lmatch(none).
index_lmatch(Dir) :-
        index_wrap(lmatch(+,+,-,-,-,-,-,-),Dir),
        index_wrap(lmatch(+,+),Dir).

index_inferhyp :-
        index_inferhyp(none).
index_inferhyp(Dir) :-
        index_wrap(suggest_hypothetical_axiom(-),Dir).
