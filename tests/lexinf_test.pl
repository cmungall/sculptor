:- begin_tests(lexinf).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).
:- use_module(library(sculptor/ptriple_loader)).
:- use_module(library(sculptor/lexinf)).

:- debug(index).

showall(G) :-
        forall(G,
               format('~q.~n',[G])).

test(load) :-
        kb_from_rdf_file('tests/data/lexmap_test.ttl',Kb),
        %index_ann('target'),
        index_ann,
        index_lmatch,
        showall(ann(_,_,_,_,_,_)),
        functor(G2,lmatch,8),
        showall(G2),
        showall(lmatch(A,B)),
        assertion( lmatch('http://purl.obolibrary.org/obo/A_1','http://purl.obolibrary.org/obo/B_1') ),
        nl.

:- end_tests(lexinf).
    
