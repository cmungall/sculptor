:- begin_tests(lexinf_mondo).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).
:- use_module(library(sculptor/ptriple_loader)).
:- use_module(library(sculptor/lexinf)).

:- debug(index).

xxxtest(train) :-
        kb_from_rdf_file('tests/data/mondo.owl',_Kb),
        index_ann,
        index_lmatch,
        G=lmatch(C1,C2,P1,P2,_V,_T1,_T2,F),
        forall(G,
               writeln(G)),
        nl.


test(train) :-
        kb_from_rdf_file('tests/data/mondo.owl',_Kb),
        index_ann,
        index_lmatch,
        index_inferences,
        
        % note: tabling training_instance/3 seems to introduce a bug where this fails
        forall(training_instance(R,_,'http://purl.obolibrary.org/obo/MONDO_0015278'-'http://purl.obolibrary.org/obo/Orphanet_1333'),
               R=equivalentTo),
        
        forall(training_instance(A,B,I),
               writeln(i(A,B,I))),
        writeln('TRAINING'),
        forall(predict_relation(R,M,P,PVal),
               writeln(learn(R,M,P,PVal))),
        nl.


:- end_tests(lexinf_mondo).
    
