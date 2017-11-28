:- begin_tests(pato).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).
:- use_module(library(sculptor/rl_reasoner)).
:- use_module(library(sculptor/ptriple_loader)).

test(load) :-
        kb_from_rdf_file('tests/data/pato.ttl',Kb),
        writeln(Kb),
        kb_A(Kb,Axioms),
        nb_setval(report_chunk_size,2),
        saturate(Axioms, AxiomsOut),
        maplist(writeln,AxiomsOut),
        nl.

:- end_tests(pato).
    
