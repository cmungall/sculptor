:- begin_tests(trig).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).
:- use_module(library(sculptor/ptriple_loader)).

:- debug(rdf).

% tests loading of trig format ontology
test(load) :-
        kb_from_rdf_file('tests/data/simple.trig',Kb),
        writeln(Kb),
        kb_search(Kb, Sols, [una(local)]),
        writeln('*FROMOWL*'),
        length(Sols,Len),
        maplist(write_solution,Sols),
        assertion( Len = 3 ),
        Sols=[Best|_],
        kb_S(Best,L),
        L=[_,_],
        assertion( member(_-equivalentTo('http://example.org/a',
                                         'http://example.org/a2'),L)),
        assertion( member(_-not(equivalentTo('http://example.org/a',
                                             'http://example.org/b2')),L)),
        nl.

:- end_tests(trig).
    
