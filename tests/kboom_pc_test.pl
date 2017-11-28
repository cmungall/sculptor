:- begin_tests(kboom).

:- use_module(library(sculptor/kboom)).

:- debug(index).
:- debug(kboom).
:- debug(search).
:- debug(xsearch).

test(kboom) :-
        kboom:set_setting(workdir,'target/pc'),
        kboom:set_setting(search_cutoff, 100),
        kboom:set_setting(ontology,'tests/data/pc.ttl'),
        run_kboom,
        nl.


:- end_tests(kboom).


