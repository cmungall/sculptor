:- begin_tests(kboom).

:- use_module(library(sculptor/kboom)).

:- debug(index).
:- debug(kboom).
:- debug(search).

test(kboom) :-
        kboom:set_setting(workdir,'target/recap'),
        kboom:set_setting(search_cutoff, 100),
        kboom:set_setting(recapitulate, [equivalentTo]),
        kboom:set_setting(ontology,'tests/data/pc.ttl'),
        run_kboom,
        nl.


:- end_tests(kboom).


