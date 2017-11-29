:- begin_tests(kboom).

:- use_module(library(sculptor/kboom)).

:- nodebug.
%:- debug(index).
%:- debug(kboom).
%:- debug(search).
%:- debug(xsearch).
%:- debug(xlearn).
%:- debug(query).
%:- debug(xquery).

test(kboom) :-
        kboom:set_setting(workdir,'target/simple'),
        kboom:set_setting(ontology,'tests/data/lexmap_test.ttl'),
        kboom:set_setting(default_axiom_probability, 0.75),
        kboom:set_setting(search_cutoff, 5000),
        run_kboom,
        nl.


:- end_tests(kboom).


