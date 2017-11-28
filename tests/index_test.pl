:- begin_tests(index_util).

:- use_module(library(sculptor/index_util)).

foo(X) :- member(X,[1,2,3]).
foo2(X) :- member(X,[1,2,3]).

showall(G) :- forall(G,writeln(G)).


test(index_util) :-
        showall(foo(_)),
        materialize_index(foo(+)),
        showall(foo(_)),
        nl.

test(index_util_cached) :-
        showall(foo2(_)),
        materialize_index_to_file(foo2(+), 'target/foo_cache.pro', [force(true)]),
        showall(foo2(_)),
        nl.


:- end_tests(index_util).


