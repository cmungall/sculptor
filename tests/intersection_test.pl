:- begin_tests(intersection).

:- use_module(library(sculptor/rl_reasoner)).

:- debug(sculptor).


test(ixn) :-
        Init=[
              subClassOf(x,a),
              subClassOf(x,b),
              is_intersection(ab,[a,b])
             ],
        saturate(Init, Out),
        writeln('*EQ*'),
        maplist(writeln,Out),
        member(subClassOf(x,ab),Out),
        member(subClassOf(ab,a),Out),
        nl.
    
:- end_tests(intersection).
    
