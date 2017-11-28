:- begin_tests(abduction).

:- use_module(library(sculptor/rl_reasoner)).

:- debug(sculptor).

test(subClass) :-
        Init=[
              subClassOf(a,b),
              subClassOf(b,c),
              subClassOf(a2,b2),
              subClassOf(b2,c2)
             ],
        New=[
             equivalentTo(a,a2),
             equivalentTo(b,b2)
             ],
        abduce_all(Init, New, Explained),
        writeln('*ABD*'),
        maplist(writeln,Explained),
        assertion( Explained=[_,_] ),
        nl.
    
:- end_tests(abduction).
    
