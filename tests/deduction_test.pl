:- begin_tests(deduction).

:- use_module(library(sculptor/rl_reasoner)).

:- debug(sculptor).


test(subClass) :-
        Init=[
              subClassOf(b,a),
              subClassOf(c,b),
              subClassOf(d,c),
              subClassOf(d2,c)
             ],
        saturate(Init, Out),
        writeln('*SUBCLASS*'),
        maplist(writeln,Out),
        member(subClassOf(d2,a),Out),
        member(subClassOf(d,a),Out),
        member(subClassOf(d,b),Out),
        member(subClassOf(d,c),Out),
        member(subClassOf(c,a),Out),
        member(subClassOf(b,a),Out).

test(equiv) :-
        Cs=[a,b,c,d],
        Init=[
              subClassOf(b,a),
              subClassOf(c,b),
              subClassOf(d,c),
              subClassOf(a,d)
             ],
        saturate(Init, Out),
        writeln('*Equiv*'),
        maplist(writeln,Out),
        forall(member(X,Cs),
               forall(member(Y,Cs),
                      member(equivalentTo(X,Y),Out))),
        member(subClassOf(d,a),Out),
        member(subClassOf(d,b),Out),
        member(subClassOf(d,c),Out),
        member(subClassOf(c,a),Out),
        member(subClassOf(b,a),Out).

test(disjoint) :-
        Init=[
              subClassOf(b,a),
              subClassOf(c,b),
              subClassOf(d,c),
              subClassOf(d,y),
              subClassOf(y,z),
              disjointWith(a,z)
             ],
        saturate(Init, Out),
        writeln('*DISJOINT*'),
        maplist(writeln,Out),
        member(unsat(d),Out).

test(noshare) :-
        Init=[
              in(a1,s1),
              in(b1,s1),
              in(c1,s1),
              in(d1,s1),
              in(a2,s2),
              in(b2,s2),
              in(c2,s2),
              in(d2,s2),
              all_unique(s1),
              all_unique(s2),
              subClassOf(b1,a1),
              subClassOf(c1,b1),
              subClassOf(d1,c1),
              subClassOf(b2,a2),
              subClassOf(c2,b2),
              subClassOf(d2,c2),
              
              subClassOf(a1,d2),
              subClassOf(a2,d1)
                         
             ],
        saturate(Init, Out),
        writeln('*NOSHARE*'),
        maplist(writeln,Out),
        member(unsat(a1),Out),
        member(unsat(b1),Out),
        member(unsat(c1),Out),
        member(unsat(d1),Out),
        member(unsat(a2),Out),
        member(unsat(b2),Out),
        member(unsat(c2),Out),
        member(unsat(d2),Out).

    
:- end_tests(deduction).
    
