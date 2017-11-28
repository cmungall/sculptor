:- begin_tests(query).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).

%:- debug(search).

wpq(Q, Init-H) :-
        wpq(Q, Init-H, _).
wpq(Q, Init-H, Expected) :-
        nl,
        format('A=~q~n',[Init]),
        format('H=~q~n',[H]),
        format('Q=~q~n',[Q]),
        pquery(Q,Init,H,Pr),
        format(':: Pr=~q  [Expected=~w]~n',[Pr,Expected]),
        (   nonvar(Expected)
        ->  Diff is abs(Expected-Pr),
            assertion(Diff < 0.01)
        ;   writeln('no_test')).

%:- debug(query).

test(basicq1) :-
        Init=[
              ],
        H=[
           0.8-equivalentTo(a,a2)
          ],
        In = Init-H,
        wpq(equivalentTo(a,a2),In,0.8),
        wpq(not(equivalentTo(a,a2)),In,0.2).

test(booleanq) :-
        Init=[
              ],
        H=[
           0.8-equivalentTo(a,a2),
           0.8-equivalentTo(a2,a3)
          ],
        In = Init-H,
        wpq(equivalentTo(a2,a3),In,0.8),
        wpq(equivalentTo(a,a3),In,0.64),
        wpq(equivalentTo(a,a2),In,0.8),
        wpq(not(equivalentTo(a,a2)),In,0.2),
        wpq( (equivalentTo(a,a2),
              equivalentTo(a2,a3)),
              In,0.64),
        wpq( (equivalentTo(a,a3),
              equivalentTo(a,a2),
              equivalentTo(a2,a3)),
              In,0.64),
        wpq( (equivalentTo(a,a2)
             ;   equivalentTo(a2,a3)),
              In,0.96),
        wpq( not((equivalentTo(a,a2)
                 ;   equivalentTo(a2,a3))),
              In,0.04),
        % TODO
        nl.

test(basicq3) :-
        Init=[
              ],
        H=[
           0.8-equivalentTo(a,a2),
           0.8-equivalentTo(a2,a3),
           0.9-disjointWith(a,a3)
          ],
        In = Init-H,
        wpq(equivalentTo(a,a2),In,0.53),
        wpq(equivalentTo(a2,a3),In,0.53),
        wpq(equivalentTo(a,a3),In,0.15),
        % TODO
        nl.

test(query) :-
        Init=[
              properSubClassOf(a,b),
              properSubClassOf(b,c),
              properSubClassOf(a2,b2),
              properSubClassOf(b2,c2),
              in(a,o1),
              in(b,o1),
              in(c,o1),
              in(a2,o2),
              in(b2,o2),
              in(c2,o2),
              all_unique(o1),
              all_unique(o2)
              ],
        H=[
           0.8-equivalentTo(a,a2),
           0.6-equivalentTo(a,c2),
           0.8-equivalentTo(b,b2)
          ],
        %search(Init,H,Sols),
        In = Init-H,
        wpq(subClassOf(a,c2),In,0.96),
        wpq(equivalentTo(a,a2),In,0.75),
        wpq(not(equivalentTo(a,a2)),In,0.25),
        % proper subClassOf
        wpq((subClassOf(a,c2),
             not(equivalentTo(a,c2))),In,0.90),
        %wpq(subClassOf(a,c2),In,0.96),
        % TODO
        nl.



                    

:- end_tests(query).
    
