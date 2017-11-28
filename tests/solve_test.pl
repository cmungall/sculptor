:- begin_tests(solve).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).
:- use_module(library(sculptor/dot_utils)).

:- debug(search).

render(Kb,N) :-
        can_render_image,
        !,
        make_directory_path(target),
        concat_atom(['target/',N,'.png'],File),
        render_kb(Kb,File,[format(png)]).
render(_,_) :-
        debug(dot,'Skipping rendering step',[]).

              
% we assign a prior probability of 
% a1=b1 if they are in the same ontology
test(nomerge_simple) :-
        Init=[
              in(a1,s1),
              in(b1,s1),
              all_unique(s1)
              ],
        H=[
           0.99-equivalentTo(a1,b1)
          ],
        search(Init, H, Sols),
        writeln('*NM_SIMPLE*'),
        maplist(write_solution,Sols),
        
        % we expect one solution
        assertion( Sols=[_] ),
        Sols=[Sol],
        
        % the equivalence solution is ruled out due to all-unique constraint
        assertion( kb_S(Sol,[_-not(equivalentTo(a1,b1))])).

/*
 more complex no-merge test

  d1->c1->b1->a1   (group s1)
  .   .   .   .  
  .   .   .   .  
  .   .   .   .  
  d2->c2->b2->a2   (group s2)

  dotted lines indicate potential equivalence
  not shown: additional high prob potential equiv a1..d2

  despite the high prob of this 'crossing' axiom it should
  nnot be chosen in the best solution as it leads to
  excessive clique merging, and we a-priori forbid
  in-group merging
  
*/
test(nomerge_inf) :-
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
              subClassOf(d2,c2)
              ],
        H=[
           0.7-equivalentTo(a1,a2),
           0.8-equivalentTo(b1,b2),
           0.6-equivalentTo(c1,c2),
           0.9-subClassOf(a1,d2)
          ],
        search(Init, H, Sols),
        writeln('*NM_INF*'),
        maplist(write_solution,Sols),
        best(Sols, Best),
        render(Best,nm),
        kb_P(Best,Pr),
        assertion( Pr =~ 0.034 ),
        assertion( (kb_S(Best, BestAxioms),
                    member(_-not(subClassOf(a1,d2)), BestAxioms)) ),
        writeln(Best).


test(filter) :-
        Init=[
              subClassOf(b,a),
              subClassOf(c,b),
              % gap
              subClassOf(e,d),
              subClassOf(f,e)
             ],
        H=[
           0.99-subClassOf(d,c),
           0.05-subClassOf(f,a)
          ],
        search(Init, H, Sols),
        writeln('*FILTER*'),
        maplist(write_solution,Sols),
        assertion( Sols=[_,_,_] ),
        nl.




:- end_tests(solve).
    
