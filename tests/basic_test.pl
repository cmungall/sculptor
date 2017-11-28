:- begin_tests(basic).

:- use_module(library(settings)).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).
:- use_module(library(sculptor/dot_utils)).

:- debug(search).
:- debug(xsearch).

render(Kb,N) :-
        concat_atom(['target/',N,'.dot'],File),
        render_kb(Kb,File,[format(dot)]).



user:prolog_exception_hook(Exception, Exception, Frame, _):-
    (   Exception = error(Term)
    ;   Exception = error(Term, _)),
    get_prolog_backtrace(Frame, 20, Trace),
    format(user_error, 'Error: ~p', [Term]), nl(user_error),
    print_prolog_backtrace(user_error, Trace), nl(user_error), fail.

test(trivial) :-
        Init=[
              ],
        H=[
           0.8-subClassOf(c,b)
          ],
        search(Init, H, Sols),
        writeln('*BASIC*'),
        maplist(write_solution,Sols),
        % we expect 2^1 solutions: {axiom=true, axiom=false}
        assertion( Sols=[_,_] ),
        best(Sols,Best),
        assertion( kb_S(Best,[_-subClassOf(c,b)]) ),
        nl.

% basic test
% logical entailments do not affect choice of solution
test(basic_subclass) :-
        Init=[
              ],
        H=[
           0.8-subClassOf(b,a),
           0.8-subClassOf(c,b)
          ],
        search(Init, H, Sols),
        writeln('*BASIC4*'),
        length(Sols,Len),
        writeln(Len),
        maplist(write_solution,Sols),

        % we expect 2^2 solutions
        % (all choices are coherent)
        assertion(Sols = [_,_,_,_]),

        % we expect the best solution to be 
        % (trivially) the most probably axioms
        best(Sols,Best),
        kb_P(Best,Pr),
        Pr =~ 0.8 * 0.8,
        kb_S(Best,SelectAxioms),
        SelectAxioms=[_,_],
        assertion( member(_-subClassOf(b,a),SelectedAxioms) ),
        assertion( member(_-subClassOf(c,b),SelectedAxioms) ),

        kb_A(Best,As),
        assertion( member(subClassOf(c,a),As) ),
        nl.

/*

  hypothetical axioms:

  (high probs)

  c -> b -> a 
  |         ^
  |         |
  +---------+
  
  (v low prob)

  it's impossible for the top path to be true and the
  bottom link to be false
  
  even though the top path is a priori likely,
  the v low prob of the entailed lower path means
  this solution is unlikely
  
  
*/
test(subclass_inference) :-
        Init=[
              ],
        H=[
           0.9-subClassOf(b,a),
           0.8-subClassOf(c,b),
           0.05-subClassOf(c,a)
          ],
        search(Init, H, Sols),
        writeln('*BASIC8*'),
        length(Sols,Len),
        maplist(write_solution,Sols),
        % total solution space = 2^3 = 8
        % one of these is incoherent
        assertion( Len = 7 ),
        best(Sols,Best),
        kb_P(Best,Pr),
        assertion(Pr =~ 0.17),
        kb_S(Best,S),
        render(Best,sc),
        assertion( \+ member(_-subClass(c,a),S)),
        nl.


% in this test, the final axiom is entailed by the first 2
test(triangle) :-
        Init=[
              ],
        H=[
           0.9-equivalentTo(a,b),
           0.8-equivalentTo(b,c),
           0.7-equivalentTo(c,a) 
          ],
        writeln('*TRIANGLE*'),
        search(Init, H, Sols),
        writeln('*TRIANGLE*'),
        length(Sols,Len),
        maplist(write_solution,Sols),

        % total search space is 8
        % 3 of these are incoherent
        % i.e. 2 equivs and 1 non-equiv
        assertion( Len = 5 ),

        % test best solution has Pr=0.504
        best(Sols,Kb),
        kb_P(Kb,Prob),
        Prob =~ 0.9 * 0.8 * 0.7,

        % test best solution is a=b,b=c,c=a
        kb_S(Kb,S),
        S=[_,_,_],
        forall(member(_-A,S),
               A\=not(_)),

        % test none of the solutions are incoherent
        forall((member(Kb2,Sols),kb_S(Kb2,S2)),
               \+ ((select(A1,S2,S2r),
                    A1\=not(_),
                    select(A2,S2r,[not(_)]),
                    A2\=not(_)))).


test(conflict) :-
        % deliberately introduce incoherent base axioms
        Init=[
              subClassOf(x,y),
              not(subClassOf(x,y))
              ],
        H=[
           0.8-subClassOf(a,b)
          ],
        search(Init, H, Sols),
        writeln('*CONFLICT*'),
        maplist(write_solution,Sols),
        % we expect no solutions
        assertion( Sols=[] ),
        nl.

test(conflict2) :-
        % deliberately introduce incoherent base axioms
        Init=[
              properSubClassOf(x,y),
              equivalentTo(x,z),
              equivalentTo(y,z)
              ],
        H=[
           0.8-subClassOf(a,b)
          ],
        search(Init, H, Sols),
        writeln('*CONFLICT*'),
        maplist(write_solution,Sols),
        % we expect no solutions
        assertion( Sols=[] ),
        nl.


test(no_merge_uri) :-
        O1C1 = 'http://purl.obolibrary.org/obo/OMIM_1',
        O1C2 = 'http://purl.obolibrary.org/obo/OMIM_2',
        O2C1 = 'http://purl.obolibrary.org/obo/Orphanet_1',
        O2C2 = 'http://purl.obolibrary.org/obo/Orphanet_2',
        Init=[
              subClassOf(O1C1,O1C2),
              subClassOf(O2C1,O2C2)
              ],
        H=[
           % 'rungs'
           0.7-equivalentTo(O1C1,O2C1),
           0.7-equivalentTo(O1C2,O2C2),

           % deliberately introce fake clique-collapsing entry
           0.8-equivalentTo(O1C1,O2C2)
          ],
        set_setting(rl_reasoner:is_obo_style,true),
        search(Init, H, Sols),
        writeln('*NO_MERGE_URI*'),
        maplist(write_solution,Sols),
        length(Sols,NumSols),
        assertion( NumSols=5 ),
        best(Sols,Best),
        kb_P(Best,Pr),
        assertion(Pr =~ 0.098),
        kb_S(Best,S),
        assertion( member(_-not(equivalentTo(O1C1,O2C2)),S) ),
        nl.

% TODO: add flag to detect if any member of H is entailed by A                    
test(h_entailed) :-
        Init=[
              not(subClassOf(x,y))
              ],
        H=[
           0.8-subClassOf(x,y)
          ],
        search(Init, H, Sols),
        writeln('*H_ENTAILED*'),
        maplist(write_solution,Sols),
        assertion( Sols=[_] ),
        best(Sols,Best),
        kb_S(Best,[_-not(subClassOf(x,y))]),
        nl.

test(h_entailed2) :-
        Init=[
              subClassOf(x,y)
              ],
        H=[
           0.8-subClassOf(x,y)
          ],
        search(Init, H, Sols),
        writeln('*H_ENTAILED2*'),
        maplist(write_solution,Sols),
        assertion( Sols=[_] ),
        best(Sols,Best),
        kb_S(Best,[_-subClassOf(x,y)]),
        nl.

:- end_tests(basic).
    
