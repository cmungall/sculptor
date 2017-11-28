:- begin_tests(weights).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).

:- debug(search).

test(weights) :-
        Init=[
              ],
        H=[
           w(3)-subClassOf(b,a),
           w(8)-subClassOf(b,a),
           w(5)-subClassOf(a,b),
           w(2)-subClassOf(c,b),
           w(-2)-subClassOf(b,c)
          ],
        search(Init, H, Sols),
        writeln('*BASIC4*'),
        length(Sols,Len),
        writeln(Len),
        maplist(write_solution,Sols),
        assertion( Len == 16 ),
        Sols=[Best|_],
        kb_S(Best,S),
        member(_-subClassOf(b,a),S),
        % TODO
        nl.


                    

:- end_tests(weights).
    
