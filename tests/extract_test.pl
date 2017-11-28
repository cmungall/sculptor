:- begin_tests(extract).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).
:- use_module(library(sculptor/extractor)).

/*


  tests extraction of sub-ontologies from a seed set of nodes

  */

%:- debug(extract).

links2axioms(L,A) :-
        findall(subClassOf(X,Y),member(X-Y,L),A).

test(extract_A) :-
        Links=[
               a-b,
               b-c,
               c-d,
               d-e,
               m-n,
               n-o,
               o-p
              ],
        links2axioms(Links,Axioms),
        
        extract_module([a],Axioms,ModAxioms),
        maplist(writeln,ModAxioms),
        assertion( ModAxioms = [subClassOf(a,b),_,_,_] ),
        
        extract_module([m],Axioms,ModAxioms2),
        maplist(writeln,ModAxioms2),
        assertion( ModAxioms2 = [subClassOf(m,n),_,_] ),

        writeln('**PAIR**'),
        extract_module([a,b],Axioms,ModAxioms3),
        maplist(writeln,ModAxioms3),
        assertion( ModAxioms3 = [subClassOf(a,b),_,_,_] ),

        writeln('**JOIN**'),
        extract_module([b,n],Axioms,ModAxioms4),
        maplist(writeln,ModAxioms4),
        assertion( ModAxioms4 = [subClassOf(b,c),_,_,_,_] ),
        nl.

links2praxioms(L,A) :-
        findall(0.2-subClassOf(X,Y),member(X-Y,L),A).

test(extract_H) :-
        Links=[
               a-b,
               b-c,
               c-d,
               d-e,
               m-n,
               n-o,
               o-p
              ],
        links2praxioms(Links,Axioms),

        extract_module([a],Axioms,ModAxioms),
        maplist(writeln,ModAxioms),
        assertion( ModAxioms = [_-subClassOf(a,b),_,_,_] ),
        
        extract_module([m],Axioms,ModAxioms2),
        maplist(writeln,ModAxioms2),
        assertion( ModAxioms2 = [_-subClassOf(m,n),_,_] ),

        writeln('**PAIR**'),
        extract_module([a,b],Axioms,ModAxioms3),
        maplist(writeln,ModAxioms3),
        assertion( ModAxioms3 = [_-subClassOf(a,b),_,_,_] ),

        writeln('**JOIN**'),
        extract_module([b,n],Axioms,ModAxioms4),
        maplist(writeln,ModAxioms4),
        assertion( ModAxioms4 = [_-subClassOf(b,c),_,_,_,_] ).


test(disjoint) :-
        Links=[
               a-b,
               b-c,
               c-d,
               d-e,
               m-n,
               n-o,
               o-p
              ],
        links2axioms(Links,Axioms1),
        append(Axioms1,
               [disjointWith(a,m),
                disjointWith(b,n)],
               Axioms),
        
        writeln('**D/a**'),
        extract_module([a],Axioms,ModAxioms),
        maplist(writeln,ModAxioms),
        assertion( ModAxioms = [subClassOf(a,b),_,_,_] ),
        assertion( \+ member(disjointWith(_,_), ModAxioms) ),
        
        writeln('**D/m**'),
        extract_module([m],Axioms,ModAxioms2),
        maplist(writeln,ModAxioms2),
        assertion( ModAxioms2 = [subClassOf(m,n),_,_] ),

        writeln('**D/ab**'),
        extract_module([a,b],Axioms,ModAxioms3),
        maplist(writeln,ModAxioms3),
        assertion( ModAxioms3 = [subClassOf(a,b),_,_,_] ),

        writeln('**D/bn**'),
        extract_module([b,n],Axioms,ModAxioms4),
        maplist(writeln,ModAxioms4),
        assertion( ModAxioms4 = [subClassOf(b,c),_,_,_,_,_] ),
        assertion( member(disjointWith(b,n), ModAxioms4) ).


test(extract_kb) :-
        Links=[
               a-b,
               b-c,
               c-d,
               d-e,
               m-n,
               n-o,
               o-p
              ],
        links2axioms(Links,A),
        links2praxioms(Links,H),
        kb_extract_module([a,o],kb(A,H,_,_,_),Kb2),
        writeln(Kb2).


:- use_module(library(sculptor/ptriple_loader)).
test(extract_full) :-
        kb_from_rdf_file('tests/data/pc.ttl',Kb, [blank_nodes(false)]),
        kb_extract_module(['http://purl.obolibrary.org/obo/Orphanet_217074'],Kb,Kb2),
        kb_A(Kb2,A),
        assertion( A\=[] ),
        maplist(writeln,A),
        length(A,Size),
        writeln(size=Size),
        assertion( Size>20 ).

:- end_tests(extract).
    
