/*

  Loads triples or quads from an RDF format and interprets as a probabilistic knowledgebase.

  Logical axioms are encoded in the standard way.

  Probabilistic axioms are encoded in one of two ways:

   - each hypothetical axiom in a named graph G, with <G :prob "NUM"^^xsd:float>
   - each hypothetical axiom has axiom annotations :prob "NUM"^^xsd:float

  Here :prob is a predicate that relates the statement/graph to a probability value
  currently hardcoded to http://semanticscience.org/resource/SIO_000638

  See trig_test for details
  
  TODO: allow weights as well as probs

*/

:- module(ptriple_loader,
          [kb_from_rdf/1,
           kb_from_rdf/2,
           kb_from_rdf_file/2,
           kb_from_rdf_file/3,

           node_id_label/2,
           node_label/2,
           node_shortid/2
           ]).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).

:- rdf_meta a2t(-,r).

a2t( subClassOf(A,B), rdf(A,rdfs:subClassOf,B,_G) ).
a2t( equivalentTo(A,B), rdf(A,owl:equivalentClass,B,_G) ).
a2t( disjointWith(A,B), rdf(A,owl:disjointWith,B,_G) ). % TODO
a2t( in(A,B), rdf(A,rdfs:isDefinedBy,B,_G) ).

%! kb_from_rdf(?Kb) is det
%
% initialize a KB from currently loaded rdf_graph
kb_from_rdf(Kb) :-
        kb_from_rdf(Kb,[]).
kb_from_rdf(Kb, Opts) :-
        solutions(A,(axiom(A, Opts),\+filtered(A, Opts)),As),
        debug(rdf,'A=~w',[As]),
        kb_A(Kb,As),
        kb_A_orig(Kb,As),
        solutions(W-A,weighted_axiom(A,W),H),
        debug(rdf,'H=~w',[H]),
        kb_H(Kb,H).

kb_from_rdf_file(File,Kb) :-
        kb_from_rdf_file(File,Kb,[]).
kb_from_rdf_file(File,Kb,Opts) :-
        (   member(append(true),Opts)
        ->  true
        ;   rdf_retractall(_, _, _, _)),
        rdf_load(File),
        kb_from_rdf(Kb, Opts).

filtered(A, Opts) :-
        option(exclude(L),Opts),
        member(P,L),
        A =.. [P|_].

                 
axiom(A, Opts) :-
        a2t(A, T),
        debug(rdf,'Q: ~w',[T]),
        T,
        \+ filter_out(A, Opts),
        debug(rdf,'A: ~w',[T]),
        \+ triple_weight(T,_).
weighted_axiom(A,W) :-
        a2t(A, T),
        T,
        triple_weight(T,W).

weight_pred('http://semanticscience.org/resource/SIO_000638').

filter_out(A, Opts):-
        member(blank_nodes(false), Opts),
        A =.. [_|Args],
        member(Arg, Args),
        rdf_bnode(Arg),
        !.



triple_weight(T,W) :-
        weight_pred(P),
        T=rdf(_,_,_,G),
        rdf(G,P,W^^_).
triple_weight(T,W) :-
        weight_pred(P),
        T=rdf(S,P,O,_),
        rdf(A,owl:annotatedSource,S),
        rdf(A,owl:annotatedProperty,P),
        rdf(A,owl:annotatedTarget,O),
        rdf(A,P,W^^_).

node_shortid(N,Id) :-
        rdf_global_id(Id,N),
        N\=Id,
        !.
node_shortid(N,Id) :-
        concat_atom(Toks,'/',N),
        !,
        reverse(Toks,[Id,_|_]).
node_shortid(N,Id) :-
        concat_atom([_,Id],'#',N),
        !.
node_shortid(N,N).

node_label(N,Label) :-
        rdf(N, rdfs:label, S^^_),
        atom_string(Label,S).

node_id_label(N,IdLabel) :-
        node_label(N,Label),
        !,
        node_shortid(N,Id),
        concat_atom([Id,Label],' ',IdLabel).
node_id_label(N,Id) :-
        !,
        node_shortid(N,Id).

