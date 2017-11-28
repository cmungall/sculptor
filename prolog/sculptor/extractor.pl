:- module(extractor,
          [kb_extract_module/3,
           extract_module/3,
           extract_module/4,

           kb_smallest_submodule/2]).

:- use_module(library(sculptor)).

% both logical axioms and weighted axioms (with probs)
kb_AH_axioms(Kb,Axioms) :-
        kb_A(Kb,A),
        kb_H(Kb,H),
        append(A,H,AxiomsL),
        list_to_ord_set(AxiomsL,Axioms).

kb_size(Kb,Size) :-
        kb_AH_axioms(Kb,Axs),
        length(Axs,Size).


%! kb_extract_module(+Nodes:list, +Kb, ?KbModule) is det
%
% extracts a sub-module using Nodes as seed list
kb_extract_module(Nodes, Kb, Kb2) :-
        kb_AH_axioms(Kb,Axioms),
        extract_module(Nodes, Axioms, SubAxioms),
        findall(X,(member(X,SubAxioms),X\=_-_),A2),
        findall(X,(member(X,SubAxioms),X=_-_),H2),
        Kb2=kb(A2,H2,[],1,A2).


extract_module(Nodes, Axioms, SubAxioms) :-
        extract_module(Nodes, Axioms, SubAxioms, []).
extract_module(Nodes, Axioms, SubAxioms, Opts) :-
        extract_module(Nodes, Axioms, [], SubAxioms, [], Opts).

extract_module([], Axioms, ModAxioms, ModAxioms2, _, _Opts) :-
        debug(extract,'All nodes processed. Axs=~w',[ModAxioms]),
        axioms_refnodes(ModAxioms,Ns,[]),
        findall(A,(member(A,Axioms),
                   all_axiom_refs_in(A,Ns)),
                ModAxioms1),
        ord_union(ModAxioms,ModAxioms1,ModAxioms2).
        
extract_module([N|Nodes], Axioms, MAs, Acc, VNodes, Opts) :-
        debug(extract,'~w from ~w',[N,Nodes]),
        setof(A,node_parent_axiom_member(N,A,Axioms),As),
        axioms_refnodes(As,NextNodes,[N|VNodes]),
        !,
        ord_union(As,MAs,NewMAs),
        ord_union(Nodes,NextNodes,Nodes2),
        ord_subtract(Nodes2,VNodes,NextNodes2),
        debug(extract,' Nextnodes = ~w Axs = ~w',[NextNodes, NewMAs]),
        extract_module(NextNodes2, Axioms, NewMAs, Acc, [N|VNodes], Opts).
extract_module([N|Nodes], Axioms, MAs, Acc, VNodes, Opts) :-
        debug(extract,'No new neighbors for ~w // ~w',[N,Nodes]),
        extract_module(Nodes, Axioms, MAs, Acc, VNodes, Opts).

node_parent_axiom_member(N,Pr-A,Axioms) :-
        member(Pr-A,Axioms),
        node_parent_axiom(N,A,_).
node_parent_axiom_member(N,A,Axioms) :-
        member(A,Axioms),
        A \= _-_,
        node_parent_axiom(N,A,_).

node_parent_axiom(N,subClassOf(N,_),_).
node_parent_axiom(N,properSubClassOf(N,_),_).
node_parent_axiom(N,equivalentTo(N,_),_).
node_parent_axiom(N,equivalentTo(_,N),_).

axiom_references(_-A,N) :- axiom_references(A,N).
axiom_references(T,N) :- T=..[_|Args],member(N,Args).

axioms_refnodes(As,Ns,VNs) :-
        setof(N2,A^(member(A,As),
                    axiom_references(A,N2),
                    \+member(N2,VNs)),Ns).

all_axiom_refs_in(A,Ns):-
        A=..[_|Args],
        forall(member(X,Args),
               member(X,Ns)).

kb_submodule(Kb,SubKb) :-
        kb_AH_axioms(Kb,Axioms),
        member(Ax,Axioms),
        axiom_references(Ax,N),
        kb_extract_module([N],Kb,SubKb),
        % exclude trivial. TODO: opts
        kb_H(SubKb,[_,_|_]),
        \+ kb_AH_axioms(SubKb,Axioms).

kb_smallest_submodule(Kb,SubKb) :-
        setof(Sub1,kb_submodule(Kb,Sub1),Subs),
        member(SubKb,Subs),
        kb_size(SubKb,Size),
        \+ ((member(Z,Subs),
             kb_size(Z,ZSize),
             ZSize<Size)),
        !.
kb_smallest_submodule(Kb,Kb).



