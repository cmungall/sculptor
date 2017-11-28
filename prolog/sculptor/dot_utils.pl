:- module(dot_utils,
          [dot_convert/3,
           can_render_image/0,
           render_kb/3]).

:- use_module(dot_dcg).
:- use_module(library(sculptor)).
:- use_module(library(sculptor/ptriple_loader)).
:- use_module(library(semweb/rdf11)).

equiv_clique(Kb,Clique) :-
        setof(CliqueId,X^member_equiv_clique(Kb,X,CliqueId),Cliques),
        member(Clique,Cliques).

member_equiv_clique(Kb,X,CliqueId) :-
        member_equiv_clique(Kb,X,_,CliqueId).
member_equiv_clique(Kb,X,CliqueSet,CliqueId) :-
        kb_A(Kb,A),
        setof(Y,member(equivalentTo(X,Y),A),CliqueSet),
        set_id(CliqueSet,CliqueId).

:- dynamic set_id_store/2.
set_id(S,Id) :-
        set_id_store(S,Id),
        !.
set_id(S,Id) :-
        gensym(s,Id),
        assert(set_id_store(S,Id)),
        !.


kb_to_dotgraph(Kb,G,Opts) :-
        findall(S,kb_stmt(Kb,S,Opts),Stmts),
        G=digraph(mygraph,
                  Stmts).

node_stmt(Kb,N,Attrs,Opts) :-
        findall(Attr,node_attr(Kb,N,Attr,Opts),Attrs).

kb_stmt(Kb,subgraph(CId,Stmts),Opts) :-
        equiv_clique(Kb,CId),
        findall(node_stmt(N,Attrs),(member_equiv_clique(Kb,N,CId),
                                    node_stmt(Kb,N,Attrs,Opts)),
                Stmts).

kb_stmt(Kb,node_stmt(N,Attrs),Opts) :-
        kb_A(Kb,A),
        member(cls(N),A),
        findall(Attr,node_attr(Kb,N,Attr,Opts),Attrs).
kb_stmt(Kb,edge_stmt([X,Y],Attrs),Opts) :-
        (   kb_A_orig(Kb,A),member(Ax,A)
        ;   kb_S(Kb,S),member(_-Ax,S)),
        axiom_edge(Ax,R,X,Y),
        findall(Attr,edge_attr(Kb,Ax,X,R,Y,Attr,Opts),Attrs).

node_attr(_,_,attr("shape",box),_).

node_attr(_,N,attr("label",Label),_Opts) :-
        node_id_label(N,Label).

xxnode_label(N,Label,_) :-
        rdf(N,rdfs:label,S^^_),
        !,
        atom_string(Label,S).
xxnode_label(_,'',_).

edge_attr(_Kb,_,_X,R,_Y,attr("label",R),_) :- R\=subClassOf, R\=equivalentTo.
edge_attr(_Kb,_,_X,subClassOf,_Y,attr("arrowhead",empty),_).
edge_attr(_Kb,_,_X,equivalentTo,_Y,attr("arrowhead",diamond),_).
edge_attr(_Kb,_,_X,equivalentTo,_Y,attr("arrowtail",diamond),_).
edge_attr(_Kb,_,_X,equivalentTo,_Y,attr("dir",both),_).
edge_attr(_Kb,_,_X,equivalentTo,_Y,attr("weight",100),_).
edge_attr(_Kb,not(_),_,_,_,attr("style",dotted),_).
edge_attr(_Kb,not(_),_,_,_,attr("color",red),_).
edge_attr(Kb,Ax,_,_,_,attr("penwidth",Width),_) :-
        kb_S(Kb,S),
        member(Pr-Ax,S),
        Width is floor(Pr*10)+1.
edge_attr(Kb,Ax,_,_,_,attr("color",blue),_) :-
        kb_S(Kb,S),
        member(_-Ax,S),
        Ax\=not(_).
edge_attr(Kb,Ax,_,_,_,attr("label",PrPct),_) :-
        kb_S(Kb,S),
        member(Pr-Ax,S),
        (   Ax=not(_)
        ->  Pr1 is 1-Pr
        ;   Pr1 = Pr),
        PrPct is floor(Pr1*100).
        


axiom_edge(subClassOf(X,Y),subClassOf,X,Y) :- X\=Y.
axiom_edge(equivalentTo(X,Y),equivalentTo,X,Y) :- X\=Y.
axiom_edge(not(A),R,X,Y) :- axiom_edge(A,R,X,Y).

render_kb(Kb,File,Opts):-
        kb_to_dotgraph(Kb,G,Opts),
        dot_convert(G,File,Opts).




%% dot_convert(+DotTerm,+Fmt,+File) is det
%
% translate Graph to dot, translate to an image format and write to File.
% if File is var, write to user_output
dot_convert(DotTerm,File,Opts):-
        option(format(Fmt),Opts,dot),
        (   nonvar(File)
        ->  true
        ;   tmp_file(Fmt,FileBase),
            concat_atom([FileBase,Fmt],'.',File)),

        % if dot format then DotFile is final, otherwise it is intermediate
        (   Fmt=dot
        ->  DotFile=File
        ;   tmp_file(dot,DotFile)),

        write_dotfile(DotTerm, DotFile),
        
        % convert dotfile to File, unless Fmt=dot
        (   Fmt=dot
        ->  true
        ;   shell_convert(File,DotFile,Fmt,Opts)).
        
write_dotfile(DotTerm,DotFile) :-
        % convert graph term to dot atom
        dot(DotTerm,DotCodes,[]),
        !,
        atom_codes(DotAtom,DotCodes),
        
        % write dot file
        tell(DotFile),
        write(DotAtom),
        told.

shell_convert(OutFile,DotFile,Fmt,Opts) :-
	option(rankdir(RankDir),Opts,'BT'),
        dotpath(DotPath),
        sformat(Cmd,'~w -o ~w -Grankdir=~w -T~w ~w',[DotPath,OutFile,RankDir,Fmt,DotFile]),
	debug(dot,'cmd: ~w',[Cmd]),
        shell(Cmd,Err),
        (   Err=0
        ->  true
        ;   format(user_error,'Command failed: ~w',[Cmd])).

dotpath(Dot):-
        (   expand_file_search_path(path_to_dot(dot),Dot)
        ->  true
        ;   Dot=dot).

can_render_image :-
        dotpath(_).


        
