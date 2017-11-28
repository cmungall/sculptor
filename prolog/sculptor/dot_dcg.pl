% taken from https://github.com/edechter/dot_dcg/blob/master/prolog/dot_dcg.pl

:- module(dot_dcg, [dot/3]).

:- use_module(library(dcg/basics)).

% Subset of the dot language grammar. See www.graphviz.org/doc/info/lang.html
% Comments prefixed "DOT Spec" are taken verbatim from the specification.

% TODO: Allow logical lines to be separated by backslash and  newline
% TODO: Allow double-quoted strings to be concatenated using a '+' operator
% TODO: Support comments
% TODO: Semi-colons are generally optional, but need to handle exlusion (see spec)
% TODO: Keywords should be case-insensitive
% TODO: Unify representation of quote and unquoted IDs
% TODO: Enforcement of quoted keyword IDs

% DOT Spec: graph : [ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
% TODO: Support strict
% TODO: Support un-directed graph
dot(digraph(Name, StmtList)) -->
        w_spc_opt, "digraph", w_spc,
        id(Name),
        w_spc,
        "{", w_spc_opt, stmt_list(StmtList), w_spc_opt, "}",
        w_spc_opt.

        

% DOT Spec: stmt_list : [ stmt [ ';' ] [ stmt_list ] ]
stmt_list([]) --> [].
stmt_list([Stmt]) --> stmt(Stmt).
stmt_list([Stmt]) --> stmt(Stmt), w_spc_opt, ";".
stmt_list([Stmt|Rest]) --> stmt(Stmt), w_spc_opt, ";", w_spc_opt, stmt_list(Rest).
% stmt_list([Stmt|Rest]) --> stmt(Stmt), w_spc_opt, stmt_list(Rest).


% DOT Spec: stmt : node_stmt | edge_stmt | attr_stmt | ID '=' ID | subgraph
% TODO: attr_stmt
                                % TODO: ID '=' ID
stmt(EdgeStmt) --> edge_stmt(EdgeStmt).
% stmt(EdgeStmt) --> edge_stmt(EdgeStmt), w_spc_opt, ";".
stmt(NodeStmt) --> node_stmt(NodeStmt).
% stmt(NodeStmt) --> node_stmt(NodeStmt), w_spc_opt, ";".
stmt(SubGraph) --> subgraph(SubGraph).
% stmt(SubGraph) --> subgraph(SubGraph), w_spc_opt, ";".

% DOT Spec: attr_stmt :	(graph | node | edge) attr_list
% TODO
        
% DOT Spec: attr_list : '[' [ a_list ] ']' [ attr_list ]
attr_list(AList) -->
        "[", w_spc_opt, a_list(AList), w_spc_opt, "]",
        !.
attr_list(Merged) -->
        "[", w_spc_opt, 
        { merge(AList, Rest, Merged) }, 
        a_list(AList),
        w_spc_opt, "]",
        w_spc_opt,
        attr_list(Rest).


% DOT Spec:  a_list : ID '=' ID [ (';' | ',') ] [ a_list ]
a_list([]) --> [].
a_list([Attr]) -->
        attr(Attr), !.
a_list([Attr|Rest]) -->
        attr(Attr),
        w_spc_opt,
        ("," ; ";"),
        w_spc_opt,
        a_list(Rest), !.


attr(attr(Name, Value)) --> id(Name), w_spc_opt, "=", w_spc_opt, id(Value), !.
attr(attr(Name)) --> id(Name).            

xxxsubgraph_stmt(subgraph(Name, _Attrs, StmtList)) -->
        w_spc_opt, "subgraph", w_spc,
        id(Name),
        w_spc,
        "{", w_spc_opt, stmt_list(StmtList), w_spc_opt, "}",
        w_spc_opt.

% DOT Spec: edge_stmt : (node_id | subgraph) edgeRHS [ attr_list ]
% TODO: Subgraph
edge_stmt(edge_stmt(Nodes)) --> edge(Nodes).
edge_stmt(edge_stmt(Nodes, AttrList)) --> edge(Nodes), w_spc_opt, attr_list(AttrList), !.


edge([First|Rest]) --> node_id(First), w_spc_opt, edge_rhs(Rest).

% DOT Spec: edgeRHS : edgeop (node_id | subgraph) [ edgeRHS ]
% TODO: Subgraph
                                % TODO: Edge type

edge_rhs([Node]) -->
        edge_op,
        w_spc_opt,
        node_id(Node).
edge_rhs([Node|Rest]) -->
        edge_op,
        w_spc_opt,
        node_id(Node),
        w_spc_opt,
        edge_rhs(Rest).
% edge_rhs([Node]) --> edge_op, w_spc_opt, node_id(Node).


% DOT Spec: node_stmt : node_id [ attr_list ]
node_stmt(node_stmt(NodeId, AttrList)) --> node_id(NodeId), w_spc, attr_list(AttrList).
node_stmt(node_stmt(NodeId)) --> node_id(NodeId).


% DOT Spec: node_id : ID [ port ]
% TODO: Port
node_id(NodeId) --> id(NodeId).

% DOT Spec: port: ':' ID [ ':' compass_pt ] | ':' compass_pt
% DOT Spec: subgraph : [ subgraph [ ID ] ] '{' stmt_list '}'
subgraph(subgraph(SubGraphId, StmtList)) -->
        "subgraph",
        w_spc,
        "cluster_",        
        id(SubGraphId),
        w_spc,
        "{", stmt_list(StmtList), "}".

% DOT Spec: compass_pt : (n | ne | e | se | s | sw | w | nw | c | _)
% TODO

id_elem(C) -->
        [C],
        {code_type(C, alnum)
        ;
         atom_codes('_', [C])
        }.

% DOT Spec: An ID is one of the following:
% DOT Spec: Any string of alphabetic ([a-zA-Z\200-\377]) characters, underscores
                                % ('_') or digits ([0-9]), not beginning with a digit;
                                % DOT Spec: a numeral [-]?(.[0-9]+ | [0-9]+(.[0-9]*)? );
% DOT Spec: any double-quoted string ("...") possibly containing escaped quotes (\");

id(Number) -->
        {number(Number)},
        !,
        number(Number).
id(AId) -->
        % if atomic, then translate to codes and recurse
        {atomic(AId), atom_codes(AId, Id)},
        !,
        id(Id).
id(AId) -->
        % swi-prolog7 - strings not lists
        {string(AId), string_codes(AId, Id)},
        !,
        id(Id).

id([C]) -->
        id_elem(C),
        {\+ digit(C, [C], [])},
        !.
id([C|Cs]) -->
        id_elem(C),
        {\+ digit(C, [C], [])},
        id_(Cs).

id(Cs) -->
       quoted_string_body(Cs, false, false).
id(Cs) -->
        {is_list(Cs),
         atom_codes('"',[Q]),
         append([Q|Cs],[Q],Cs2)},
        quoted_string_body(Cs2, false, false).



id_([C]) --> id_elem(C).
id_([C|Cs]) --> id_elem(C), id_(Cs).




% id(Id) --> quoted_string(Id).

% DOT Spec: an HTML string (<...>).
% TODO



% Quoted string
quoted_string(AString) -->
        { atom(AString), atom_codes(AString, String) },
        quoted_string_body(String, false, false).

quoted_string_body([34|String], false, false, [34|Codes], Rest):-
    % First character is a quote
    quoted_string_body(String, true, false, Codes, Rest).

quoted_string_body([92|String], true, false, [92|Codes], Rest):-
    % First character is a backslash (i.e., escape symbol)
    quoted_string_body(String, true, true, Codes, Rest).

quoted_string_body([C|String], true, true, [C|Codes], Rest):-
    % Escaped is true, so just (blindly, for now) append whatever was escaped
    quoted_string_body(String, true, false, Codes, Rest).

quoted_string_body([C|String], true, false, [C|Codes], Rest):-
    % Character not a quote
    C \= 34,
    quoted_string_body(String, true, false, Codes, Rest).

quoted_string_body([34], true, false, [34|Codes], Rest) :-
    % Closing quote - unify Rest with remainder of input
    Rest = Codes, !.

% Misc
% TODO: Un-directed graph (--)
edge_op --> "-", ">".

% Mandatory white space
w_spc --> w_spc_char.
w_spc --> w_spc_char, w_spc.

w_spc_char --> [32]; [10]; [11]; [12]; [13].

% Optional white space
w_spc_opt --> [].
w_spc_opt --> w_spc.

% Utility predicate for merging one list into another
merge([], Ys, Ys).
merge([X|Xs], Ys, [X|Zs]) :- merge(Xs, Ys,  Zs).
