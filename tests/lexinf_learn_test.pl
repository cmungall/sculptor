:- begin_tests(lexinf_learn).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).
:- use_module(library(sculptor/dot_utils)).
:- use_module(library(sculptor/ptriple_loader)).
:- use_module(library(sculptor/lexinf)).
:- use_module(library(sculptor/extractor)).

:- debug(index).
:- debug(search).
:- debug(xsearch).

% https://rlaanemets.com/post/show/reporting-exception-stack-traces-in-a-swi-prolog-application
:- use_module(library(prolog_stack)).
:- use_module(library(error)).

user:prolog_exception_hook(Exception, Exception, Frame, _):-
    (   Exception = error(Term)
    ;   Exception = error(Term, _)),
    get_prolog_backtrace(Frame, 20, Trace),
    format(user_error, 'Error: ~p', [Term]), nl(user_error),
    print_prolog_backtrace(user_error, Trace), nl(user_error), fail.

render(Kb,N) :-
        can_render_image,
        !,
        concat_atom(['target/',N,'.png'],File),
        render_kb(Kb,File,[format(png)]).
render(_,_) :-
        debug(dot,'Skipping rendering step',[]).


showall(G) :-
        forall(G,
               format('~q.~n',[G])).


seed_class('http://purl.obolibrary.org/obo/Orphanet_217074').
%foo('0').

:- debug(extract).

test(train) :-
        kb_from_rdf_file('tests/data/pc.ttl',Kb, [blank_nodes(false)]),
        index_inferences,
        index_ann,
        index_lmatch,

        % note: tabling training_instance/3 seems to introduce a bug where this fails
        forall(training_instance(R,M,'http://purl.obolibrary.org/obo/MONDO_0015278'-'http://purl.obolibrary.org/obo/Orphanet_1333'),
               (   writeln(m=M),
                   R=equivalentTo)),
        
        writeln('INST'),
        showall(training_instance(_,_,_)),
        writeln('LEARNED'),
        showall(predict_relation(_R,_M,_P,_PVal)),
        showall(suggest_hypothetical_axiom(_)),
        assertion( \+ ((suggest_hypothetical_axiom(equivalentTo(X,Y)),
                        suggest_hypothetical_axiom(equivalentTo(Y,Y))))),
        nl.


:- end_tests(lexinf_learn).


