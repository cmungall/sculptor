:- module(kboom,
          [run_kboom/0]).

:- use_module(library(sculptor)).
:- use_module(library(sculptor/utils)).
:- use_module(library(sculptor/dot_utils)).
:- use_module(library(sculptor/ptriple_loader)).
:- use_module(library(sculptor/index_util)).
:- use_module(library(sculptor/lexinf)).
:- use_module(library(sculptor/extractor)).

:- use_module(library(settings)).

:- setting(workdir,
           atom,
           'target',
           'Directory/folder for intermediate files').
:- setting(search_cutoff,
           number,
           50,
           'Max solutions to search per extracted module').
:- setting(ontology,
           atom,
           none,
           'Ontology file').
:- setting(default_axiom_probability,
           number,
           0.25,
           'For axioms suggested e.g via lexical matching, assign a default probability if no training data').
:- setting(recapitulate,
           list,
           [],
           'Axiom types to be removed and then recapitulated').

run_kboom :-
        debug(kboom,'Loading ontologies',[]),
        load_ontologies(Kb),
        setting(workdir,Dir),
        make_directory_path(Dir),
        debug(kboom,'Indexing',[]),
        index_ann(Dir),
        index_inferences(Dir),
        index_lmatch(Dir),
                                %index_inferhyp(Dir),
        debug(kboom,'Fetching hypothetical axioms',[]),
        setting(default_axiom_probability,DefaultPr),
        setof(Template,suggest_hypothetical_axiom(Template,[default_probability(DefaultPr)]),H),
        write_prolog_list(mod('INIT',weighted),H),
        kb_set_H(Kb,H,Kb2),
        debug(kboom,'Starting iterative solve phase',[]),
        iterative_solve(Kb2,KbFinal),
        write_final_report(Kb2,KbFinal),
        debug(kboom,'Completed!',[]).


iterative_solve(Kb,KbFinal) :-
        showinfo_kb(iteration,Kb),
        select_nodes(Kb,Ns),
        debug(kboom,'Seed for module: ~w',[Ns]),
        kb_extract_module(Ns,Kb,SubKb1),
        showinfo_kb(initial_module,SubKb1),
        kb_smallest_submodule(SubKb1,SubKb),
        showinfo_kb(smallest_submodule,SubKb),
        kb_H(SubKb,H),
        debug(kboom,'Searching over module: H=~w',[H]),
        setting(search_cutoff,SearchCutoff),
        kb_search(SubKb,Sols,[max(SearchCutoff)]),
        Sols=[Best|_SolsRest],
        !,
        showinfo_kb(best,Best),
        make_report(Sols),
        debug(kboom,'Updating kb',[]),
        update_kb(Kb,Best,NextKb),
        showinfo_kb(updated,NextKb),
        iterative_solve(NextKb,KbFinal).
iterative_solve(Kb,Kb) :-
        debug(kboom,'Cannot select nodes, done',[]),
        showinfo_kb(iteration,Kb),
        writeln(done).

axiom_in(A,Axioms,A) :- member(A,Axioms).
axiom_in(A,Axioms,not(A)) :- member(not(A),Axioms).

write_final_report(KbOrig,KbFinal) :-
        kb_A(KbFinal,A1),
        kb_A(KbOrig,A2),
        ord_subtract(A1,A2,A),
        rpt_path('FINAL',ont,pro,Path),
        tell(Path),
        forall(member(Axiom,A),
               format('~q.~n',[Axiom])),
        told.


%! update_kb(+Kb,+GroundKb,?Kb2) is det
%
% given a Kb (the original Kb) plus a solution to a submodule (GroundKb)
% create a new Kb that takes the chosen hypothetical axioms from the solution
% and adds these as base axioms to the new Kb
update_kb(Kb,GroundKb,Kb2) :-
        kb_A_orig(Kb,BaseAxioms),
        kb_A(GroundKb,AllAxioms),
        kb_H(Kb,H1),
        findall(Pr-A,(member(Pr-A,H1),axiom_in(A,AllAxioms,_)),HSel),
        findall(A,(member(_-A1,H1),axiom_in(A1,AllAxioms,A)),ASel),
        assertion( HSel\=[] ),
        ord_subtract(H1,HSel,H2),
        ord_union(BaseAxioms,ASel,BaseAxioms2),
        Kb2 = kb(BaseAxioms2,H2,[],1,BaseAxioms2).

:- dynamic cluster_kb/2.
make_report([Kb|Rest]) :-
        (   nb_current(cluster_id,Id0)
        ->  Id is Id0 +1
        ;   Id = 1),
        nb_setval(cluster_id,Id),
        assert(cluster_kb(Id,Kb)),
        rpt_path(Id,img,png,ImgFile),
        render_kb(Kb,ImgFile,[format(png)]),
        rpt_path(Id,kb,pro,KbProFile),
        write_prolog(KbProFile,Kb),
        rpt_path(Id,rpt,txt,RptFile),
        write_txt_report(RptFile,Kb,Rest).


rpt_path(Id,Base,Suffix,Path) :-
        setting(workdir,Dir),
        concat_atom([Dir,/,module,Id,'-',Base,'.',Suffix],Path).

write_prolog(Path,Term) :-
        tell(Path),
        format('~q.~n',[Term]),
        told.

write_prolog_list(mod(Id,Type),L) :-
        !,
        rpt_path(Id,Type,pro,Path),
        write_prolog_list(Path,L).

write_prolog_list(Path,L) :-
        tell(Path),
        forall(member(X,L),
               format('~q.~n',[X])),
        told.

write_txt_report(Path,Kb,Rest) :-
        tell(Path),
        kb_P(Kb,Pr),
        length(Rest,NumSols),
        all_probs(Pr,Rest,RestPrs,Confidence),
        format(' * Module Probability: ~10f~n',[Pr]),
        format(' * Module Confidence: ~3f~n',[Confidence]),
        format(' * Alternate solutions: ~w~n',[NumSols]),
        format(' * Next best: ',[]),
        forall(member(Pr1,RestPrs),
               format(' ~25f',[Pr1])),
        nl,
        nl,
        AllSols=[Kb|Rest],
        kb_S(Kb,S),
        format('## Positive Axioms Selected~n~n'),
        forall((member(Pr1-A,S),ppa(A,PPA),pquery_given_kbs(A,AllSols,PostPr,[])),
               format(' * ~w Pr(prior)=~3f Pr(post)=~25f~n',[PPA,Pr1,PostPr])),
        nl,
        format('## Negative Axioms Selected~n~n'),
        forall((member(Pr1-not(A),S),ppa(A,PPA),pquery_given_kbs(not(A),AllSols,PostPr,[])),
               format(' * NOT( ~w ) Pr(prior)=~3f Pr(post)=~3f~n',[PPA,Pr1,PostPr])),
        told.

all_probs(PrToCompare, Kbs, Probs, Confidence) :-
        findall(Pr,(member(Kb,Kbs),kb_P(Kb,Pr)),Probs),
        (   Probs=[BestPr|_], BestPr\=0
        ->  Confidence is PrToCompare / BestPr
        ;   Confidence=inf).

        


% only matches positive/atomic axioms
ppa(A,S) :-
        A =.. [R,X,Y],
        node_id_label(X,XL),
        node_id_label(Y,YL),
        sformat(S,'~w --[~w]--> ~w',[XL,R,YL]).

                
showinfo_kb(M, Kb) :-
        kb_H(Kb,H),
        length(H,NumH),
        kb_A_orig(Kb,Ao),
        length(Ao,NumAo),
        kb_A(Kb,A),
        length(A,NumA),
        kb_S(Kb,S),
        length(S,NumS),
        debug(kboom,'KBINFO ~w |A|=~w |A_orig|=~w |H|=~w |S|=~w',[M,NumA,NumAo,NumH,NumS]).

load_ontologies(Kb) :-
        setting(ontology,Ont),
        setting(recapitulate,Excludes),
        kb_from_rdf_file(Ont, Kb, [blank_nodes(false), exclude(Excludes)]).

%! select_nodes(+Kb,?Ns:list) is semidet
%
% select a hypothetic axiom yet to be determined;
% use as base for creating next module
%                               
% succeeds once if there exists a positive axiom in H
select_nodes(Kb,Ns) :-
        kb_H(Kb,H),
        % TODO: store probs descending
        reverse(RH,H),
        (   member(_-A,RH),
            A =.. [_|Ns]
        ->  true
        ;   member(_-not(A),RH),
            A =.. [_|Ns]),
        !.


        
        


