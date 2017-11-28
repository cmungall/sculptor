/*

  BORAT - Bayesian Ontology Reasoning Abduction Tool
  
*/

:- module(sculptor,
          [search/3,
           search/4,
           kb_search/2,
           kb_search/3,
           pquery/4,
           pquery/5,
           pquery_given_kbs/4,

           kb_A/2,
           kb_A_orig/2,
           kb_H/2,
           kb_set_H/3,
           kb_S/2,
           kb_S_axioms/2,
           kb_P/2
           ]).

:- use_module(sculptor/rl_reasoner).

    % https://rlaanemets.com/post/show/reporting-exception-stack-traces-in-a-swi-prolog-application
:- use_module(library(prolog_stack)).
:- use_module(library(error)).

user:prolog_exception_hook(Exception, Exception, Frame, _):-
    (   Exception = error(Term)
    ;   Exception = error(Term, _)),
    get_prolog_backtrace(Frame, 20, Trace),
    format(user_error, 'Error: ~p', [Term]), nl(user_error),
    print_prolog_backtrace(user_error, Trace), nl(user_error), fail.


/*
  util predicates
*/

%! kb_A(+Kb,?LogicalAxioms:list) is det
%
% all logical axioms in KB
kb_A(kb(X,_,_,_,_),X).

%! kb_H(+Kb,?PrAxiomPairs:list) is det
%
% list of pairs of Pr-Axiom from set of hypothetical axioms
kb_H(kb(_,X,_,_,_),X).
kb_H_axioms(S,X) :- kb_H(S,H),findall(A,member(_-A,H),X).

kb_set_H(Kb,H,Kb2) :-
        Kb =.. [F,A,_|Args],
        Kb2 =.. [F,A,H|Args].



%! kb_S(+Kb,?AxiomsInSolution:list) is det
%
% all axioms selected as true in current solution
kb_S(kb(_,_,X,_,_),X).
kb_S_axioms(S,X) :- kb_S(S,H),findall(A,member(_-A,H),X).

%! kb_P(+Kb,?Prob:number) is det
%
% probability of KB
kb_P(kb(_,_,_,X,_),X).

%! kb_A_orig(+Kb,?LogicalAxioms:list) is det
%
% original/seed set of axioms
kb_A_orig(kb(_,_,_,_,X),X).

%! pquery(+Query:term, +Axioms:list, +PrAxioms:list, ?QueryProb:number, +Opts:list) is det
%
% finds the probability a query is true given a knowledge base of logical axioms (Axioms, aka A)
% and hypothetical axioms (PrAxioms, aka H), specified as Weight-Axiom or Prob-Axiom pairs
%
% Query is any boolean conjunction of axioms specified using standard prolog constructs (','/2, ';'/2).
%
% the space of possible consistent worlds is explored, 
pquery(Q,Axioms,PrAxioms,QueryProb) :-
        pquery(Q,Axioms,PrAxioms,QueryProb,[]).
pquery(Q,Axioms,PrAxioms,QueryProb,Opts) :-
        search(Axioms,PrAxioms,Sols,Opts),
        pquery_given_kbs(Q,Sols,QueryProb,Opts).

pquery_given_kbs(Q,Sols,QueryProb,_Opts) :-
        findall(QResult,
                (   member(Kb,Sols),
                    debug(query,'  World = ~w',[Kb]),
                    kb_satisfies_query(Q,Kb,QResult)),
                QResults),
        debug(xquery,'QueryResults=~w',[QResults]),
        QResults1 = [true-0, false-0 | QResults],
        aggregate(sum(P), member(true-P, QResults1), PrTrue),
        aggregate(sum(P), member(false-P, QResults1), PrFalse),
        debug(xquery,'~w Sum(PT)=~w Sum(PF)=~w',[Q,PrTrue,PrFalse]),
        QueryProb is PrTrue / (PrFalse+PrTrue),
        !.
pquery_given_kbs(_,_,0,_).



%! kb_satisfies_query(+Q,+Kb,?Result) is nondet
%
% Result = IsTrue-Prob
%
% nondet - unifies with two results, one for each value of IsTrue
kb_satisfies_query(Q,_,_) :-
        var(Q),
        throw(query_cannot_be_var(Q)).
kb_satisfies_query((X,Y),Kb,True-Pr) :-
        !,
        kb_satisfies_query(X,Kb,True1-Pr),
        kb_satisfies_query(Y,Kb,True2-Pr),
        eval_and(True1,True2,True).
kb_satisfies_query((X;Y),Kb,True-Pr) :-
        !,
        kb_satisfies_query(X,Kb,True1-Pr),
        kb_satisfies_query(Y,Kb,True2-Pr),
        eval_or(True1,True2,True).
kb_satisfies_query(not(Q),Kb,True-Pr) :-
        !,
        kb_satisfies_query(Q,Kb,PosTrue-Pr),
        eval_not(PosTrue,True).
kb_satisfies_query(\+(Q),Kb,Result) :-
        !,
        kb_satisfies_query(not(Q),Kb,Result).
kb_satisfies_query(Q,Kb,True-Pr) :-
        kb_A(Kb,A),
        kb_P(Kb,Pr),
        debug(query,' Pr(~q)=~w',[A,Pr]),
        (   member(Q,A)
        ->  True=true
        ;   True=false).

eval_not(true,false).
eval_not(false,true).

eval_and(true,true,true) :- !.
eval_and(_,_,false) :- !.

eval_or(false,false,false) :- !.
eval_or(_,_,true) :- !.



%! search(+LogicalAxioms:list, +PrAxioms:list, ?CandidateKbs:list) is det
% 
% Search solution space for most likely configuration of weighted axioms.
% 
% PrAxioms = [Pr1-Axiom1, Pr2-Axiom2, ...]
search(Axioms,PrAxioms,Sols2) :-
        search(Axioms,PrAxioms,Sols2,[]).
search(Axioms,PrAxioms,Sols,Opts) :-
        process_opts(Axioms,PrAxioms,Opts,Axioms2,PrAxioms2,Opts2),
        !,
        search(Axioms2,PrAxioms2,Sols,Opts2).
search(Axioms,PrAxioms,Sols2,Opts) :-
        Kb=kb(Axioms,PrAxioms,[],1,Axioms),
        debug(search,'Pre-processing initial weights = ~q',[Kb]),
        preprocess_weights(PrAxioms,PrAxioms2,Opts),
        debug(search,'Pre-processed. Performing search: ~q',[Kb]),

        % switch on axioms that are entailed
        saturate(Axioms, AxiomsInf),
        partition_merge_pr_axioms(AxiomsInf, PrAxioms2, PrAxioms3, [], PrAxiomsOn),
        KbInit=kb(AxiomsInf,PrAxioms3,PrAxiomsOn,1,Axioms),
        
        lsearch([KbInit], Sols, [], 0/0, Opts),
        !,
        length(Sols,NumSols),
        predsort(compare_kbs,Sols,Sols2),
        length(Sols2,NumSols2),
        debug(search,'Solutions: ~w (post-sort: ~w)',[NumSols,NumSols2]).

kb_search(Kb,Sols2) :-
        kb_search(Kb,Sols2,[]).
kb_search(Kb,Sols2,Opts) :-
        kb_A(Kb,Axioms),
        kb_H(Kb,PrAxioms),
        search(Axioms,PrAxioms,Sols2,Opts).

compare_kbs(Order,Kb1,Kb2) :-
        kb_P(Kb1,P1),
        kb_P(Kb2,P2),
        compare(Order,P2-Kb2,P1-Kb1).

% succeeds if an option can be processed, creating
% a new A/H set

process_opts(Axioms,PrAxioms,Opts,Axioms2,PrAxioms,Opts2) :-
        % local unique name assumption
        select(una(local),Opts,Opts2),
        setof(all_unique(Src),C^member(in(C,Src),Axioms),NewAxioms),
        !,
        append(Axioms,NewAxioms,Axioms2).

%! preprocess_weights(+PAs:list,?PAs2:list,+Opts:list) is det
%
% input weighted axiom can either be Prob-Axiom or w(Weight)-Axiom
%
% here we normalize to probabilities, after first
% summing weights for the same axiom
preprocess_weights(H, H5, _Opts) :-
        maplist(prob_to_weight,H,H2),
        sumweights(H2,H3),
        maplist(weight_to_prob,H3,H4),
        sort(H4,H5).

% PAs can include the same axiom with multiple weights
% PAs2 is guaranteed to have unique axioms with weights for same axiom summed
sumweights(PAs,PAs2) :-
        findall(w(SumW)-A,
                aggregate(sum(W),member(w(W)-A, PAs),SumW),
                PAs2).

% logit
prob_to_weight(PA, PA) :-
        % already a weight
        PA=w(_)-_,
        !.
prob_to_weight(P-A, W-A) :-
        % max out at P=0.999
        P > 0.999,
        !,
        prob_to_weight(0.999-A, W-A).
prob_to_weight(P-A, w(W)-A) :-
        W is log(P/(1-P))/log(2),
        !.

% inverse logit
weight_to_prob(w(W)-A, P-A) :-
        !,
        P is 1/(1+2**(-W)).
weight_to_prob(PA,PA).
        

%! lsearch(+KbStack:list, ?SolvledKbs:list, +Visited:list, +NumSols/NumTries:int) is det
%
% depth first search of solution space

lsearch([], [], _, Counter, _Opts) :-
        % BASE CASE 1
        debug(search,'All solutions explored after ~w iterations',[Counter]).
lsearch([_S|_], [], _, TotSols/Counter, Opts) :-
        % BASE CASE 2
        1 is Counter mod 10,

        member(max(Max),Opts),
        % note: this may repeat as dead-ends do not increment
        debug(xsearch,'Testing if ~w iterations > ~w max and total solns ~w > 1',[Counter, Max,TotSols]),
        Counter > Max,
        TotSols > 0,
        !,
        debug(search,'Stopping after ~w iterations',[Counter]).

lsearch([S|Sols], TerminalSols, VList, Counter, Opts) :-
        % DEAD-END
        % A zero-probability solution is not explored further
        % and not added to the list of candidate solutions
        kb_P(S,0),
        !,
        debug(xsearch,'ZERO-PROB DEAD-END',[]),
        lsearch(Sols, TerminalSols, VList, Counter, Opts).

lsearch([S|Sols], TerminalSols, VList, TotSols/Counter, Opts) :-
        % non-terminal: extend node

        % get adjacent solution
        adj(S, S2, VList, VList2, Opts),
        !,

        % depth-first
        CounterPlus1 is Counter+1,
        lsearch([S2,S|Sols], TerminalSols, VList2, TotSols/CounterPlus1, Opts).

lsearch([S|SRest], [S|TerminalSols], VList, TotSols/Counter, Opts) :-
        % no more to choose from
        kb_H(S,[]),
        !,
        % TERMINAL
        kb_P(S,Pr), % for reporting
        debug(xsearch,'Terminal: Pr=~w',[Pr]),
        TotSolsPlus1 is TotSols+1,
        CounterPlus1 is Counter+1,
        lsearch(SRest, TerminalSols, VList, TotSolsPlus1/CounterPlus1, Opts).

lsearch([S|Sols], TerminalSols, VList, Counter, Opts) :-
        debug(xsearch,'Discarding solution; no further selections possible: ~w',[S]),
        lsearch(Sols, TerminalSols, VList, Counter, Opts).


%% adj(+Kb, ?NextKb, +VList:list, ?VlistUpdated) is semidet
%
% find adjacent/next node
%
% currently just selects next-most probable independent of adjacency
adj(S, S2, VList, [Sig2|VList], Opts) :-
        %debug(xsearch,'Choosing from ~w',[S]),
        S = kb(Axioms, PrAxioms, PrAxiomsOn, _, OrigAxioms),
        pr_axioms_signature(PrAxiomsOn, Sig),
        debug(xsearch,'Extending, curr sig = ~w',[Sig]),
        findall(PA, (select_pr_axiom(PrAxioms,Pr,A,PrAxiomsOn,Axioms,_),
                     PA = Pr-A,
                     add_axiom_to_signature(A,Sig,Sig2),
                     \+ member(Sig2, VList)
                  ),
                PAs),
        PAs\=[],
        debug(xsearch,'potential next choices = ~w',[PAs]),
        !,
        length(PAs,NumPAs),
        sort(PAs,SortedPAs),
        reverse(SortedPAs,[Pr-A|_]),
        debug(xsearch,'Select: ~w Pr: ~w from ~w choices',[A, Pr, NumPAs]),
        saturate([A|Axioms], AxiomsInf),
        partition_merge_pr_axioms(AxiomsInf, PrAxioms, PrAxioms2, PrAxiomsOn, PrAxiomsOn2),
        %PrAxiomsOn2 = [Pr-A|PrAxiomsOn],
        add_axiom_to_signature(A,Sig,Sig2),
        debug(xsearch,'EXT = ~w + ~w ==> ~w',[A,Sig,Sig2]),
        S2 = kb(AxiomsInf, PrAxioms2, PrAxiomsOn2, Prob, OrigAxioms),

        % this grounds Prob
        calc_kb_prob(S2, A, Opts),
        debug(xsearch,'PROB = ~w',[Prob]).

%! partition_merge_pr_axioms(+LogicalAxioms:list, +PrAxioms:list, ?PrAxioms2:list, +PrAxiomsOn:list, ?PrAxiomsOn2:list) is nondet
%
% find all hypothetical axioms in PrAxioms that are already entailed (ie in LogicalAxioms)
% remove these and add to list of ON PrAxioms
%
% LogicalAxiom = AtomicAxiom | not(AtomicAxiom)
% PrAxiomRemaining = Pr-AtomicAxiom
% PrAxiomSelected = Pr-LogicalAxiom

partition_merge_pr_axioms(LA, PAs, PAs2, PAsOn, PAsOn2Set) :-
        partition_merge_pr_axioms1(LA, PAs, PAs2, PAsOn, PAsOn2),
        !,
        sort(PAsOn2,PAsOn2Set).

partition_merge_pr_axioms1(_, [], [], PrAxiomsOn, PrAxiomsOn) :- !.
partition_merge_pr_axioms1(LogicalAxioms, [Pr-AA|PrAxioms], PrAxioms2, PrAxiomsOn, [Pr2-A2|PrAxiomsOn2]) :-
        (   member(AA,LogicalAxioms),
            Pr2 = Pr,
            A2=AA
        ;   member(not(AA),LogicalAxioms),
            Pr2 is 1-Pr,
            A2=not(AA)),
        !,
        partition_merge_pr_axioms1(LogicalAxioms, PrAxioms, PrAxioms2, PrAxiomsOn, PrAxiomsOn2).
partition_merge_pr_axioms1(LogicalAxioms, [PA|PrAxioms], [PA|PrAxioms2], PrAxiomsOn, PrAxiomsOn2) :-
        partition_merge_pr_axioms1(LogicalAxioms, PrAxioms, PrAxioms2, PrAxiomsOn, PrAxiomsOn2).


%! axiom_selected(+Pr, +Axiom, +Axioms:list, ?Pr2, ?Axiom2)
%
axiom_selected(P,Axiom,Axioms,P,Axiom) :- member(Axiom,Axioms),!.
axiom_selected(P,Axiom,Axioms,P2,not(Axiom)) :- member(not(Axiom),Axioms),!,P2 is 1-P.



%! select_pr_axiom(+PrAxioms:list, ?Pr, ?A, +PrAxiomsOn:list, +Axioms:list) is nondet
%
% select an axiom that is in H but not in the current A
select_pr_axiom(PrAxioms,Pr,A,PrAxiomsOn,Axioms,PrAxioms2) :-
        select(PrPos-PosA,PrAxioms,PrAxioms2),
        % TODO: investigate if more efficient to keep stack of remaining items in H
        % TODO - no longer necessary?
        \+member(_-PosA, PrAxiomsOn),
        \+member(_-not(PosA), PrAxiomsOn),
        \+member(PosA, Axioms),
        \+member(not(PosA), Axioms),
        (   Pr=PrPos,
            A=PosA
        ;   Pr is 1-PrPos,
            A=not(PosA)).

%! calc_kb_prob(+Kb, +NextAxiom, +Opts:list) is det
%! 
% calculates prob and unifies free variable
%
% P(Kb) = P(H,A) = P(H)P(A|H)
% P(H) = P(H1=h1)P(H2=h2)...P(Hn=hn)
calc_kb_prob(S, _A, _Opts) :-
        kb_A(S,Axioms),
        % P(A|H)=0 if ontology is incoherent
        member(unsat(_), Axioms),
        debug(xsearch,'UNSAT',[]),
        kb_P(S,0),
        !.

% P(Kb) = P(H,A) = P(H)P(A|H)
% P(H) = P(H1=h1)P(H2=h2)...P(Hn=hn)
calc_kb_prob(S, NextAxiom , Opts) :-
        kb_S(S,PrAxiomsOn),
        
        % P(H): product of all probabilities
        aproduct(PrAxiomsOn, 1, ProbProduct),

        % P(A|H) - default is fixed probability
        %  increase P(A|H) if it has explanatory power
        %  todo: calculate this only for leaf nodes for efficiency?
        explanatory_power(S, NextAxiom,Opts,Power),
        
        % Crude: noisy-OR of two possible outcomes
        Prob is 1-( (1/(2**Power)) * (1-ProbProduct) ),

        % unify probability (assumption is that currently unbound)
        kb_P(S,Prob),
        !.

%! explanatory_power(+Kb, +NextAxiom, +Opts:list, ?Power:int) is det
%
% calculate explanatory power of an axiom
% see explain_test for example.
explanatory_power(_, _, Opts, 0) :-
        \+ member(use_explain(true),Opts),
        !.
explanatory_power(S, NextAxiom, _Opts, Power) :-
        NextAxiom\=in(_),
        NextAxiom\=not(_),
        kb_A_orig(S,OrigAxioms),
        kb_S_axioms(S,OnAxioms),
        select(NextAxiom, OnAxioms, OnAxiomsWithout),
        append(OrigAxioms,OnAxiomsWithout,BaseAxioms),
        setof(EA,get_expl(BaseAxioms, NextAxiom, EA),EAs),
        !,
        debug(explain,'Explained axioms = ~w',[EAs]),
        length(EAs,Power).
explanatory_power(_S, _NextAxiom, _Opts, 0).

% TODO: consider backwards chaining
get_expl(Axioms,NewA,EA) :-
        select(EA,Axioms,Axioms2),
        saturate(Axioms2,AxiomsInfWithout),
        \+ member(EA,AxiomsInfWithout),
        saturate([NewA|AxiomsInfWithout],AxiomsInf),
        member(EA,AxiomsInf).

% adds an axiom to an axiom signature
add_axiom_to_signature(A,Sig,Sig2) :-
        axiom_id(A,A_id),
        ord_union([A_id],Sig,Sig2).
        
% For efficiency we translate axioms to Ids
:- dynamic axiom_id_fact/2.
axiom_id(A,Id) :-  axiom_id_fact(A,Id),!.
axiom_id(A,Id) :-  gensym(a,Id),assert(axiom_id_fact(A,Id)),!.

% todo - use bitwise ops
pr_axioms_signature(PrAxioms,Ids) :- setof(Id,Pr^A^(member(Pr-A,PrAxioms),axiom_id(A,Id)),Ids),!.
pr_axioms_signature(_,[]).

aproduct([],P,P).
aproduct([Pr-_|L],P1,POut) :-
        P2 is Pr*P1,
        aproduct(L,P2,POut).
