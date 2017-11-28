:- module(learner,
          [
           learn/4,
           learn/5
          ]).
:- use_module(stats).

/*

  STUB CODE

  Currently this uses a very simplistic approach to assign a probability Pr( TargetClass | Features)
  
  */

learn(TargetClass,FTerm,Instances,Pr) :-
        learn(TargetClass,FTerm,Instances,Pr,_).
learn(TargetClass,FTerm,Instances,Pr,PVal) :-
        setof(C,T^I^member(instance(I,T,C),Instances),TargetClasses),
        setof(T,C^I^member(instance(I,T,C),Instances),FTerms),
        aggregate(count, I^T^C^member(instance(I,T,C),Instances), NumInstances),
        member(TargetClass, TargetClasses),        
        aggregate(count, I^T^member(instance(I,T,TargetClass),Instances), NumWithTargetClass),
        PrBackground is NumWithTargetClass / NumInstances,

        % in some cases there may be insufficient instances to estimate
        % background probability - fail here
        NumInstances > 5,
        
        member(FTerm,FTerms),
        aggregate(count,I^C^member(instance(I,FTerm,C),Instances),Num),
        aggregate(count,I^member(instance(I,FTerm,TargetClass),Instances),NumPos),
        % todo - two-sided
        p_value_by_hypergeometric(NumPos,Num, NumWithTargetClass,NumInstances,PVal),
        Pr1 is NumPos/Num,
        Pr is (PVal * PrBackground) + ((1-PVal)*Pr1).
