:- load_files([
    tests/abduction_test,
    tests/basic_test,
    tests/deduction_test,
    tests/explain_test,
    tests/extract_test,
    tests/intersection_test,
    tests/kboom_test,
    tests/learner_test,
    tests/lexinf_test,
    tests/query_test,
    tests/solve_test,
    tests/trig_test,
    tests/weights_test
    %tests/lexinf_learn_test,
], [ if(not_loaded) ]).
