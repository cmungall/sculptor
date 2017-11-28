all: test

SWIPL = swipl  -L0 -G0 -T0  -p library=prolog
test:
	$(SWIPL) -l tests/tests.pl -g run_tests,halt

bigtest:
	$(SWIPL) -l tests/bigtests.pl -g run_tests,halt

test-%:
	$(SWIPL) -l tests/$*_test.pl -g run_tests,halt


