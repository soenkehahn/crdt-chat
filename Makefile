run-tests:
	STACK_YAML=stack-run-tests.yaml stack test

start-sensei:
	STACK_YAML=stack-run-tests.yaml stack exec -- sensei-web all-tests/AllTests.hs

seito:
	seito
