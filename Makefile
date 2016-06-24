run-tests:
	STACK_YAML=stack-run-tests.yaml stack test

start-server:
	stack build
	stack exec -- patches-server

start-sensei:
	STACK_YAML=stack-run-tests.yaml stack exec -- sensei-web all-tests/AllTests.hs -Wall -- --fail-fast

seito:
	seito
