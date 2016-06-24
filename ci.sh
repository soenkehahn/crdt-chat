#!/usr/bin/env bash

set -o errexit

echo testing server and client...
echo ============================
STACK_YAML=stack-run-tests.yaml stack build
STACK_YAML=stack-run-tests.yaml stack test
echo testing server...
echo =================
stack test
echo testing client...
echo =================
(cd client; stack test)
echo building client...
echo ==================
(cd client; make)
