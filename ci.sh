#!/usr/bin/env bash

set -o errexit

stack test
(cd client; stack test)
(cd client; make)
STACK_YAML=stack-run-tests.yaml stack test
