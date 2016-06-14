#!/usr/bin/env bash

set -o errexit

stack test
(cd client; stack test)
STACK_YAML=stack-run-tests.yaml stack test
