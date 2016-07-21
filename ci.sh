#!/usr/bin/env bash

set -o errexit

echo testing server...
echo =================
stack test
echo testing client...
echo =================
(cd client; stack test)
echo building client...
echo ==================
(cd client; make)
