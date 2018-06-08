#!/usr/bin/env bash

erlc -DTEST romdition.erl
erl -noshell -eval "eunit:test(romdition, [verbose])." -run init stop