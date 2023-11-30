#!/bin/bash
set -e

cryptol $1 -c 'deepseq (traceVal "--------" "") main'

# generate an expected file
cryptol $1 -c 'deepseq (traceVal "--------" "") main' | grep -A999999 -e "--------" | tail -n +2 > $1.expected
