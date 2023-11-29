#!/bin/bash
set -e

# generate an expected file
cryptol $1 -c 'deepseq (traceVal "--------" "") cryMain' | grep -A999999 -e "--------" | tail -n +2 > $1.expected

# remove newline at the end
truncate -s -1 $1.expected