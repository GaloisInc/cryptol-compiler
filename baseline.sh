#!/bin/bash
for x in test/golden/*.cry; do
  if [ ! -f "$x.expected" ]; then
    echo "Generating $x.expected"
    ./gen_expected.sh $x
  fi
done
