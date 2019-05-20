#!/bin/bash

current_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )";
cd ${current_dir}

for f in proto/*.proto; do
  rm -f src/$(basename $f .proto)_pb.erl;
  rm -f src/$(basename $f .proto).erl;
  rm -f include/$(basename $f .proto)_pb.hrl;
  rm -rf proto
done
rm -f rebar.lock
rm -rf _build/default/lib/maxwell_*
