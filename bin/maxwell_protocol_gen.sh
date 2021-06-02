#!/bin/bash

current_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )";
cd ${current_dir}

# Fetch proto files
if [[ ! -d proto ]]; then
    git clone https://github.com/maxwell-dev/maxwell-protocol.git proto;
fi

# Generate pb files
root_dir=${current_dir}
if [[ ${root_dir} =~ (.*)_build.*$ ]]; then
    root_dir=${BASH_REMATCH[1]};
fi
mkdir -p include src
chmod a+x ${root_dir}/_build/default/lib/gpb/bin/protoc-erl
${root_dir}/_build/default/lib/gpb/bin/protoc-erl \
  -Iproto \
  -o-erl src \
  -o-hrl include \
  -modsuffix _pb \
  -mapfields-as-maps \
  -strbin \
  proto/*.proto
unamestr=`uname`
for f in src/*_pb.erl; do
    if [[ "$unamestr" == 'Linux' ]]; then
        sed -i 's%include("gpb.hrl")%include_lib("gpb\/include\/gpb.hrl")%g' ${f}
    elif [[ "$unamestr" == 'Darwin' ]]; then
        sed -i '' 's%include("gpb.hrl")%include_lib("gpb\/include\/gpb.hrl")%g' ${f}
    fi
done

# Generate api files
bin/maxwell_protocol_gen_api.py \
    --proto_file proto/maxwell_protocol.proto \
    --enum_type_names msg_type_t