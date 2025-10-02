#!/bin/bash

function build_example () {
    echo "Building $1"
    sbcl --eval "(require \"asdf\")" \
         --eval "(asdf:make \"zmq-examples/$1\")" \
         > /dev/null
}

rm -rf bin
mkdir bin

build_example hwserver
build_example hwclient
build_example version
build_example wuserver
build_example wuclient
build_example rrclient
build_example rrworker
build_example rrbroker

