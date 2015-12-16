!/usr/bin/env bash

function run() {
  mono "$@"
}

run .paket/paket.bootstrapper.exe
run .paket/paket.exe restore