#!/bin/bash

readonly SCRIPT_ROOT=$(cd $(dirname ${0}); pwd)

main() {
    dotnet tool restore
    cp pre-commit $SCRIPT_ROOT/.git/hooks
}

main
