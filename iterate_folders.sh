#!/usr/bin/env bash
set -uo pipefail

compiler=llvm
export FFLAGS="-fopenmp -fopenmp-targets=nvptx64-nvidia-cuda"

for dir in */; do
    [ -d "$dir" ] || continue
    echo "Folder: $dir"

    (
        cd "$dir"
        make $compiler
        ./build/main
    )

    if [[ $? -ne 0 ]]; then
        echo "Failed in $dir"
    else
        echo "Success in $dir"
    fi

done
