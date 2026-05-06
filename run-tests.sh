#!/usr/bin/env bash
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$ROOT_DIR/build"
TESTS_DIR="$ROOT_DIR/tests"
BUILD_TESTS_DIR="$BUILD_DIR/tests"

if [[ ! -d "$BUILD_DIR" ]]; then
    echo "[FAIL] Build directory does not exist: $BUILD_DIR"
    echo "Configure and build first, then run this script."
    exit 1
fi

mapfile -t TEST_CASES < <(
    find "$TESTS_DIR" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort
)

echo "Using build directory: $BUILD_DIR"

for test_case in "${TEST_CASES[@]}"; do
    echo ""
    echo "[TEST] $test_case"

    exe_path="$BUILD_TESTS_DIR/$test_case/$test_case"
    if [[ ! -x "$exe_path" ]]; then
        echo "Test executable not built: $BUILD_TESTS_DIR/$test_case"
        continue
    fi

    OMP_TARGET_OFFLOAD=mandatory "$exe_path"
done
