#!/bin/bash

# Exit on any error
set -e

# Read stdin into a variable so we can use it twice
input=$(cat)

# Run Type Checker
# Pass the input to the type checker.
# If it fails (non-zero exit), the script exits (due to set -e) and prints the error from typecheck.
echo "$input" | ./interpreter/target/release/typecheck

# Run Interpreter
# If type check passed, run the actual interpreter
echo "$input" | ./interpreter/target/release/interpreter