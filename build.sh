#!/bin/bash

# Exit on any error
set -e

# Check if Rust is installed, install if not
if ! command -v cargo &> /dev/null; then
    echo "Installing Rust..."
    sudo apt-get install -y curl
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source $HOME/.cargo/env
else
    echo "Rust is already installed."
fi

# Navigate to the interpreter directory
cd interpreter

# Build the main interpreter (release)
echo "Building interpreter..."
cargo build --release --bin interpreter

# Build the typechecker (release)
echo "Building typecheck..."
cargo build --release --bin typecheck

# Check if the build was successful
if [ $? -eq 0 ]; then
    echo "Build completed successfully."
else
    echo "Build failed."
    exit 1
fi