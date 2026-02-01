# Note:
This project currently relies on parser architecture by Dr. Jamie Jennings(jajenni3@ncsu.edu), as it started out as a project for my CSC417 - Theory of Programming Languages course at North Carolina State University, but has since outgrown that scope. As I do not wish to take credit for any code that is not expressly my own, her parser is not included in this repository.

I am currently working on my own version of the parser with further functionality and modularity, but until the time I am able to finish it, a large portion of this repository's code is not runnable by other users. As such, I have included a separate folder of curated testscripts that come pre-parsed, so that others may test it on their own. This README includes instructions on how to run those files as well as ones that need the parser. Thank you for your patience while I finish the parser.

# The .417 Interpreter

A statically-typed, lexically-scoped interpreter implemented in Rust. This project features a custom functional language with support for mutable state, higher-order functions (closures), and strict static type checking via a dedicated analysis pass.

## System Architecture

The interpreter operates as a three-stage pipeline designed to decouple parsing, verification, and execution.

1.  **Parsing:** Source code is converted into a JSON-based Abstract Syntax Tree (AST).
2.  **Static Analysis (`typecheck`):** The JSON AST is piped into the type checker. This pass verifies type safety, arity, and return types before execution begins. It implements "Contextual Typing" to infer types where the parser output is incomplete.
3.  **Execution (`interpreter`):** If type checking succeeds, the AST is passed to the interpreter. The runtime environment utilizes `Rc<RefCell<Value>>` to manage shared ownership and interior mutability, enabling variable reassignment within a lexical scope.

## The .417 Language Specification

The language uses a functional syntax with procedural elements. It relies on prefix notation for operations and requires explicit type annotations.

### Core Features

* **Static Typing:** Supports `:int`, `:bool`, `:str`, and `:unit`.
* **Arrow Types:** Functions are typed by their parameters and return value (e.g., `(:int :int -> :int)`).
* **Closures:** Functions capture their definition environment, allowing for persistent state in functional factories.
* **Mutation:** Variables are bound via `let` but can be updated via assignment if the scope permits.

### Syntax Examples

**Variable Binding & Mutation**

```rust
let x :int = 10;
x = 20; // Reassignment allowed due to RefCell architecture
```

**Recursive Functions**

Defined using `def` to handle self-reference within the scope.

```rust
def fact (:int -> :int) = fn(n :int) :int {
   if ==(n, 0) then 1
   else mul(n, fact(sub(n, 1)))
};
```

**Prefix Operations**

Arithmetic and logic utilize functional prefix notation.

```rust
add(5, 10)       // 5 + 10
==(x, y)         // x == y
print("Result")  // Standard output
```

## Build Instructions

The project includes a build script that compiles both the interpreter and typechecker binaries in release mode.

```bash
./build.sh
```

Prerequisites: Rust (cargo) must be installed. The script will attempt to install it via rustup if missing.

## Usage & Test Suites

Execution is handled via the `run.sh` script, which pipes stdin through the type checker and then to the interpreter.

### Running Without the External Parser

If you do not have the parse tool installed, you can use the pre-parsed JSON ASTs located in the `preparsed_scripts/` directory. You must pipe the file contents into the runner (do not try to execute the text file directly)..

**Using cat:**

```bash
cat preparsed_scripts/true_parser_preparsed.txt | ./run.sh
```

**Using Input Redirection:**

```bash
./run.sh < preparsed_scripts/true_parser_preparsed.txt
```

### 1. Recursion (Factorial)

Demonstrates recursive definitions and conditional logic. File: `testscripts/type-fact-let.417.txt`

```bash
parse -f testscripts/type-fact-let.417.txt | ./run.sh
```

**Output:**

```text
720
```

### 2. Complex String Manipulation (Self-Hosted Parser)

A parser written in the .417 language itself. This stress-test demonstrates deep recursion, string manipulation (substring, indexOf), and nested let blocks. File: `testscripts/true_parser.txt`

```bash
parse -f testscripts/true_parser.txt | ./run.sh
```

**Output (Truncated JSON AST):**

```json
{"Application":{"Operator":{"Identifier":"print"},"Operands":[{"Application":{"Operator":{"Identifier":"div"},"Operands":[{"Application":{"Operator":{"Identifier":"add"},"Operands":[{"Application":{"Operator":{"Identifier":"mul"},"Operands":[{"Number":5},{"Number":-1}]}},{"Application":{"Operator":{"Identifier":"sqrt"},"Operands":[{"Application":{"Operator":{"Identifier":"sub"},"Operands":[{"Application":{"Operator":{"Identifier":"pow"},"Operands":[{"Number":5},{"Number":2}]}},{"Application":{"Operator":{"Identifier":"mul"},"Operands":[{"Number":4},{"Application":{"Operator":{"Identifier":"mul"},"Operands":[{"Number":1},{"Number":4}]}}]}}]}}]}}]}},{"Application":{"Operator":{"Identifier":"mul"},"Operands":[{"Number":2},{"Number":1}]}}]}}]}}
```

### 3. Static Type Enforcement (Bank Account)

This test case demonstrates the type checker preventing runtime errors. The source attempts to return an integer where a string is expected in the function signature. File: `testscripts/Q6.txt`

```bash
parse -f testscripts/Q6.txt | ./run.sh
```

**Output:**
```text
Type Error: Function body type mismatch. Declared return (:str :int -> :str), got (:str :int -> :int)
```

## Final Secret
Though the main parser may not be finished, I have implemented a limited parser within the 417 language itself. This can be seen by running `cat preparsed_scripts/Q6_preparsed.txt | ./run.sh`, which will interpret the parser. Therefore, if you run `cat preparsed_scripts/Q6_preparsed.txt | ./run.sh | ./run.sh`, running the interpreter on itself, you might see something kinda cool happen.