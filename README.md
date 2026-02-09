# 🌲 The Pine Programming Language

Pine is a statically typed, compiled programming language focused on high performance, minimal design, and a modern developer experience.

It features a clean syntax, explicit typing, LLVM-based code generation, and a transparent compilation pipeline that exposes every stage - from tokenization to native executable output.

This project includes a CLI compiler called **`pinec`**, which can build, analyze, and run Pine programs.

---

## Language Features

Pine has evolved beyond arithmetic and functions into a small but expressive systems-style language core.

### Core Language

* Statically typed
* Compiled to native executables (via LLVM)
* First-class functions
* Explicit function signatures
* Local variables & reassignment
* Arithmetic & comparison expressions
* Conditional branching (`if / else if / else`)
* `while` loops
* Expression-based returns

---

### Primitive Types

* `i32`
* `i62`
* `f32`
* `f64`
* `bool`
* `str`
---

### Strings

* Constant string literals
* String comparison (`==`, `!=`)
* Runtime-backed operations
* Designed for future evolution into heap strings

---

### Foreign Function Interface (FFI)

Pine supports calling native functions using the **`alien`** keyword:

```pine
alien "c" fn pine_print(s: str) -> i32
```

This enables direct interoperability with C (and other C-ABI languages).

---

### Standard I/O (via runtime + FFI)

Example wrappers:

```pine
fn print(s: str) {
    pine_print(s)
}

fn println(s: str) {
    pine_println(s)
}
```

You can also read input:

```pine
let name = read("Enter your name:")
println(name)
```

---

## Compilation Pipeline

Pine exposes its compilation stages for learning and debugging:

1. **Lexing** → Token stream
2. **Parsing** → AST
3. **Semantic Analysis** → Type checking
4. **Codegen** → LLVM IR
5. **Object emission**
6. **Linking** → Native executable (with runtime)

---

## 📦 Project Structure

```text
pine/
├── examples/
│   └── main.alp
├── runtime/
│   └── c/
├── crates/
│   ├── lexer/
│   ├── parser/
│   ├── analyzer/
│   ├── codegen/
│   ├── linker/
│   └── utils/
├── tools/
│   └── pinec/
```

---

## Getting Started

### Write a Pine program

Create a file with the `.alp` extension:

```pine
fn add(n1: i32, n2: i32) -> i32 {
    return n1 + n2
}

fn main() {
    let result = add(10, 20)
    return result
}
```

---

## Using the Compiler CLI (`pinec`)

### Compile a program

```bash
pinec build -s examples/main.alp
```

This will:

* Lex the source
* Parse it into an AST
* Type-check the program
* Generate LLVM IR
* Emit object code
* Link runtime & produce a native executable

Output binary:

```bash
./examples/main
```

---

### Run the compiled program

```bash
./examples/main
```

---

## Compiler Stages (Inspect & Debug)

Transparency is a core Pine goal - you can inspect every stage.

---

### Tokenization

```bash
pinec build -s examples/main.alp tokens
```

Use cases:

* Debug lexer issues
* Study token output

---

### Abstract Syntax Tree (AST)

```bash
pinec build -s examples/main.alp ast
```

Visualize:

```bash
dot -Tpng ast.dot -o ast.png
```

---


### TypeCheck Analysis

```bash
pinec build -s examples/main.alp analyze
```
Useful for:

* Populating infered types
* Debugging AST

### Object Creation

```bash
pinec build -s examples/main.alp object
```
Useful for:

* Creating object file
* Custom linking with other FFI languages


### LLVM IR

```bash
pinec build -s examples/main.alp llvm-ir
```

Useful for:

* Learning LLVM
* Debugging codegen
* Optimization research

---

## Example Programs

---

### String Comparison

```pine
fn main() {
    let a = "hello"
    let b = "world"

    return if a != b { 1 } else { 0 }
}
```

---

### Functions + Control Flow

```pine
fn is_even(n: i32) -> bool {
    return n % 2 == 0
}

fn main() {
    let x = 10

    if is_even(x) {
        return 1
    }

    return 0
}
```

---

### Interactive Input (requires alien function definition)

```pine
fn main() {
    let res = read("Are you a one or a zero?")
    println(res)
    return 0
}
```

---

##  Alien Functions (FFI)

Declare foreign functions using `alien`:

```pine
alien "c" fn pine_print(s: str) -> i32
alien "c" fn pine_read_line_stdin(prompt: str) -> str
```

Then wrap them in Pine:

```pine
fn read(prompt: str) -> str {
    pine_read_line_stdin(prompt)
}
```

This enables:

* Native runtime libraries
* libc integration
* Custom system bindings

---

## Runtime Architecture

Runtime functions are implemented in C and linked during compilation.

Examples:

* `pine_print`
* `pine_println`
* `pine_strcmp`
* `pine_read_line_stdin`

This modular design allows selective linking and smaller binaries.

---

## Design Goals

* Learn compiler internals
* Explore LLVM IR generation
* Experiment with language features
* Maintain transparent compilation stages
* Enable gradual evolution toward a self-hosting language

---

## Roadmap

Planned / future features:

* Heap strings (`struct { ptr, len }`)
* String methods (concat, slice, length)
* Structs & custom types
* Arrays / slices
* File I/O
* Modules & stdlib packaging
* Self-hosting compiler (long-term)

---

## Contributing

Contributions are welcome!

* Open issues for bugs or feature ideas
* Submit PRs with focused changes
* Discuss language design improvements

---

## License

Apache License Version 2.0

---

## 🌲 Final Note

Pine is a learning-focused language built with **real compiler principles**:

* LLVM backend
* Native codegen
* Runtime linking
* FFI support
* Transparent compilation stages

If you’re exploring compilers, language design, or systems programming - Pine is meant to grow with you.

Happy hacking 🌲
