# 🌲 The Pine Programming Language

Pine is a **statically typed, compiled programming language** designed for learning, experimentation, and building a clean understanding of how compilers work. It features a simple syntax, first-class functions, and a clear compilation pipeline that exposes each stage—from tokenization to LLVM IR generation.

This project includes a CLI compiler called **`pinec`**, which can build, analyze, and run Pine programs.

---

## Language Features

* Statically typed
* First-class functions
* Explicit function signatures
* Local variables with type annotations
* Arithmetic expressions
* Multiple compiler stages (tokens, AST, LLVM IR)
* Native executable output

---

## 📦 Project Structure

```text
pine/
├── examples/
│   └── main.alp
├── crates/
│   ├── lexer/
│   ├── parser/
│   ├── analyzer/
│   ├── linker/
│   ├── codegen/
│   └── cli/
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

fn main() -> i32 {
    let result: i32 = add(10, 20);
    return result;
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
* Produce a native executable

The output binary will be created at:

```bash
./examples/main
```

---

### Run the compiled program

```bash
./examples/main
```

---

## Compiler Stages (Debug & Inspect)

One of Pine’s goals is **transparency**. You can inspect each compiler stage using subcommands.

---

### Tokenization

Generate and print the token stream:

```bash
pinec build -s examples/main.alp tokens
```

Useful for:

* Debugging lexer issues
* Understanding how source code is tokenized

---

### Abstract Syntax Tree (AST)

Generate the AST and export it as a **Graphviz DOT file**:

```bash
pinec build -s examples/main.alp ast
```

You can visualize it with:

```bash
dot -Tpng ast.dot -o ast.png
```

---

### LLVM IR Generation

Generate LLVM Intermediate Representation:

```bash
pinec build -s examples/main.alp llvm-ir
```

This is useful for:

* Learning LLVM
* Debugging code generation
* Future optimizations

---

## 🧠 Type System Overview

* Primitive types:

  * `i32`
  * `i64`
  * `u32`
  * `u64`
  * `boolean`
  * `unit`
* Functions are first-class values
* Function calls are type-checked using **structural unification**
* All identifiers must be defined before use

Example:

```pine
fn mul(a: i32, b: i32, c: i32) -> i32 {
    return a * b * c
}
```

---

## 🧪 Example Program

```pine
fn main() -> i32 {
    let sum: i32 = add(10, 20);
    let diff: i32 = sub(40, 10);
    let prod: i32 = mul(1, 2, 3);
    let quo: i32 = div(8, 4);
    return sum + diff + prod + quo;
}
```

Output:

```text
68
```

---

## 🤝 Contributing

Contributions are welcome!

* Open issues for bugs or feature requests
* Submit pull requests with clear descriptions
* Keep changes small and focused

---

## 📜 License

Apache License Version 2.0

---

## 🌲 Final Note

Pine is a learning-focused language, but it is built with **real compiler principles**. If you’re interested in compilers, type systems, or LLVM, this project is meant to grow with you.

Happy hacking 🌲
