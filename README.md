# PQ Language

A toy-language compiler implementing an ML-like language with quotient types.

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
- [Usage](#usage)
- [License](#license)

## Overview

PQ is an ML-like programming language that integrates quotient-inductive types from type theory and performs SMT-decidable type checking on programs.

After checking, PQ programs can be directly executed or compiled to OCaml modules allowing for quotient type checking to be integrated directly into larger OCaml projects.

## Installation

Ensure you have OPAM (OCaml Package Manager) installed. Then:

```sh
# Clone the repository
git clone https://github.com/ofsouzap/pq-lang.git
cd pq-lang

# Install dependencies
opam install . --deps-only

# Build the project
dune build

# Install the project's executables
opam install .
```

## Usage

### Execution

To check and directly execute a PQ source file:

```sh
pq_exec {source_file}.pq
```

### OCaml Compilation

To compile a PQ source file to an OCaml module:

```sh
pq_to_ocaml {source_file}.pqb
```

#### dune Integration

For larger projects, the PQ checker can be used within a dune project.

To acheive this, place the `.pq` file in the same directory as you want the corresponding OCaml module to seem to exist in.

Then, we add to the relevant `dune` file the following stanza:

```dune
(rule
 (targets {module_name}.mli {module_name}.ml)
 (deps
  (package pq_lang)
  {module_name}.pq)
 (action
  (chdir
   %{workspace_root}
   (run %{bin:pq_to_ocaml} %{deps}))))
```

This will mean that, in the building stage, dune will run `pq_to_ocaml` on the `.pq` file, producing the expected `.ml` and `.mli` outputs, then dune can continue with compilation with these OCaml files.

##### Full Example

TODO

## License

This project is licensed under the MIT License.
