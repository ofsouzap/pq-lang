# PQ Language

A toy-language compiler implementing an ML-like language with quotient types.

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
- [Usage](#usage)
- [License](#license)

## Overview

PQ is toy programming language that explores the integration of quotient types from type theory into an ML-style language.

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
```

## Usage

To compile a PQ source file:

```sh
dune exec pq_lang -- source_file.pq
```

## License

This project is licensed under the MIT License.
