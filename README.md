# :night_with_stars: Nox

**Nox** is a small functional, statically typed scripting language that looks and feels like a dynamic one thanks to powerful *type inference*.

The language can either be used interactively in its own [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop), or compiled to [Lua](https://www.lua.org/) source code.

## Example

Compute the factorial of a number `n` with a recursive function in Nox:

```
fn fact(n) {
  if n <= 1 {
    1
  } else {
    fact(n - 1) * n
  }
};

fact(5)
```

## Installation

To build and install Nox, you will first need to install the [opam](https://opam.ocaml.org/) package manager for the [OCaml](https://ocaml.org/) programming language on your machine.

Once this is done, simply run the following commands to clone this repository, create a local opam switch for the project and install Nox on your computer:

```
$ git clone git@github.com:coetaur0/nox.git
$ cd nox
$ opam switch create . ocaml.4.14.0
$ dune install
```

## Usage

To launch the Nox REPL, simply run the command `nox` from your terminal.

If you prefer to compile a Nox program to Lua, you can call the `nox` command followed by the path to the source file you would like to compile.
This will emit a new Lua file with the same name as your Nox file (but with a `.lua` extension), in the same folder as the original source.
For example, the command:
```
$ nox fact.nox
```
will emit a new `fact.lua` file in the folder where your `fact.nox` file is located. 
