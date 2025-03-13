# PF5 project

## `opam` installation

To begin, install the package manager [`opam`](https://opam.ocaml.org/) following the instructions given [here](https://opam.ocaml.org/doc/Install.html).

## Package installation

Go to the cloned directory.
From there, run the following commands, which create a local `opam` switch by installing the necessary packages:

```
opam update
opam switch create . 4.14.1 -y --deps-only
```

## Compiling

To compile the project, run the `make` command.

## Toplevel

For your own testing and debugging, you can use the `utop` toplevel that has been installed.
To launch it, run the `make top` command.

## Tests

To run all available tests, execute `make test`.
To test only the functions of exercise *i*, run `make test-i`.
