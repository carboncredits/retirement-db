## 4c-retirement-db

The Irmin database and server for 4c-retirement data.

## Structure

The shape of retirement data is described in `src/atd` using [Adaptable Type Definitions](https://github.com/ahrefs/atd). This allows us to have a single source of types and generate programming-language-specific APIs automatically. `src/ts` contains Typescript bindings and `src/atd` contains OCaml bindings.

The `src/lib` directory is a wrapper around these bindings for producing an Irmin store and `src/bin` is executable for running the Irmin store using a Unix filesystem backend.

## Up and running

To get up and running with the project make sure you have `opam` installed and run `opam update`. Then:

```bash
opam switch create 5.0.0~alpha1
opam pin . -yn
opam install . --deps-only --with-test
dune build @install @runtest
dune exec -- src/bin/main.exe --root=./var
```

This creates a new git-compatible Irmin store in `<CWD>/var`. You can interact with it through the `irmin` cli tool, for example:

```bash
$ irmin get hello/world --root=./var
{ "version": { "major": 0, "minor": 1 }, "details": ... }
```

The default store is on the file-system using a git-compatible format and JSON serialisation, so it is entirely possible to `cd` into the directory and checkout the `main` branch and interact with the files directly and `git commit` changes.
