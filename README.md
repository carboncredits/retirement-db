4c-retirement-db
----------------

The Irmin database and server for 4c-retirement data. To get up and running with the project make sure you have `opam` installed and run `opam update`. Then:

```bash
opam switch create 5.0.0~alpha1
opam pin . -yn
opam install . --deps-only --with-test
dune build @install @runtest
dune exec -- src/bin/main.exe --root=./var
```

This creates a new git-compatible Irmin store in `<CWD>/var`. You can interact with it either through the GraphQL interface, for example:

```graphql
mutation {
  set(info: {parents: [], allow_empty: false, retries: 1, message: "Hello", author: "Me"}, value: {version: "v0", json: ""}, path: "hello/world", branch: "main") {
    hash
  }
}
```

Or using the `irmin` cli tool, for example:

```bash
$ irmin get hello/world --root=./var
{"version":"v0","json":""}
```

The default store is on the file-system using a git-compatible format and JSON serialisation, so it is entirely possible to `cd` into the directory and checkout the `main` branch and interact with the files directly and `git commit` changes.



