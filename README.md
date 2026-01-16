# CS4555 Compiler Construction Project Group 15

**Group members**:
- James Bingen
- Dinu Blanovschi
- Yigit Çolakoğlu


**Responsible TA**: Arthur Jacques

[Link to the Report](./Report_CC.pdf).

To run, just do `cargo run`

## Compiling with the LLVM Backend

Set the environment variable `LLVM_SYS_211_PREFIX` to the path to a local LLVM 21.1.* installation.
`$LLVM_SYS_211_PREFIX/bin/llvm-config` should be available to configure the compilation of this project.

Then run with the `--features llvm-backend` flag:

```sh
cargo run --features llvm-backend
```


## Testing

Refer to the testing section in the report.

TL;DR:

```sh
cargo test
```

Or

```sh
cargo test --features llvm-backend
```

To run the LLVM backend tests as well.

## WebAssembly build (AST/CFG/Graphviz)

The wasm bindings are behind the `wasm` feature and expose:
- `parse(source: &str) -> String`
- `compile(source: &str, opts: Vec<String>, dve_emit_warnings: bool) -> JsValue`

`opts` accepts the same pass names as the CLI `--opt` flag: `full`, `none`, `cp`, `dve`,
`cpdvetdb`, `bi`, `hp`, `ps`, `vi`, `vips`, `phi2sel`, `bd`, `tu`, `tdb`.

`compile` returns a structured object:
`{ ok, cfg_text, graphviz, errors: [{ range, error_name, severity }], warnings: [{ range, error_name, severity }] }`.

Build with:

```sh
cargo build --target wasm32-unknown-unknown --features wasm --lib
```

If you see missing target errors, install it first:

```sh
rustup target add wasm32-unknown-unknown
```

## Running the web frontend

```sh
cd playground
bun i
bun run dev
```
