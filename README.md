# CS4555 Compiler Construction Project Group

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