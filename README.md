# CS4555 Compiler Construction Project Group

**Responsible TA**: Arthur Jacques


To run, just do `cargo run`

## Compiling with the LLVM Backend

Set the environment variable `LLVM_SYS_211_PREFIX` to the path to a local LLVM 21.1.* installation.
`$LLVM_SYS_211_PREFIX/bin/llvm-config` should be available to configure the compilation of this project.

Then run with the `--features llvm-backend` flag:

```sh
cargo run --features llvm-backend
```
