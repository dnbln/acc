#!/usr/bin/env bun
import { $ } from "bun";
import { mkdir, rm } from "fs/promises";
import path from "path";

const playgroundRoot = path.resolve(import.meta.dir);
const repoRoot = path.resolve(playgroundRoot, "..");
const outDir = path.join(playgroundRoot, "src", "wasm");
const wasmPath = path.join(
  repoRoot,
  "target",
  "wasm32-unknown-unknown",
  "debug",
  "acc.wasm"
);

await rm(outDir, { recursive: true, force: true });
await mkdir(outDir, { recursive: true });

await $`cargo build --target wasm32-unknown-unknown --features wasm --lib`.cwd(repoRoot);
await $`wasm-bindgen --target web --no-typescript --out-dir ${outDir} ${wasmPath}`.cwd(
  repoRoot
);

console.log(`✅ WASM artifacts written to ${outDir}`);
