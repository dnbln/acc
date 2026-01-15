# WASM Compiler Interface

The compiler is exposed to the playground via a WebAssembly (WASM) module. The interface
provides a single `compile` entrypoint that returns AST, CFG, Graphviz, and diagnostics
in one structured response.

## Build the WASM bundle

From the repo root:

```sh
rustup target add wasm32-unknown-unknown
cargo install wasm-bindgen-cli
bun run wasm:build
```

This generates `playground/src/wasm/acc.js` and `playground/src/wasm/acc_bg.wasm`, which
are loaded by `playground/src/wasm.ts`.

## API

### `compile(source: string, opts: string[], dveEmitWarnings: boolean) -> object`

Runs parse -> semantic analysis -> CFG lowering -> optimization pipeline.

#### `opts` values

Pass names match the CLI `--opt` flag:

`full`, `none`, `cp`, `dve`, `cpdvetdb`, `bi`, `hp`, `ps`, `vi`, `vips`, `phi2sel`, `bd`, `tu`, `tdb`

Rules:
- If `opts` is empty, the full pipeline runs.
- Use only one of `full` or `none` or an explicit list of pass names.
- `dveEmitWarnings` toggles warnings from the Dead Value Elimination pass.

## Optimization options

- `full`: predefined pipeline from `OptPassConfig::full` (order: `vips, cpdvetdb, bi, hp, vips, phi2sel, bd, tu, dve, tdb, bi, phi2sel, bd, tu, tdb, bi`).
- `none`: run no optimization passes.
- `cp`: constant propagation; folds constants and propagates constant values through the CFG.
- `dve`: dead value elimination; removes assignments to values that are not live.
- `cpdvetdb`: loop of constant propagation, dead value elimination, and trim-dead-blocks until no changes.
- `bi`: block inliner; inlines a block into its predecessor when it has a single predecessor (and a few structured branch cases).
- `hp`: hoist pass; hoists repeated pure expressions to a common dominating block when safe.
- `ps`: phi simplification; replaces phi nodes with identical sources by simple assignments.
- `vi`: value inliner; replaces uses of values assigned from other values, chasing equality chains.
- `vips`: loop of phi simplification and value inliner until no changes.
- `phi2sel`: converts certain two-predecessor phi nodes into a `select` instruction under a matching branch pattern.
- `bd`: block deduplication; merges empty blocks with identical tails (when safe with phi nodes).
- `tu`: tail unification; replaces conditional branches that go to the same target with an unconditional branch.
- `tdb`: trim dead blocks; removes unreachable blocks and relinks the CFG.

## Return shape

```ts
type Diagnostic = {
  range: { start: number; end: number } | null;
  error_name: string;
  severity: "error" | "warning";
};

type CompileResult = {
  ok: boolean;
  ast: object | null;
  cfg_text: string | null;
  graphviz: string | null;
  errors: Diagnostic[];
  warnings: Diagnostic[];
};
```

### Fields

- `ok`: `true` when parsing/sema/lowering succeeded. `false` otherwise.
- `ast`: structured AST object (only on success).
- `cfg_text`: human-readable CFG string for all functions (only on success).
- `graphviz`: DOT output for all CFGs (only on success).
- `errors`: structured diagnostics (parse/sema/lowering errors).
- `warnings`: structured diagnostics (CFG warnings; DVE warnings when enabled).

### AST structure

The AST is serialized directly from the Rust parser types:

- `Program` has `items: Spanned<TopLevel>[]`
- `Spanned<T>` has `{ node: T, span: { start, end } }`
- `TopLevel` is either `Function(Function, VarId)` or `GlobalVar { ... }`
- `Expr`, `Stmt`, `Type`, `BinaryOp`, `UnaryOp`, `VarId`, `RefId` mirror the Rust enums/structs

`VarId`/`RefId` are serialized as a single number. All spans are byte offsets into the input.

### Ranges

`range` is expressed in byte offsets into the original `source` string. When a specific
span is not available (e.g., some CFG validation errors), the range is `null`.

## How to call from the playground

Use the wrapper in `playground/src/wasm.ts`:

```ts
import { compile } from "./wasm";

const result = await compile("int main() { return 0; }", ["full"], true);
```

`compile` returns the object described above. Use `cfg_text` for a console view and
`graphviz` to render with a Graphviz library in the browser.
