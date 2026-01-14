# bun-react-tailwind-template

To install dependencies:

```bash
bun install
```

To start a development server:

```bash
bun dev
```

To run for production:

```bash
bun start
```

This project was created using `bun init` in bun v1.3.5. [Bun](https://bun.com) is a fast all-in-one JavaScript runtime.

## WASM integration

Build the compiler wasm bundle (requires `wasm-bindgen` CLI):

```bash
cargo install wasm-bindgen-cli
bun run wasm:build
```

Then import it from the frontend:

```ts
import { compile } from "./wasm";

const result = await compile(source, ["full"], true);
```
