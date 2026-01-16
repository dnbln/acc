let wasmInit: Promise<boolean> | null = null;
let wasmReady = false;

async function initWasm() {
  if (wasmReady) {
    return true;
  }
  if (!wasmInit) {
    wasmInit = (async () => {
      try {
        const mod = await import("./wasm/acc.js");
        const wasmUrl = new URL("./wasm/acc_bg.wasm", window.location.href);
        await mod.default(wasmUrl);
        wasmReady = true;
        return true;
      } catch {
        return false;
      }
    })();
  }
  return wasmInit;
}

export async function compile(
  source: string,
  opts: string[] = [],
  dveEmitWarnings = false
) {
  const ready = await initWasm();
  if (!ready) {
    return null;
  }
  const mod = await import("./wasm/acc.js");
  return mod.compile(source, opts, dveEmitWarnings);
}
