import url from "node:url";
import fs from "node:fs";
import path from "node:path";

const __dirname = url.fileURLToPath(new URL(".", import.meta.url));

const memory = new WebAssembly.Memory({ initial: 1 });

function log(offset: number, length: number) {
  const bytes = new Uint8Array(memory.buffer, offset, length);
  const string = new TextDecoder("utf8").decode(bytes);
  console.log(string);
}

const { module: _, instance } = await WebAssembly.instantiate(
  fs.readFileSync(path.resolve(__dirname, "3_heap.wasm")),
  {
    console: { log },
    js: { mem: memory },
  },
);

const { malloc, free } = instance.exports as any;

const ptr1 = malloc(100);
console.log(`Allocated at: ${ptr1}`);

const ptr2 = malloc(50);
console.log(`Allocated at: ${ptr2}`);

free(ptr1);

const ptr3 = malloc(20);
console.log(`Reused space at: ${ptr3}`); // Should equal ptr1
