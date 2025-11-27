import url from "node:url";
import fs from "node:fs";
import path from "node:path";

const __dirname = url.fileURLToPath(new URL(".", import.meta.url));

const memory = new WebAssembly.Memory({ initial: 1 });

function readUnalignedVec(ptr: number, len: number) {
  const ELEMENT_SIZE = 4;
  const view = new DataView(memory.buffer);
  const result: number[] = [];

  for (let i = 0; i < len; i++) {
    const offset = ptr + i * ELEMENT_SIZE;

    // DataView.getInt32 reads 4 bytes starting at the offset,
    // assembling them into an i32 value using Little-Endian order.
    const value = view.getInt32(offset, true);

    result.push(value);
  }

  return result;
}

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

const { malloc, free, new_vec, push } = instance.exports as any;

const ptr1 = malloc(100);
console.log(`Allocated at: ${ptr1}`);

const ptr2 = malloc(50);
console.log(`Allocated at: ${ptr2}`);

free(ptr1);

const ptr3 = malloc(65536 - 32);
console.log(`Reused space at: ${ptr3}`); // Should equal ptr1

let vec = new_vec(4);
vec = push(...vec, 1262);
vec = push(...vec, 112358);
console.log(`Vec: ${vec}`);
console.log("data:", readUnalignedVec(vec[0], vec[2]));
vec = push(...vec, 4);
vec = push(...vec, 5);
vec = push(...vec, 6);
console.log(`Vec: ${vec}`);
console.log("data:", readUnalignedVec(vec[0], vec[2]));
