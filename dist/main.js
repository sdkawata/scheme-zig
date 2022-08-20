
let memory;

const debug = {
    debug_out(ptr, size) {
        // ascii only
        var result = "";
        const array = new Uint8Array(memory.buffer);
        for (var i =0; i< size; i++) {
            result += String.fromCharCode(array[ptr + i]);
        }
        console.log(ptr, size, result);
    }
}


async function startWasm() {
    const response = await fetch('main_wasm.wasm');
    const wasm = await WebAssembly.instantiate(await response.arrayBuffer(), {
        debug,
    });
    console.log(wasm);
    memory = wasm.instance.exports.memory;
    console.log(wasm.instance.exports._start());
    console.log(wasm.instance.exports.eval_str());
    console.log(memory);
}

startWasm();