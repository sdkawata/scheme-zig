
let wasm;

const debug = {
    debug_out(ptr, size) {
        // ascii only
        var result = "";
        const array = new Uint8Array(wasm.instance.exports.memory.buffer);
        for (var i =0; i< size; i++) {
            result += String.fromCharCode(array[ptr + i]);
        }
        window.document.getElementById("output_textarea").value += result;
    }
}


async function startWasm() {
    const response = await fetch('main_wasm.wasm');
    wasm = await WebAssembly.instantiate(await response.arrayBuffer(), {
        debug,
    });
    console.log(wasm);
    wasm.instance.exports._start();
}

function evalStr(s) {
    const size = s.length;
    const ptr = wasm.instance.exports.alloc_str(size);
    console.log(ptr, size);
    const array = new Uint8Array(wasm.instance.exports.memory.buffer);
    for (var i =0; i< size; i++) {
        array[ptr + i] = s.charCodeAt(i);
    }
    wasm.instance.exports.eval_str(ptr, size);
    wasm.instance.exports.free_str(ptr, size);
}

startWasm();

window.document.getElementById("exec").addEventListener("click", () => {
    window.document.getElementById("output_textarea").value = "";
    evalStr(window.document.getElementById("input_textarea").value);
});