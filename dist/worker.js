
let wasm;

const debug = {
    debug_out(channel, ptr, size) {
        // ascii only
        var result = "";
        const array = new Uint8Array(wasm.instance.exports.memory.buffer);
        for (var i =0; i< size; i++) {
            result += String.fromCharCode(array[ptr + i]);
        }
        postMessage({
            type: 'stdout_message',
            value: result,
        })
    }
}


async function startWasm() {
    const response = await fetch('main_wasm.wasm');
    wasm = await WebAssembly.instantiate(await response.arrayBuffer(), {
        debug,
    });
    console.log(wasm);
    wasm.instance.exports._start();
    postMessage({
        type: 'initialized',
    })
}

function evalStr(s) {
    const size = s.length;
    const ptr = wasm.instance.exports.alloc_str(size);
    const array = new Uint8Array(wasm.instance.exports.memory.buffer);
    for (var i =0; i< size; i++) {
        array[ptr + i] = s.charCodeAt(i);
    }
    const begin = performance.now();
    wasm.instance.exports.eval_str(ptr, size);
    const end = performance.now();
    wasm.instance.exports.free_str(ptr, size);
    postMessage({
        type: 'finished',
        elapsed_msec: end - begin,
    })
}
onmessage = (e) => {
    switch(e.data.type) {
        case 'start':
            evalStr(e.data.value);
            break;
        default:
            console.log('unrekognizable message');
            break;
    }
}

startWasm();
