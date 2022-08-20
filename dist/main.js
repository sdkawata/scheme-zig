async function startWasm() {
    const response = await fetch('main_wasm.wasm');
    const instance = await WebAssembly.instantiate(await response.arrayBuffer(), {});
    console.log(instance);
    console.log(instance.instance.exports.one());
}

startWasm();