const worker = new Worker('worker.js');

const execButton = window.document.getElementById("exec");
function enableButton() {
    execButton.innerHTML = 'exec';
    execButton.disabled = false;
}

worker.onmessage = (e) => {
    switch(e.data.type) {
        case 'initialized':
            enableButton();
            break;
        case 'finished':
            window.document.getElementById("result").innerHTML = `elapsed: ${e.data.elapsed_msec} msec`;
            enableButton();
            break;
        case 'stdout_message':
            window.document.getElementById("output_textarea").value += e.data.value;
            break;
        default:
            console.log('unrekognizable message', e);
            break;
    }
}

window.document.getElementById("exec").addEventListener("click", () => {
    window.document.getElementById("output_textarea").value = "";
    execButton.innerHTML = 'executing...';
    execButton.disabled = true;
    worker.postMessage({
        type: 'start',
        value: window.document.getElementById("input_textarea").value,
    });
});