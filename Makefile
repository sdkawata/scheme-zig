test:
	zig test src/main.zig
wasm:
	zig build-exe -O ReleaseSmall -target wasm32-freestanding src/main_wasm.zig
	mv main_wasm.wasm dist/