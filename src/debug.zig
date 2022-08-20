const builtin = @import("builtin");
const std = @import("std");

extern "debug" fn debug_out(ptr: usize, size: usize) void;

const WriterContext = struct{};

fn debug_print_wasm(_: WriterContext, s: [] const u8) error{}!usize {
    debug_out(@ptrToInt(&s[0]), s.len);
    return s.len;
}

const Writer = std.io.Writer(WriterContext, error{}, debug_print_wasm);
const writer = Writer{.context = .{}};


pub fn debug_print(comptime fmt: [] const u8, args: anytype) void {
    if (builtin.os.tag == .linux) {
        std.debug.print(fmt, args);
    } else if (builtin.os.tag == .freestanding and builtin.cpu.arch == .wasm32){
        std.fmt.format(writer, fmt, args) catch debug_print_wasm("format error");
    } else {
        @compileError("unexpected target os");
    }
}