const builtin = @import("builtin");
const std = @import("std");

extern "debug" fn debug_out(channel:usize, ptr: usize, size: usize) void;

const WriterContext = usize;

fn debug_print_wasm(channel: WriterContext, s: [] const u8) error{}!usize {
    debug_out(channel, @ptrToInt(&s[0]), s.len);
    return s.len;
}

pub const Writer = std.io.Writer(WriterContext, error{}, debug_print_wasm);

pub fn wasm_writer(channel: usize) Writer {
    return Writer{.context = channel};
}

const stdout_writer = wasm_writer(0);


pub fn debug_print(comptime fmt: [] const u8, args: anytype) void {
    if (builtin.os.tag == .linux) {
        std.debug.print(fmt, args);
    } else if (builtin.os.tag == .freestanding and builtin.cpu.arch == .wasm32){
        std.fmt.format(stdout_writer, fmt, args) catch debug_print_wasm("format error");
    } else {
        @compileError("unexpected target os");
    }
}