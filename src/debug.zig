const builtin = @import("builtin");
const std = @import("std");

pub fn debug_print(comptime fmt: [] const u8, args: anytype) void {
    if (builtin.os.tag == .linux) {
        std.debug.print(fmt, args);
    } else if (builtin.os.tag == .freestanding and builtin.cpu.arch == .wasm32){
        
    } else {
        @compileError("unexpected target os");
    }
}