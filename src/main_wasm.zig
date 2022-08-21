const std = @import("std");
const parser = @import("parser.zig");
const eval = @import("eval.zig");
const emit = @import("emit.zig");
const object = @import("object.zig");
const format = @import("format.zig");
const debug = @import("debug.zig");
const debug_print = debug.debug_print;

pub fn main() void {
}

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

fn eval_str_raw(s: [] const u8) !void {
    const evaluator = try eval.create_evaluator(allocator);
    defer eval.destroy_evaluator(evaluator);
    var p = parser.Parser{.s = s, .p = 0};
    while(true) {
        const obj = parser.parse(&p, evaluator.pool) catch |err| {
            debug_print("error occurred while parsing code\n", .{});
            return err;
        };
        const emitted_idx = emit.emit_func(evaluator, obj, try object.create_nil(evaluator.pool)) catch |err| {
            debug_print("error occurred while emmiting code\n", .{});
            return err;
        };
        _ = eval.eval_compiled_global(evaluator, emitted_idx) catch |err| {
            debug_print("error occurred while executing\n", .{});
            return err;
        };
        try parser.skip_whitespaces(&p);
        if (! parser.is_char_left(&p)) {
            break;
        }
    } else unreachable;
    try eval.print_funcs(debug.wasm_writer(1), evaluator);
}

export fn eval_str(ptr: usize, size: usize) void {
    const s = @intToPtr([*]u8, ptr)[0..size];
    eval_str_raw(s) catch |err|{
        debug_print("error:{}\n", .{err});
        return;
    };
}

export fn alloc_str(size: usize) usize {
    const ptr = allocator.alloc(u8, size) catch {
        debug_print("allocation failed\n", .{});
        return 0;
    };
    return @ptrToInt(&ptr[0]);
}

export fn free_str(ptr: usize, size: usize) void {
    allocator.free(@intToPtr([*]u8, ptr)[0..size]);
}