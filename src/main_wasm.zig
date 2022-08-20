const std = @import("std");
const parser = @import("parser.zig");
const eval = @import("eval.zig");
const emit = @import("emit.zig");
const object = @import("object.zig");
const format = @import("format.zig");

pub fn main() void {

}

fn eval_str_raw() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const evaluator = try eval.create_evaluator(allocator);
    defer eval.destroy_evaluator(evaluator);
    var p = parser.Parser{.s = "(display 1)", .p = 0};
    while(true) {
        const obj = try parser.parse(&p, evaluator.pool);
        const emitted_idx = emit.emit_func(evaluator, obj, try object.create_nil(evaluator.pool)) catch |err| {
            //// std.debug.print("error occurred while emmiting code\n", .{});
            return err;
        };
        _ = eval.eval_compiled_global(evaluator, emitted_idx) catch |err| {
            //// std.debug.print("error occurred while executing\n", .{});
            return err;
        };
        try parser.skip_whitespaces(&p);
        if (! parser.is_char_left(&p)) {
            break;
        }
    } else unreachable;
}

export fn eval_str() void {
    eval_str_raw() catch return;
}