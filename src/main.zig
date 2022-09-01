const std = @import("std");
const parser = @import("parser.zig");
const eval = @import("eval.zig");
const emit = @import("emit.zig");
const object = @import("object.zig");
const format = @import("format.zig");

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    _ = try args.next(allocator) orelse {
        std.debug.print("get arg error\n", .{});
        return;
    };
    const fname = try args.next(allocator) orelse {
        std.debug.print("get filename error\n", .{});
        return;
    };
    const file = try std.fs.cwd().openFile(fname, .{});
    defer file.close();
    const content = try file.readToEndAlloc(allocator, 10000);
    defer allocator.free(content);
    const evaluator = try eval.create_evaluator(allocator);
    defer eval.destroy_evaluator(evaluator);
    var p = parser.Parser{.s = content, .p = 0};
    while(true) {
        const obj = try parser.parse(&p, evaluator.pool);
        const emitted_idx = emit.emit_func(evaluator, obj, try object.create_nil(evaluator.pool)) catch |err| {
            std.debug.print("error occurred while emmiting code\n", .{});
            return err;
        };
        _ = eval.eval_compiled_global(evaluator, emitted_idx) catch |err| {
            std.debug.print("error occurred while executing\n", .{});
            return err;
        };
        try parser.skip_whitespaces(&p);
        if (! parser.is_char_left(&p)) {
            break;
        }
    } else unreachable;
}

test {
    _ = @import("parser.zig");
    _ = @import("object.zig");
    _ = @import("small_test.zig");
    _ = @import("runs_test.zig");
}


