const std = @import("std");
const parser = @import("parser.zig");
const eval = @import("eval.zig");
const object = @import("object.zig");

const TestError = error {
    Error
};

test "execute small test" {
    const allocator = std.testing.allocator;
    const file = try std.fs.cwd().openFile("./tests/small_tests.scm", .{});
    defer file.close();
    const content = try file.readToEndAlloc(allocator, 100000);
    defer allocator.free(content);

    const evaluator = try eval.create_evaluator(allocator);
    defer eval.destroy_evaluator(evaluator, allocator);
    const parsed = try parser.parse_string(content, evaluator.pool);
    var current = parsed;
    while (object.obj_type(current) != .nil) {
        if (object.obj_type(current) != .cons) {
            std.debug.print("illegal test file: input is not list\n", .{});
            return TestError.Error;
        }
        const expr = object.get_car(object.get_car(current));
        const expected = object.get_car(object.get_cdr(object.get_car(current)));
        const result = eval.eval_global(evaluator, expr) catch |err| {
            const formatted = try object.format(evaluator.pool, expr, allocator);
            defer allocator.free(formatted);
            std.debug.print("eval error while evaling: {s}\n", .{formatted});
            return err;
        };
        std.debug.print("a\n", .{});
        if (! object.equal(expected, result)) {
            const formatted = try object.format(evaluator.pool, expr, allocator);
            defer allocator.free(formatted);
            const result_formatted = try object.format(evaluator.pool, result, allocator);
            defer allocator.free(result_formatted);
            const expected_formatted = try object.format(evaluator.pool, expected, allocator);
            defer allocator.free(expected_formatted);
            std.debug.print("eval error mismatch while evaling:{s}\n", .{formatted});
            std.debug.print("expected:{s}\n", .{expected_formatted});
            std.debug.print("actual:{s}\n", .{result_formatted});
            return TestError.Error;
        }
        current = object.get_cdr(current);
    }
}