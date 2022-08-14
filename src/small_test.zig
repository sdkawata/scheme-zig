const std = @import("std");
const parser = @import("parser.zig");
const eval = @import("eval.zig");
const emit = @import("emit.zig");
const object = @import("object.zig");
const format = @import("format.zig");

const TestError = error {
    Error
};

test "execute small test" {
    const allocator = std.testing.allocator;
    const file = try std.fs.cwd().openFile("./tests/small_tests.scm", .{});
    defer file.close();
    const content = try file.readToEndAlloc(allocator, 100000);
    defer allocator.free(content);

    const global_pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(global_pool);
    global_pool.gc_enabled = false;
    const parsed = try parser.parse_string(content, global_pool);
    var current = parsed;
    while (object.obj_type(&current) != .nil) {
        if (object.obj_type(&current) != .cons) {
            std.debug.print("illegal test file: input is not list\n", .{});
            return TestError.Error;
        }
        const expr_str = try format.write(global_pool, object.get_car(&object.get_car(&current)), allocator);
        defer allocator.free(expr_str);
        const expected_str = try format.write(global_pool, object.get_car(&object.get_cdr(&object.get_car(&current))), allocator);
        defer allocator.free(expected_str);

        const evaluator = try eval.create_evaluator(allocator);
        defer eval.destroy_evaluator(evaluator);
        var expr = try parser.parse_string(expr_str, evaluator.pool);
        var expected = try parser.parse_string(expected_str, evaluator.pool);
        try object.push_root(evaluator.pool, &expr);
        try object.push_root(evaluator.pool, &expected);

        evaluator.pool.gc_every_time = true;

        // std.debug.print("evaling {s}\n", .{expr_str});

        const emitted_idx = emit.emit_func(evaluator, expr, try object.create_nil(evaluator.pool)) catch |err| {
            std.debug.print("emit error while emiting:", .{});
            format.debug_println_obj(evaluator.pool, expr);
            return err;
        };
        const result = eval.eval_compiled_global(evaluator, emitted_idx) catch |err| {
            std.debug.print("eval error while evaling: ", .{});
            format.debug_println_obj(evaluator.pool, expr);
            return err;
        };
        if (! object.equal(&expected, &result)) {
            const formatted = try format.write(evaluator.pool, expr, allocator);
            defer allocator.free(formatted);
            const result_formatted = try format.write(evaluator.pool, result, allocator);
            defer allocator.free(result_formatted);
            const expected_formatted = try format.write(evaluator.pool, expected, allocator);
            defer allocator.free(expected_formatted);
            std.debug.print("eval error mismatch while evaling:{s}\n", .{formatted});
            std.debug.print("expected:{s}\n", .{expected_formatted});
            std.debug.print("actual:{s}\n", .{result_formatted});
            return TestError.Error;
        }
        current = object.get_cdr(&current);
    }
}