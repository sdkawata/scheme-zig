const std = @import("std");
const parser = @import("parser.zig");
const eval = @import("eval.zig");
const emit = @import("emit.zig");
const object = @import("object.zig");
const format = @import("format.zig");


var output_buf: ?*std.ArrayList(u8) = null;
fn test_displayer(e:*eval.Evaluator, obj:object.Obj) anyerror!void {
    const formatted = try format.display_to_slice(e.pool, obj, std.testing.allocator);
    defer std.testing.allocator.free(formatted);
    try std.fmt.format(output_buf.?.*.writer(), "{s}", .{formatted});
}


test "execute test scm files" {
    const allocator = std.testing.allocator;
    const dir = try std.fs.cwd().openDir("./tests/runs", .{.iterate = true});
    var dir_iterator = dir.iterate();
    while (try dir_iterator.next()) |path| {
        if (!std.mem.eql(u8, ".scm", path.name[path.name.len-4..path.name.len])) {
            continue;
        }
        const name = try std.fmt.allocPrint(allocator, "tests/runs/{s}", .{path.name});
        defer allocator.free(name);
        // if (std.mem.eql(u8, name, "tests/runs/obj_create.scm")) {continue;}
        const file = try std.fs.cwd().openFile(name, .{});
        defer file.close();
        const content = try file.readToEndAlloc(allocator, 10000);
        defer allocator.free(content);

        const expected_out_name = try std.fmt.allocPrint(allocator, "tests/runs/{s}.out", .{path.name});
        defer allocator.free(expected_out_name);
        const expected_out_file = try std.fs.cwd().openFile(expected_out_name, .{});
        defer expected_out_file.close();
        const expected = try expected_out_file.readToEndAlloc(allocator, 10000);
        defer allocator.free(expected);

        var current_buf = std.ArrayList(u8).init(allocator);
        defer std.ArrayList(u8).deinit(current_buf);
        output_buf = &current_buf;

        // std.debug.print("testing {s}\n", .{path.name});
        const evaluator = try eval.create_evaluator(allocator);
        defer eval.destroy_evaluator(evaluator);
        evaluator.displayer = test_displayer;
        var p = parser.Parser{.s = content, .p = 0};
        while(true) {
            const obj = try parser.parse(&p, evaluator.pool);
            const emitted_idx = emit.emit_func(evaluator, obj, try object.create_nil(evaluator.pool)) catch |err| {
                std.debug.print("error occurred while emmiting code for {s}\n", .{name});
                return err;
            };
            _ = eval.eval_compiled_global(evaluator, emitted_idx) catch |err| {
                std.debug.print("error occurred while executing {s}\n", .{name});
                return err;
            };
            try parser.skip_whitespaces(&p);
            if (! parser.is_char_left(&p)) {
                break;
            }
        } else unreachable;

        try std.testing.expectEqualStrings(
            std.mem.trim(u8, expected, " \n\r"),
            std.mem.trim(u8, current_buf.items, " \n\r")
        );
    }
}