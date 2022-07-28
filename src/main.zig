const std = @import("std");
const parser = @import("parser.zig");
const eval = @import("eval.zig");
const object = @import("object.zig");

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
}

test {
    _ = @import("parser.zig");
    _ = @import("object.zig");
    _ = @import("small_test.zig");
}


test "execute test scm files" {
    const allocator = std.testing.allocator;
    const dir = try std.fs.cwd().openDir("./tests/runs", .{.iterate = true});
    var dir_iterator = dir.iterate();
    while (try dir_iterator.next()) |path| {
        if (!std.mem.eql(u8, ".scm", path.name[path.name.len-4..path.name.len])) {
            continue;
        }
        // std.debug.print("testing {s}\n", .{path.name});
        const name = try std.fmt.allocPrint(allocator, "tests/runs/{s}", .{path.name});
        defer allocator.free(name);
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
        const trimed = std.mem.trim(u8, expected, " \n\r");

        const evaluator = try eval.create_evaluator(allocator);
        defer eval.destroy_evaluator(evaluator, allocator);
        var p = parser.Parser{.s = content, .p = 0};
        var evaled = while(true) {
            const obj = try parser.parse(&p, evaluator.pool);
            const evaled = try eval.eval_global(evaluator, obj);
            try parser.skip_whitespaces(&p);
            if (! parser.is_char_left(&p)) {
                break evaled;
            }
        } else unreachable;

        try object.expectFormatEqual(evaluator.pool, trimed, evaled);
    }
}
