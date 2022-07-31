const std = @import("std");
const parser = @import("parser.zig");
const eval = @import("eval.zig");
const emit = @import("emit.zig");
const object = @import("object.zig");
const format = @import("format.zig");

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
}

test {
    _ = @import("parser.zig");
    _ = @import("object.zig");
    _ = @import("small_test.zig");
    _ = @import("runs_test.zig");
}


