const std = @import("std");

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
}

test {
    _ = @import("parser.zig");
    _ = @import("object.zig");
}