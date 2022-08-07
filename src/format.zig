const std = @import("std");
const object = @import("object.zig");

const Writer = std.ArrayList(u8);

fn format_symbol(pool: *object.ObjPool, obj: object.Obj, _: std.mem.Allocator, writer: *Writer) !void {
    const symbol = try object.as_symbol(pool, &obj);
    try std.fmt.format(writer.writer(), "{s}", .{symbol});
}

fn format_cons(pool: *object.ObjPool, obj: object.Obj, allocator: std.mem.Allocator, writer: *Writer) anyerror!void {
    var current = obj;
    var first = true;
    while(object.obj_type(&current) == .cons) {
        if (! first) {
            try std.fmt.format(writer.writer(), " ", .{});
        } else {
            try std.fmt.format(writer.writer(), "(", .{});
        }
        first = false;
        try format_rec(pool, object.get_car(&current), allocator, writer);
        current = object.get_cdr(&current);
    }
    if (object.obj_type(&current) == .nil) {
        try std.fmt.format(writer.writer(), ")", .{});
    } else {
        try std.fmt.format(writer.writer(), " . ", .{});
        try format_rec(pool, current, allocator, writer);
        try std.fmt.format(writer.writer(), ")", .{});
    }
}


fn format_rec(pool: *object.ObjPool, obj: object.Obj, allocator: std.mem.Allocator, writer: *Writer) anyerror!void {
    try switch (object.obj_type(&obj)) {
        .b_true => std.fmt.format(writer.writer(), "#t", .{}),
        .b_false => std.fmt.format(writer.writer(), "#f", .{}),
        .undef => std.fmt.format(writer.writer(), "#<undef>", .{}),
        .nil => std.fmt.format(writer.writer(), "()", .{}),
        .symbol => format_symbol(pool, obj, allocator, writer),
        .number => std.fmt.format(writer.writer(), "{}", .{object.as_number(&obj)}),
        .buildin => std.fmt.format(writer.writer(), "#<buildin: {}>", .{object.get_buildin_value(&obj)}),
        .frame => std.fmt.format(writer.writer(), "#<frame>", .{}),
        .func => std.fmt.format(writer.writer(), "#<closure>", .{}),
        .cons  => format_cons(pool, obj, allocator, writer),
    };
}

pub fn format(pool: *object.ObjPool, obj: object.Obj, allocator: std.mem.Allocator) ![] const u8 {
    var writer = Writer.init(allocator);
    defer Writer.deinit(writer);
    try format_rec(pool, obj, allocator, &writer);
    return Writer.toOwnedSlice(&writer);
}

pub fn debug_println_obj(pool: *object.ObjPool, obj: object.Obj) void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const formatted = format(pool, obj, allocator) catch {
        std.debug.print("!!format error!!\n", .{});
        return;
    };
    defer allocator.free(formatted);
    std.debug.print("{s}\n", .{formatted});
}

pub fn expectFormatEqual(pool: *object.ObjPool, expected: [] const u8, o: object.Obj) !void {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const actual = try format(pool, o, allocator);
    defer allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

test "format" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    pool.gc_enabled = false;
    try expectFormatEqual(pool, "#t", try object.create_true(pool));
    try expectFormatEqual(pool, "#f", try object.create_false(pool));
    try expectFormatEqual(pool, "42", try object.create_number(pool, 42));
    try expectFormatEqual(pool, "symbol", try object.create_symbol(pool, "symbol"));
    try expectFormatEqual(pool, "()", try object.create_nil(pool));
    try expectFormatEqual(pool, "(42 43)", try object.create_cons(pool,  &try object.create_number(pool, 42), &try object.create_cons(pool, &try object.create_number(pool, 43), &try object.create_nil(pool))));
    try expectFormatEqual(pool, "(+ 43)", try object.create_cons(pool,  &try object.create_symbol(pool, "+"), &try object.create_cons(pool, &try object.create_number(pool, 43), &try object.create_nil(pool))));
    try expectFormatEqual(pool, "(42 . 43)", try object.create_cons(pool,  &try object.create_number(pool, 42), &try object.create_number(pool, 43)));
}
