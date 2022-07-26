const std = @import("std");
const object = @import("object.zig");

const Writer = std.ArrayList(u8);

fn write_symbol(writer: anytype, pool: *object.ObjPool, obj: object.Obj) !void {
    const symbol = try object.as_symbol(pool, &obj);
    try std.fmt.format(writer, "{s}", .{symbol});
}

fn write_cons(writer: anytype, pool: *object.ObjPool, obj: object.Obj, is_write: bool) anyerror!void {
    var current = obj;
    var first = true;
    while(object.obj_type(&current) == .cons) {
        if (! first) {
            try std.fmt.format(writer, " ", .{});
        } else {
            try std.fmt.format(writer, "(", .{});
        }
        first = false;
        try write_rec(writer, pool, object.get_car(&current), is_write);
        current = object.get_cdr(&current);
    }
    if (object.obj_type(&current) == .nil) {
        try std.fmt.format(writer, ")", .{});
    } else {
        try std.fmt.format(writer, " . ", .{});
        try write_rec(writer, pool, current, is_write);
        try std.fmt.format(writer, ")", .{});
    }
}

fn write_rec(writer: anytype, pool: *object.ObjPool, obj: object.Obj, is_write: bool) anyerror!void {
    try switch (object.obj_type(&obj)) {
        .b_true => std.fmt.format(writer, "#t", .{}),
        .b_false => std.fmt.format(writer, "#f", .{}),
        .undef => std.fmt.format(writer, "#<undef>", .{}),
        .char => {
            const a = [_]u8{object.get_char_value(&obj)}; // workaround for segfault
            if (is_write) {
                // write
                try std.fmt.format(writer, "#\\{s}", .{a});
            } else {
                // display
                try std.fmt.format(writer, "{s}", .{a});
            }
        },
        .nil => std.fmt.format(writer, "()", .{}),
        .symbol => write_symbol(writer, pool, obj),
        .number => std.fmt.format(writer, "{}", .{object.as_number(&obj)}),
        .float => std.fmt.format(writer, "{e}", .{object.as_float(&obj)}),
        .buildin => std.fmt.format(writer, "#<buildin: {}>", .{object.get_buildin_value(&obj)}),
        .frame => std.fmt.format(writer, "#<frame>", .{}),
        .func => std.fmt.format(writer, "#<closure>", .{}),
        .cons  => write_cons(writer, pool, obj, is_write),
    };
}

pub fn write(writer: anytype, pool: *object.ObjPool, obj: object.Obj) !void {
    try write_rec(writer, pool, obj, true);
}

pub fn write_to_slice(pool: *object.ObjPool, obj: object.Obj, allocator: std.mem.Allocator) ![] u8 {
    var writer = Writer.init(allocator);
    defer Writer.deinit(writer);
    try write(writer.writer(), pool, obj);
    return Writer.toOwnedSlice(&writer);
}

pub fn display(writer: anytype, pool: *object.ObjPool, obj: object.Obj) !void {
    try write_rec(writer, pool, obj, false);
}

pub fn display_to_slice(pool: *object.ObjPool, obj: object.Obj, allocator: std.mem.Allocator) ![] u8 {
    var writer = Writer.init(allocator);
    defer Writer.deinit(writer);
    try display(writer.writer(), pool, obj);
    return Writer.toOwnedSlice(&writer);
}

pub fn debug_println_obj(pool: *object.ObjPool, obj: object.Obj) void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const formatted = write_to_slice(pool, obj, allocator) catch {
        std.debug.print("!!format error!!\n", .{});
        return;
    };
    defer allocator.free(formatted);
    std.debug.print("{s}\n", .{formatted});
}

pub fn expectWriteEqual(pool: *object.ObjPool, expected: [] const u8, o: object.Obj) !void {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const actual = try write_to_slice(pool, o, allocator);
    defer allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

fn expectDisplayEqual(pool: *object.ObjPool, expected: [] const u8, o: object.Obj) !void {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const actual = try display_to_slice(pool, o, allocator);
    defer allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

test "write" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    pool.gc_enabled = false;
    try expectWriteEqual(pool, "#t", try object.create_true(pool));
    try expectWriteEqual(pool, "#f", try object.create_false(pool));
    try expectWriteEqual(pool, "42", try object.create_number(pool, 42));
    try expectWriteEqual(pool, "symbol", try object.create_symbol(pool, "symbol"));
    try expectWriteEqual(pool, "#\\a", try object.create_char(pool, 'a'));
    try expectWriteEqual(pool, "()", try object.create_nil(pool));
    try expectWriteEqual(pool, "(42 43)", try object.create_cons(pool,  &try object.create_number(pool, 42), &try object.create_cons(pool, &try object.create_number(pool, 43), &try object.create_nil(pool))));
    try expectWriteEqual(pool, "(+ 43)", try object.create_cons(pool,  &try object.create_symbol(pool, "+"), &try object.create_cons(pool, &try object.create_number(pool, 43), &try object.create_nil(pool))));
    try expectWriteEqual(pool, "(42 . 43)", try object.create_cons(pool,  &try object.create_number(pool, 42), &try object.create_number(pool, 43)));
}


test "display" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    pool.gc_enabled = false;
    try expectDisplayEqual(pool, "a", try object.create_char(pool, 'a'));
    try expectDisplayEqual(pool, "(a)", try object.create_cons(pool,  &try object.create_char(pool, 'a'), &try object.create_nil(pool)));
}

