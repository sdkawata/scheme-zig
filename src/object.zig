const std = @import("std");
const assert = std.debug.assert;

pub const Obj = u64;

// value is pointer or value
// value layout
// [<- value(32bit)->][<- type(16bit) ->][<- reserved ->][<- value bit(1 for value)(1bit) ->]
// note: pointer is always aligned by 8
// type ture value:none
// type false value:none
// type nil value: none
// type number(32bit) value: number
// type opaque value: number which meaning is given by executor
// memory layout
// |<- obj value(32bit) -> |<- obj tag(32 bit) -> |<- additional (any byte) >|
// TYPE:4 cons cell value:none additional:CAR pointer(8byte) + CDR pointer(8byte)
// TYPE symbol value:len additional:string(length: value)

const ObjHeader = extern struct {
    i: u64,
};

const ObjConsCell = extern struct {
    header: ObjHeader,
    car: Obj,
    cdr: Obj,
};

const SymbolObj = extern struct {
    header: ObjHeader,
    str: [1]u8, // actually have variable length
};

const ObjValueType = enum(u16) {
    b_true,
    b_false,
    number_i32,
    nil,
    b_opaque
};
const ObjRefType = enum(u32) {
    cons,
    symbol,
};

pub const ObjType =  enum(u32) {
    b_true,
    b_false,
    number,
    cons,
    nil,
    symbol,
    b_opaque,
};

const INITIAL_BUF_SIZE = 1000;

pub const ObjPool = struct {
    buf: [] u8,
    current: [*] u8,
    end: [*] u8,
};

fn is_value (obj:Obj) bool {
    return obj & 0x1 == 1;
}
fn as_obj_header(obj:Obj) *ObjHeader {
    return @intToPtr(*ObjHeader, @intCast(usize, obj));
}
fn as_obj(header: *ObjHeader) Obj {
    return @intCast(u64, @ptrToInt(header));
}

fn obj_ref_type(obj:Obj) ObjRefType {
    assert(!is_value(obj));
    return @intToEnum(ObjRefType, @intCast(u32, as_obj_header(obj).i & 0xffff));
}

fn obj_value_type(obj:Obj) ObjValueType {
    assert(is_value(obj));
    return @intToEnum(ObjValueType, @intCast(u16, obj >> 16 & 0xff));
}

pub fn obj_type (obj:Obj) ObjType {
    if (is_value(obj)) {
        return switch(obj_value_type(obj)) {
            .b_true => .b_true,
            .b_false => .b_false,
            .nil => .nil,
            .number_i32 => .number,
            .b_opaque => .b_opaque,
        };
    } else {
        return switch(obj_ref_type(obj)) {
            .cons => .cons,
            .symbol => .symbol,
        };
    }
}

fn obj_value(obj: Obj) i32 {
    assert(is_value(obj));
    return @intCast(i32, obj >> 32);
}

fn obj_ref_value(header: *ObjHeader) i32 {
    return @intCast(i32, header.i >> 32);
} 

pub fn as_number(obj: Obj) i32 {
    assert(is_value(obj) and obj_value_type(obj) == .number_i32);
    return obj_value(obj);
}
pub fn get_opaque_value(obj: Obj) i32 {
    assert(is_value(obj) and obj_value_type(obj) == .b_opaque);
    return obj_value(obj);
}

pub fn as_symbol(obj: Obj, allocator: std.mem.Allocator) ![] const u8 {
    const header = as_obj_header(obj);
    const len = @intCast(usize, obj_ref_value(header));
    const slice: []u8 = try allocator.alloc(u8, @intCast(usize, len));
    const symbol = @ptrCast([*] const u8, &@ptrCast(*SymbolObj, header).str[0]);
    @memcpy(@ptrCast([*] u8, &slice[0]), symbol, len);
    return slice;
}

pub fn as_symbol_noalloc(obj: Obj) ![] const u8 {
    assert(!is_value(obj) and obj_ref_type(obj) == .symbol);
    const header = as_obj_header(obj);
    const len = @intCast(usize, obj_ref_value(header));
    return @ptrCast([*] const u8, &@ptrCast(*SymbolObj, header).str[0])[0..len];
}

pub fn get_car(obj: Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .cons);
    const header = as_obj_header(obj);
    const consCell = @ptrCast(*ObjConsCell, header);
    return consCell.car;
}
 
pub fn get_cdr(obj: Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .cons);
    const header = as_obj_header(obj);
    const consCell = @ptrCast(*ObjConsCell, header);
    return consCell.cdr;
}

pub fn create_obj_pool(allocator: std.mem.Allocator) !*ObjPool {
    const pool = try allocator.create(ObjPool);
    pool.buf = try allocator.alloc(u8, INITIAL_BUF_SIZE);
    pool.current = @ptrCast([*] u8, &pool.buf[0]);
    pool.end = pool.current + INITIAL_BUF_SIZE;
    return pool;
}

pub fn destroy_obj_pool(pool: *ObjPool, allocator: std.mem.Allocator) void {
    allocator.free(pool.buf);
    allocator.destroy(pool);
}

fn align_size(size: usize) usize {
    return (size + (8-1)) / 8 * 8;
}

fn create(pool: *ObjPool, comptime T: type) !*T {
    const ptr = @ptrCast(*T, @alignCast(@alignOf(T), pool.current));
    pool.current+= align_size(@sizeOf(T));
    return ptr;
}

fn alloc(pool: *ObjPool, size: usize) ![*]u8 {
    const ptr = @ptrCast([*]u8, pool.current);
    pool.current+= align_size(size);
    return ptr;
}

pub fn create_symbol(pool: *ObjPool, str: [] const u8) !Obj {
    const obj = @ptrCast(*SymbolObj, @alignCast(8, try alloc(pool, @sizeOf(ObjHeader) + str.len)));
    const header = &obj.header;
    try init_header(header, ObjRefType.symbol, @intCast(i32, str.len));
    @memcpy(@ptrCast([*] u8, &obj.str[0]), @ptrCast([*] const u8, &str[0]), str.len);
    return as_obj(header);
}

fn init_header(header:*ObjHeader, o_type: ObjRefType, value: i32) !void {
    header.i = @intCast(u64, @enumToInt(o_type)) + (@intCast(u64, value) << 32);
}

pub fn create_value(o_type: ObjValueType, value: i32) Obj {
    return (@intCast(u64, value) << 32) + (@intCast(u64, @enumToInt(o_type)) << 16) + 1;
}

pub fn create_true(_: *ObjPool) !Obj {
    return create_value(ObjValueType.b_true, 0);
}

pub fn create_false(_: *ObjPool) !Obj {
    return create_value(ObjValueType.b_false, 0);
}

pub fn create_number(_: *ObjPool, n: i32) !Obj {
    return create_value(ObjValueType.number_i32, n);
}

pub fn create_opaque(_: *ObjPool, n: i32) !Obj {
    return create_value(ObjValueType.b_opaque, n);
}

pub fn create_nil(_: *ObjPool) !Obj {
    return create_value(ObjValueType.nil, 0);
}

pub fn create_cons(pool: *ObjPool, car: Obj, cdr:Obj) !Obj {
    const cell: *ObjConsCell = try create(pool, ObjConsCell);
    try init_header(&cell.header, ObjRefType.cons, 0);
    cell.car = car;
    cell.cdr = cdr;
    return as_obj(@ptrCast(*ObjHeader, cell));
}


const Writer = std.ArrayList(u8);

fn format_symbol(obj: Obj, allocator: std.mem.Allocator, writer: *Writer) !void {
    const symbol = try as_symbol(obj, allocator);
    defer allocator.free(symbol);
    try std.fmt.format(writer.writer(), "{s}", .{symbol});
}

fn format_cons(obj: Obj, allocator: std.mem.Allocator, writer: *Writer) anyerror!void {
    var current = obj;
    var first = true;
    while(obj_type(current) == .cons) {
        if (! first) {
            try std.fmt.format(writer.writer(), " ", .{});
        } else {
            try std.fmt.format(writer.writer(), "(", .{});
        }
        first = false;
        try format_rec(get_car(current), allocator, writer);
        current = get_cdr(current);
    }
    if (obj_type(current) == .nil) {
        try std.fmt.format(writer.writer(), ")", .{});
    } else {
        try std.fmt.format(writer.writer(), " . ", .{});
        try format_rec(current, allocator, writer);
        try std.fmt.format(writer.writer(), ")", .{});
    }
}


fn format_rec(obj: Obj, allocator: std.mem.Allocator, writer: *Writer) anyerror!void {
    try switch (obj_type(obj)) {
        .b_true => std.fmt.format(writer.writer(), "#t", .{}),
        .b_false => std.fmt.format(writer.writer(), "#f", .{}),
        .nil => std.fmt.format(writer.writer(), "()", .{}),
        .symbol => format_symbol(obj, allocator, writer),
        .number => std.fmt.format(writer.writer(), "{}", .{as_number(obj)}),
        .b_opaque => std.fmt.format(writer.writer(), "#<opaque: {}>", .{get_opaque_value(obj)}),
        .cons  => format_cons(obj, allocator, writer),
    };
}

pub fn format(obj: Obj, allocator: std.mem.Allocator) ![] const u8 {
    var writer = Writer.init(allocator);
    defer Writer.deinit(writer);
    try format_rec(obj, allocator, &writer);
    return Writer.toOwnedSlice(&writer);
}


pub fn expectFormatEqual(expected: [] const u8, o: Obj) !void {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const actual = try format(o, allocator);
    defer allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

test "format" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try create_obj_pool(allocator);
    defer destroy_obj_pool(pool, allocator);
    try expectFormatEqual("#t", try create_true(pool));
    try expectFormatEqual("#f", try create_false(pool));
    try expectFormatEqual("42", try create_number(pool, 42));
    try expectFormatEqual("symbol", try create_symbol(pool, "symbol"));
    try expectFormatEqual("()", try create_nil(pool));
    try expectFormatEqual("(42 43)", try create_cons(pool,  try create_number(pool, 42), try create_cons(pool, try create_number(pool, 43), try create_nil(pool))));
    try expectFormatEqual("(+ 43)", try create_cons(pool,  try create_symbol(pool, "+"), try create_cons(pool, try create_number(pool, 43), try create_nil(pool))));
    try expectFormatEqual("(42 . 43)", try create_cons(pool,  try create_number(pool, 42), try create_number(pool, 43)));
}