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
// type buildin value: number which meaning is given by executor
// type symbol value: symbol id
// memory layout
// |<- obj value(32bit) -> |<- obj tag(32 bit) -> |<- additional (any byte) >|
// TYPE cons cell value:none additional:CAR pointer(8byte) + CDR pointer(8byte)
// TYPE env frame value:none additional:vars pointer(8byte, list of pair of key and value) + previous frame (8btye)
// TYPE env frame value:none additional:func id(8byte) + env (8btye)


const ObjHeader = packed struct {
    i: u64,
};

const ObjConsCell = packed struct {
    header: ObjHeader,
    car: Obj,
    cdr: Obj,
};

const ObjFrame = packed struct {
    header: ObjHeader,
    vars: Obj,
    previous: Obj,
};

const ObjFunc = packed struct {
    header: ObjHeader,
    func_id: usize,
    env: Obj,
};

const ObjValueType = enum(u16) {
    b_true,
    b_false,
    number_i32,
    nil,
    buildin,
    symbol,
    undef,
};
const ObjRefType = enum(u32) {
    cons,
    frame,
    func,
};

pub const ObjType =  enum(u32) {
    b_true,
    b_false,
    undef,
    number,
    cons,
    nil,
    symbol,
    buildin,
    frame,
    func,
};

const INITIAL_BUF_SIZE = 1000000;

pub const ObjPool = struct {
    buf: [] u8,
    current: [*] u8,
    end: [*] u8,
    symbol_table: std.ArrayList([] const u8),
    allocator: std.mem.Allocator,
    stack_frames: std.ArrayList(Obj),
    consts: std.ArrayList(Obj),
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
            .undef => .undef,
            .nil => .nil,
            .number_i32 => .number,
            .buildin => .buildin,
            .symbol => .symbol,
        };
    } else {
        return switch(obj_ref_type(obj)) {
            .cons => .cons,
            .frame => .frame,
            .func => .func,
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
pub fn get_buildin_value(obj: Obj) i32 {
    assert(is_value(obj) and obj_value_type(obj) == .buildin);
    return obj_value(obj);
}

pub fn get_symbol_id(obj: Obj) usize {
    assert(is_value(obj) and obj_value_type(obj) == .symbol);
    return @intCast(usize, obj_value(obj));
}

pub fn as_symbol(pool: *ObjPool, obj: Obj) ![] const u8 {
    return pool.symbol_table.items[get_symbol_id(obj)];
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

pub fn get_func_id(obj: Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .func);
    const header = as_obj_header(obj);
    const func = @ptrCast(*ObjFunc, header);
    return func.func_id;
}

pub fn get_func_env(obj: Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .func);
    const header = as_obj_header(obj);
    const func = @ptrCast(*ObjFunc, header);
    return func.env;
}

pub fn get_frame_vars(obj: Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .frame);
    const header = as_obj_header(obj);
    const frame = @ptrCast(*ObjFrame, header);
    return frame.vars;
}

pub fn get_frame_previous(obj: Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .frame);
    const header = as_obj_header(obj);
    const frame = @ptrCast(*ObjFrame, header);
    return frame.previous;
}

pub fn push_frame_var(pool: *ObjPool, obj: Obj, key: Obj, value: Obj) !void {
    assert(!is_value(obj) and obj_ref_type(obj) == .frame);
    const header = as_obj_header(obj);
    const frame = @ptrCast(*ObjFrame, header);
    frame.vars = try create_cons(pool, try create_cons(pool, key, value), frame.vars);
}

pub const LookUpError = error {
    NotFound,
};

pub fn lookup_frame(obj: Obj, symbol_id: usize) !Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .frame);
    var currentFrame = obj;
    while (obj_type(currentFrame) != .nil) {
        var currentVars = get_frame_vars(currentFrame);
        while (obj_type(currentVars) != .nil) {
            const varPair = get_car(currentVars);
            if (symbol_id == get_symbol_id(get_car(varPair))) {
                return get_cdr(varPair);
            }
            currentVars = get_cdr(currentVars);
        }
        currentFrame = get_frame_previous(currentFrame);
    }
    return LookUpError.NotFound;
}

pub fn create_obj_pool(allocator: std.mem.Allocator) !*ObjPool {
    const pool = try allocator.create(ObjPool);
    pool.buf = try allocator.alloc(u8, INITIAL_BUF_SIZE);
    pool.current = @ptrCast([*] u8, &pool.buf[0]);
    pool.end = pool.current + INITIAL_BUF_SIZE;
    pool.symbol_table = std.ArrayList([] const u8).init(allocator);
    pool.stack_frames = std.ArrayList(Obj).init(allocator);
    pool.consts = std.ArrayList(Obj).init(allocator);
    pool.allocator = allocator;
    return pool;
}

pub fn destroy_obj_pool(pool: *ObjPool) void {
    pool.allocator.free(pool.buf);
    for (pool.symbol_table.items) |sym| {
        pool.allocator.free(sym);
    }
    std.ArrayList([] const u8).deinit(pool.symbol_table);
    std.ArrayList(Obj).deinit(pool.stack_frames);
    std.ArrayList(Obj).deinit(pool.consts);
    pool.allocator.destroy(pool);
}

fn align_size(size: usize) usize {
    return (size + (8-1)) / 8 * 8;
}

const CreateError = error {
    NoMemoryError,
};

fn create(pool: *ObjPool, comptime T: type) !*T {
    const ptr = @ptrCast(*T, @alignCast(@alignOf(T), pool.current));
    pool.current+= align_size(@sizeOf(T));
    if (@ptrToInt(pool.current) > @ptrToInt(pool.end)) {
        return CreateError.NoMemoryError;
    }
    return ptr;
}

fn alloc(pool: *ObjPool, size: usize) ![*]u8 {
    const ptr = @ptrCast([*]u8, pool.current);
    pool.current+= align_size(size);
    return ptr;
}

pub fn create_symbol_by_id(_: *ObjPool, id: usize) !Obj {
    return create_value(ObjValueType.symbol, @intCast(i32, id));
}

pub fn create_symbol(pool: *ObjPool, str: [] const u8) !Obj {
    for (pool.symbol_table.items) |sym, i| {
        if (std.mem.eql(u8, sym, str)) {
            return create_value(ObjValueType.symbol, @intCast(i32, i));
        }
    }
    const dest = try pool.allocator.alloc(u8, str.len);
    for(str[0..str.len]) |b, i| dest[i] = b;
    const idx = pool.symbol_table.items.len;
    try std.ArrayList([] const u8).append(&pool.symbol_table, dest);
    return create_symbol_by_id(pool, idx);
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

pub fn create_undef(_: *ObjPool) !Obj {
    return create_value(ObjValueType.undef, 0);
}

pub fn create_number(_: *ObjPool, n: i32) !Obj {
    return create_value(ObjValueType.number_i32, n);
}

pub fn create_opaque(_: *ObjPool, n: i32) !Obj {
    return create_value(ObjValueType.buildin, n);
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

pub fn create_frame(pool: *ObjPool, vars: Obj, previous: Obj) !Obj {
    const frame = try create(pool, ObjFrame);
    try init_header(&frame.header, ObjRefType.frame, 0);
    frame.vars = vars;
    frame.previous = previous;
    return as_obj(@ptrCast(*ObjHeader, frame));
}

pub fn create_func(pool: *ObjPool, func_id: usize, env: Obj) !Obj {
    const func = try create(pool, ObjFunc);
    try init_header(&func.header, ObjRefType.func, 0);
    func.func_id = func_id;
    func.env = env;
    return as_obj(@ptrCast(*ObjHeader, func));
}

pub fn equal(obj1: Obj, obj2: Obj) bool {
    const type1 = obj_type(obj1);
    const type2 = obj_type(obj2);
    if (type1 == .b_true and type2 == .b_true) {
        return true;
    } else if  (type1 == .b_false and type2 == .b_false) {
        return true;
    } else if (type1 == .symbol and type2 == .symbol) {
        return get_symbol_id(obj1) == get_symbol_id(obj2);
    } else if (type1 == .cons and type2 == .cons) {
        return equal(get_car(obj1), get_car(obj2)) and equal(get_cdr(obj1), get_cdr(obj2));
    } else if (type1 == .nil and type2 == .nil) {
        return true;
    } else if (type1 == .number and type2 == .number) {
        return as_number(obj1) == as_number(obj2);
    }
    return false;
}

const Writer = std.ArrayList(u8);

fn format_symbol(pool: *ObjPool, obj: Obj, _: std.mem.Allocator, writer: *Writer) !void {
    const symbol = try as_symbol(pool, obj);
    try std.fmt.format(writer.writer(), "{s}", .{symbol});
}

fn format_cons(pool: *ObjPool, obj: Obj, allocator: std.mem.Allocator, writer: *Writer) anyerror!void {
    var current = obj;
    var first = true;
    while(obj_type(current) == .cons) {
        if (! first) {
            try std.fmt.format(writer.writer(), " ", .{});
        } else {
            try std.fmt.format(writer.writer(), "(", .{});
        }
        first = false;
        try format_rec(pool, get_car(current), allocator, writer);
        current = get_cdr(current);
    }
    if (obj_type(current) == .nil) {
        try std.fmt.format(writer.writer(), ")", .{});
    } else {
        try std.fmt.format(writer.writer(), " . ", .{});
        try format_rec(pool, current, allocator, writer);
        try std.fmt.format(writer.writer(), ")", .{});
    }
}


fn format_rec(pool: *ObjPool, obj: Obj, allocator: std.mem.Allocator, writer: *Writer) anyerror!void {
    try switch (obj_type(obj)) {
        .b_true => std.fmt.format(writer.writer(), "#t", .{}),
        .b_false => std.fmt.format(writer.writer(), "#f", .{}),
        .undef => std.fmt.format(writer.writer(), "#<undef>", .{}),
        .nil => std.fmt.format(writer.writer(), "()", .{}),
        .symbol => format_symbol(pool, obj, allocator, writer),
        .number => std.fmt.format(writer.writer(), "{}", .{as_number(obj)}),
        .buildin => std.fmt.format(writer.writer(), "#<buildin: {}>", .{get_buildin_value(obj)}),
        .frame => std.fmt.format(writer.writer(), "#<frame>", .{}),
        .func => std.fmt.format(writer.writer(), "#<closure>", .{}),
        .cons  => format_cons(pool, obj, allocator, writer),
    };
}

pub fn format(pool: *ObjPool, obj: Obj, allocator: std.mem.Allocator) ![] const u8 {
    var writer = Writer.init(allocator);
    defer Writer.deinit(writer);
    try format_rec(pool, obj, allocator, &writer);
    return Writer.toOwnedSlice(&writer);
}

pub fn debug_println_obj(pool: *ObjPool, obj: Obj) void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const formatted = format(pool, obj, allocator) catch {
        std.debug.print("!!format error!!\n", .{});
        return;
    };
    // defer allocator.free(formatted);
    std.debug.print("{s}\n", .{formatted});
}

pub fn expectFormatEqual(pool: *ObjPool, expected: [] const u8, o: Obj) !void {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const actual = try format(pool, o, allocator);
    defer allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

test "format" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try create_obj_pool(allocator);
    defer destroy_obj_pool(pool);
    try expectFormatEqual(pool, "#t", try create_true(pool));
    try expectFormatEqual(pool, "#f", try create_false(pool));
    try expectFormatEqual(pool, "42", try create_number(pool, 42));
    try expectFormatEqual(pool, "symbol", try create_symbol(pool, "symbol"));
    try expectFormatEqual(pool, "()", try create_nil(pool));
    try expectFormatEqual(pool, "(42 43)", try create_cons(pool,  try create_number(pool, 42), try create_cons(pool, try create_number(pool, 43), try create_nil(pool))));
    try expectFormatEqual(pool, "(+ 43)", try create_cons(pool,  try create_symbol(pool, "+"), try create_cons(pool, try create_number(pool, 43), try create_nil(pool))));
    try expectFormatEqual(pool, "(42 . 43)", try create_cons(pool,  try create_number(pool, 42), try create_number(pool, 43)));
}
