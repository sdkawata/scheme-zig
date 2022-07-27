const std = @import("std");

// object
// |<- obj value(32bit) -> |<- obj tag(32 bit) -> |<- additional (any byte) >|
// TYPE:1 true value:none additional:none
// TYPE:2 false value:none additional:none
// TYPE:3 number(32bit) value:number additional:none
// TYPE:4 cons cell value:none additional:CAR pointer(8byte) + CDR pointer(8byte)
// TYPE:5 nil value:none additional:none
// TYPE:6 symbol value:len additional:string(length: value)

pub const ObjHeader = extern struct {
    i: u64,
};

const ObjConsCell = extern struct {
    header: ObjHeader,
    car: *ObjHeader,
    cdr: *ObjHeader,
};

const SymbolObj = extern struct {
    header: ObjHeader,
    str: [1]u8, // actually have variable length
};


pub const ObjType =  enum(u32) {
    b_true,
    b_false,
    number,
    cons,
    nil,
    symbol,
};

const INITIAL_BUF_SIZE = 1000;

pub const ObjPool = struct {
    buf: [] u8,
    current: [*] u8,
    end: [*] u8,
};

pub fn obj_type (header: *ObjHeader) ObjType {
    return @intToEnum(ObjType, @intCast(u32, header.i & 0xffff));
}

pub fn as_number(header: *ObjHeader) i32 {
    return @intCast(i32, header.i >> 32);
}

pub fn as_symbol(header: *ObjHeader, allocator: std.mem.Allocator) ![] const u8 {
    const len = @intCast(usize, as_number(header));
    const slice: []u8 = try allocator.alloc(u8, @intCast(usize, len));
    const symbol = @ptrCast([*] const u8, &@ptrCast(*SymbolObj, header).str[0]);
    @memcpy(@ptrCast([*] u8, &slice[0]), symbol, len);
    return slice;
}

pub fn as_symbol_noalloc(header: *ObjHeader) ![] const u8 {
    const len = @intCast(usize, as_number(header));
    return @ptrCast([*] const u8, &@ptrCast(*SymbolObj, header).str[0])[0..len];
}

pub fn get_car(header: *ObjHeader) *ObjHeader {
    const consCell = @ptrCast(*ObjConsCell, header);
    return consCell.car;
}
 
pub fn get_cdr(header: *ObjHeader) *ObjHeader {
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
    return (size + (8-1)) %8 * 8;
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

pub fn create_symbol(pool: *ObjPool, str: [] const u8) !*ObjHeader {
    const obj = @ptrCast(*SymbolObj, @alignCast(8, try alloc(pool, @sizeOf(ObjHeader) + str.len)));
    const header = &obj.header;
    try init_header(header, ObjType.symbol, @intCast(i32, str.len));
    @memcpy(@ptrCast([*] u8, &obj.str[0]), @ptrCast([*] const u8, &str[0]), str.len);
    return header;
}

fn init_header(header:*ObjHeader, o_type: ObjType, value: i32) !void {
    header.i = @intCast(u64, @enumToInt(o_type)) + (@intCast(u64, value) << 32);
}

pub fn create_simple(pool: *ObjPool, o_type: ObjType, value: i32) !*ObjHeader {
    const header = try create(pool, ObjHeader);
    try init_header(header, o_type, value);
    return header;
}

pub fn create_true(pool: *ObjPool) !*ObjHeader {
    return create_simple(pool, ObjType.b_true, 0);
}

pub fn create_false(pool: *ObjPool) !*ObjHeader {
    return create_simple(pool, ObjType.b_false, 0);
}

pub fn create_number(pool: *ObjPool, n: i32) !*ObjHeader {
    return create_simple(pool, ObjType.number, n);
}

pub fn create_nil(pool: *ObjPool) !*ObjHeader {
    return create_simple(pool, ObjType.nil, 0);
}

pub fn create_cons(pool: *ObjPool, car: *ObjHeader, cdr:*ObjHeader) !*ObjHeader {
    const cell: *ObjConsCell = try create(pool, ObjConsCell);
    try init_header(&cell.header, ObjType.cons, 0);
    cell.car = car;
    cell.cdr = cdr;
    return @ptrCast(*ObjHeader, cell);
}