const std = @import("std");

const Parser = struct {
    s: [] const u8,
    p: usize,
};

// object
// |<- obj value(32bit) -> |<- obj tag(32 bit) -> |<- additional (any byte) >|
// TYPE:1 true value:none additional:none
// TYPE:2 false value:none additional:none
// TYPE:3 number(32bit) value:number additional:none
// TYPE:4 cons cell value:none additional:CAR pointer(8byte) + CDR pointer(8byte)
// TYPE:5 nil value:none additional:none

const ObjHeader = extern struct {
    i: u64,
};

const ParseError  = error {
    UnexpectedToken,
};

const TYPE_OF_OBJ_TYPE = u32;
const TYPE_TRUE: TYPE_OF_OBJ_TYPE = 1;
const TYPE_FALSE: TYPE_OF_OBJ_TYPE = 2;
const TYPE_NUMBER: TYPE_OF_OBJ_TYPE = 3;
const TYPE_CONS: TYPE_OF_OBJ_TYPE = 4;
const TYPE_NIL: TYPE_OF_OBJ_TYPE = 5;

const ObjPool = struct {
    allocator: std.mem.Allocator,
};

pub fn obj_type (header: *ObjHeader) TYPE_OF_OBJ_TYPE {
    return @intCast(u32, header.i & 0xffff);
}

pub fn obj_as_number(header: *ObjHeader) i32 {
    return @intCast(i32, header.i >> 32);
}
 
pub fn objpool_create(allocator: std.mem.Allocator) !*ObjPool {
    const objPool = try allocator.create(ObjPool);
    objPool.allocator = allocator;
    std.log.info("{}\n", .{objPool.allocator});
    return objPool;
}

pub fn objpool_create_simple(objPool: *ObjPool, o_type: TYPE_OF_OBJ_TYPE, value: i32) !*ObjHeader {
    const objHeader = try objPool.allocator.create(ObjHeader);
    objHeader.i = @intCast(u64, o_type) + (@intCast(u64, value) << 32);
    return objHeader;
}

pub fn objpool_create_true(objPool: *ObjPool) !*ObjHeader {
    return objpool_create_simple(objPool, TYPE_TRUE, 0);
}

pub fn objpool_create_false(objPool: *ObjPool) !*ObjHeader {
    return objpool_create_simple(objPool, TYPE_FALSE, 0);
}

pub fn objpool_create_number(objPool: *ObjPool, n: i32) !*ObjHeader {
    return objpool_create_simple(objPool, TYPE_NUMBER, n);
}

pub fn parse_number(p: *Parser, pool: *ObjPool) !*ObjHeader {
    var val: i32 = 0;
    while(p.s.len > p.p and p.s[p.p] >= '0' and p.s[p.p] <= '9') {
        val = val * 10 + (p.s[p.p] - '0');
        p.p+=1;
    }
    return try objpool_create_number(pool, val);
}

pub fn parse(p:*Parser, pool: *ObjPool) !*ObjHeader {
    if (p.s[p.p] == '#') {
        if (p.s[p.p + 1] == 't') {
            p.p+=2;
            return try objpool_create_true(pool);
        } else if (p.s[p.p + 1] == 'f') {
            p.p+=2;
            return try objpool_create_false(pool);
        }
        return ParseError.UnexpectedToken;
    } else if (p.s[p.p] >= '0' and p.s[p.p] <= '9') {
        return try parse_number(p, pool);
    }
    return ParseError.UnexpectedToken;
}

pub fn parseString(s: [] const u8, allocator: std.mem.Allocator) !*ObjHeader {
    const objPool = try objpool_create(allocator);
    return try parse(
        &Parser{.s=s, .p=0,},
        objPool
    );
}

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
}

test "parse true & false" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    try std.testing.expectEqual(TYPE_TRUE, obj_type(try parseString("#t", allocator)));
    try std.testing.expectEqual(TYPE_FALSE, obj_type(try parseString("#f", allocator)));
}

test "parse number" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    const number: *ObjHeader = try parseString("42", allocator);
    try std.testing.expectEqual(TYPE_NUMBER, obj_type(number));
    try std.testing.expectEqual(@intCast(i32, 42), obj_as_number(number));
}

