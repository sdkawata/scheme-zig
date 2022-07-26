const std = @import("std");

const Parser = struct {
    s: [] const u8,
    p: usize,
};

const ObjHeader = extern struct {
    i: u64,
};

const ParseError  = error {
    UnexpectedToken,
};

const TYPE_TRUE: u64 = 1;
const TYPE_FALSE: u64 = 2;

const ObjPool = struct {
    allocator: std.mem.Allocator,
};

pub fn obj_type (header: *ObjHeader) u64 {
    return header.i;
}

pub fn objpool_create(allocator: std.mem.Allocator) !*ObjPool {
    const objPool = try allocator.create(ObjPool);
    objPool.allocator = allocator;
    std.log.info("{}\n", .{objPool.allocator});
    return objPool;
}

pub fn objpool_create_true(objPool: *ObjPool) !*ObjHeader {
    const objHeader = try objPool.allocator.create(ObjHeader);
    objHeader.i = TYPE_TRUE;
    return objHeader;
}

pub fn objpool_create_false(objPool: *ObjPool) !*ObjHeader {
    const objHeader = try objPool.allocator.create(ObjHeader);
    objHeader.i = TYPE_FALSE;
    return objHeader;
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

test "allocator" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    const h: *ObjHeader = try allocator.create(ObjHeader);
    h.i = 0;
    try std.testing.expectEqual(h.i, 0);
}

test "parse true" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    try std.testing.expectEqual(TYPE_TRUE, obj_type(try parseString("#t", allocator)));
    try std.testing.expectEqual(TYPE_FALSE, obj_type(try parseString("#f", allocator)));
}
