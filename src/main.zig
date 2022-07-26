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
// TYPE:6 symbol value:len additional:string(length: value)

const ObjHeader = extern struct {
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

const ParseError  = error {
    UnexpectedToken,
};

const TYPE_OF_OBJ_TYPE = u32;
const TYPE_TRUE: TYPE_OF_OBJ_TYPE = 1;
const TYPE_FALSE: TYPE_OF_OBJ_TYPE = 2;
const TYPE_NUMBER: TYPE_OF_OBJ_TYPE = 3;
const TYPE_CONS: TYPE_OF_OBJ_TYPE = 4;
const TYPE_NIL: TYPE_OF_OBJ_TYPE = 5;
const TYPE_SYMBOL: TYPE_OF_OBJ_TYPE = 6;

const ObjPool = struct {
    allocator: std.mem.Allocator,
};

pub fn obj_type (header: *ObjHeader) TYPE_OF_OBJ_TYPE {
    return @intCast(u32, header.i & 0xffff);
}

pub fn obj_as_number(header: *ObjHeader) i32 {
    return @intCast(i32, header.i >> 32);
}

pub fn obj_as_symbol(header: *ObjHeader, allocator: std.mem.Allocator) ![] const u8 {
    const len = @intCast(usize, obj_as_number(header));
    const slice: []u8 = try allocator.alloc(u8, @intCast(usize, len));
    const symbol = @ptrCast([*] const u8, &@ptrCast(*SymbolObj, header).str[0]);
    @memcpy(@ptrCast([*] u8, &slice[0]), symbol, len);
    return slice;
}

pub fn obj_car(header: *ObjHeader) *ObjHeader {
    const consCell = @ptrCast(*ObjConsCell, header);
    return consCell.car;
}
 
pub fn obj_cdr(header: *ObjHeader) *ObjHeader {
    const consCell = @ptrCast(*ObjConsCell, header);
    return consCell.cdr;
}

pub fn objpool_create(allocator: std.mem.Allocator) !*ObjPool {
    const objPool = try allocator.create(ObjPool);
    objPool.allocator = allocator;
    std.log.info("{}\n", .{objPool.allocator});
    return objPool;
}

pub fn objpool_create_symbol(pool: *ObjPool, str: [] const u8) !*ObjHeader {
    const obj = @ptrCast(*SymbolObj, @alignCast(8, try pool.allocator.alloc(u8, @sizeOf(ObjHeader) + str.len)));
    const header = &obj.header;
    try objpool_init_header(header, TYPE_SYMBOL, @intCast(i32, str.len));
    @memcpy(@ptrCast([*] u8, &obj.str[0]), @ptrCast([*] const u8, &str[0]), str.len);
    return header;
}

pub fn objpool_init_header(header:*ObjHeader, o_type: TYPE_OF_OBJ_TYPE, value: i32) !void {
    header.i = @intCast(u64, o_type) + (@intCast(u64, value) << 32);
}

pub fn objpool_create_simple(objPool: *ObjPool, o_type: TYPE_OF_OBJ_TYPE, value: i32) !*ObjHeader {
    const header = try objPool.allocator.create(ObjHeader);
    try objpool_init_header(header, o_type, value);
    return header;
}

pub fn objpool_create_true(pool: *ObjPool) !*ObjHeader {
    return objpool_create_simple(pool, TYPE_TRUE, 0);
}

pub fn objpool_create_false(pool: *ObjPool) !*ObjHeader {
    return objpool_create_simple(pool, TYPE_FALSE, 0);
}

pub fn objpool_create_number(pool: *ObjPool, n: i32) !*ObjHeader {
    return objpool_create_simple(pool, TYPE_NUMBER, n);
}

pub fn objpool_create_nil(pool: *ObjPool) !*ObjHeader {
    return objpool_create_simple(pool, TYPE_NIL, 0);
}

pub fn objpool_create_cons(pool: *ObjPool, car: *ObjHeader, cdr:*ObjHeader) !*ObjHeader {
    const cell: *ObjConsCell = try pool.allocator.create(ObjConsCell);
    try objpool_init_header(&cell.header, TYPE_CONS, 0);
    cell.car = car;
    cell.cdr = cdr;
    return @ptrCast(*ObjHeader, cell);
}

pub fn parser_skip_whitespaces(p: *Parser) !void {
    while(p.s.len > p.p and p.s[p.p] == ' ') {
        p.p+=1;
    }
}

pub fn parse_number(p: *Parser, pool: *ObjPool) !*ObjHeader {
    var val: i32 = 0;
    while(p.s.len > p.p and p.s[p.p] >= '0' and p.s[p.p] <= '9') {
        val = val * 10 + (p.s[p.p] - '0');
        p.p+=1;
    }
    return try objpool_create_number(pool, val);
}

pub fn parse_list(p: *Parser, pool: *ObjPool) anyerror!*ObjHeader {
    if (p.s[p.p] == ')') {
        p.p+=1;
        return objpool_create_nil(pool);
    } else {
        const car: *ObjHeader = try parse_datum(p, pool);
        try parser_skip_whitespaces(p);
        const cdr = try parse_list(p, pool);
        return objpool_create_cons(pool, car, cdr);
    }
}

pub fn parse_simple_symbol(p: *Parser, pool: *ObjPool, start_pos: usize) anyerror!*ObjHeader {
    while(p.s.len > p.p and ((p.s[p.p] >= 'a' and p.s[p.p] <= 'z') or (p.s[p.p] >= 'A' and p.s[p.p] <= 'Z'))) {
        p.p+=1;
    }
    return try objpool_create_symbol(pool, p.s[start_pos..p.p]);
}

pub fn parse_datum(p:*Parser, pool: *ObjPool) anyerror!*ObjHeader {
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
    } else if (p.s[p.p] == '(') {
        p.p+=1;
        try parser_skip_whitespaces(p);
        return try parse_list(p, pool);
    } else if (
        (p.s[p.p] >= 'a' and p.s[p.p] <= 'z') or
        (p.s[p.p] >= 'A' and p.s[p.p] <= 'Z') or
        (p.s[p.p] >= '!') or
        (p.s[p.p] >= '$') or
        (p.s[p.p] >= '%') or
        (p.s[p.p] >= '&') or
        (p.s[p.p] >= '*') or
        (p.s[p.p] >= '/') or
        (p.s[p.p] >= ':') or
        (p.s[p.p] >= '<') or
        (p.s[p.p] >= '=') or
        (p.s[p.p] >= '>') or
        (p.s[p.p] >= '?') or
        (p.s[p.p] >= '^') or
        (p.s[p.p] >= '_') or
        (p.s[p.p] >= '~')
    ) {
        const start_pos = p.p;
        p.p+=1;
        return try parse_simple_symbol(p, pool, start_pos);
    } else if ((p.s[p.p] == '+' or p.s[p.p] == '-') and (p.s.len > (p.p + 1) or p.s[p.p+1] == ' ')) {
        const symbol = objpool_create_symbol(pool, p.s[p.p..p.p+1]);
        p.p+=1;
        try parser_skip_whitespaces(p);
        return symbol;
    }
    return ParseError.UnexpectedToken;
}

pub fn parse(p:*Parser, pool: *ObjPool) !*ObjHeader {
    try parser_skip_whitespaces(p);
    return parse_datum(p, pool);
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
test "parse list" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    try std.testing.expectEqual(TYPE_NIL, obj_type(try parseString("()", allocator)));
    const list: *ObjHeader = try parseString("( 42 43 )", allocator);
    const car: *ObjHeader = obj_car(list);
    const cadr: *ObjHeader = obj_car(obj_cdr(list));
    try std.testing.expectEqual(TYPE_NUMBER, obj_type(car));
    try std.testing.expectEqual(@intCast(i32, 42), obj_as_number(car));
    try std.testing.expectEqual(TYPE_NUMBER, obj_type(cadr));
    try std.testing.expectEqual(@intCast(i32, 43), obj_as_number(cadr));
    try std.testing.expectEqual(TYPE_NIL, obj_type(obj_cdr(obj_cdr(list))));
}

test "parse symbol" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    const sym = try parseString("aa", allocator);
    try std.testing.expectEqual(TYPE_SYMBOL, obj_type(sym));
    const slice = try obj_as_symbol(sym, allocator);
    defer allocator.free(slice);
    const expected: [] const u8 = "aa";
    try std.testing.expectEqualStrings(expected, slice);
}


test "parse symbol +" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    const sym = try parseString("+", allocator);
    try std.testing.expectEqual(TYPE_SYMBOL, obj_type(sym));
    const slice = try obj_as_symbol(sym, allocator);
    defer allocator.free(slice);
    const expected: [] const u8 = "+";
    try std.testing.expectEqualStrings(expected, slice);
}
