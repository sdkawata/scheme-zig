const std = @import("std");
const object = @import("object.zig");

const ParseError  = error {
    UnexpectedToken,
};

const Parser = struct {
    s: [] const u8,
    p: usize,
};

fn parser_skip_whitespaces(p: *Parser) !void {
    while(p.s.len > p.p and p.s[p.p] == ' ') {
        p.p+=1;
    }
}

fn parse_number(p: *Parser, pool: *object.ObjPool) !*object.ObjHeader {
    var val: i32 = 0;
    while(p.s.len > p.p and p.s[p.p] >= '0' and p.s[p.p] <= '9') {
        val = val * 10 + (p.s[p.p] - '0');
        p.p+=1;
    }
    return try object.create_number(pool, val);
}

fn parse_list(p: *Parser, pool: *object.ObjPool) anyerror!*object.ObjHeader {
    if (p.s[p.p] == ')') {
        p.p+=1;
        return object.create_nil(pool);
    } else {
        const car: *object.ObjHeader = try parse_datum(p, pool);
        try parser_skip_whitespaces(p);
        const cdr = try parse_list(p, pool);
        return object.create_cons(pool, car, cdr);
    }
}

fn parse_simple_symbol(p: *Parser, pool: *object.ObjPool, start_pos: usize) anyerror!*object.ObjHeader {
    while(p.s.len > p.p and ((p.s[p.p] >= 'a' and p.s[p.p] <= 'z') or (p.s[p.p] >= 'A' and p.s[p.p] <= 'Z'))) {
        p.p+=1;
    }
    return try object.create_symbol(pool, p.s[start_pos..p.p]);
}

fn parse_datum(p:*Parser, pool: *object.ObjPool) anyerror!*object.ObjHeader {
    if (p.s[p.p] == '#') {
        if (p.s[p.p + 1] == 't') {
            p.p+=2;
            return try object.create_true(pool);
        } else if (p.s[p.p + 1] == 'f') {
            p.p+=2;
            return try object.create_false(pool);
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
        const symbol = object.create_symbol(pool, p.s[p.p..p.p+1]);
        p.p+=1;
        try parser_skip_whitespaces(p);
        return symbol;
    }
    return ParseError.UnexpectedToken;
}

pub fn parse(p:*Parser, pool: *object.ObjPool) !*object.ObjHeader {
    try parser_skip_whitespaces(p);
    return parse_datum(p, pool);
}

pub fn parseString(s: [] const u8, allocator: std.mem.Allocator) !*object.ObjHeader {
    const objPool = try object.create_obj_pool(allocator);
    return try parse(
        &Parser{.s=s, .p=0,},
        objPool
    );
}

test "parse true & false" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    try std.testing.expectEqual(object.TYPE_TRUE, object.obj_type(try parseString("#t", allocator)));
    try std.testing.expectEqual(object.TYPE_FALSE, object.obj_type(try parseString("#f", allocator)));
}

test "parse number" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    const number: *object.ObjHeader = try parseString("42", allocator);
    try std.testing.expectEqual(object.TYPE_NUMBER, object.obj_type(number));
    try std.testing.expectEqual(@intCast(i32, 42), object.as_number(number));
}
test "parse list" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    try std.testing.expectEqual(object.TYPE_NIL, object.obj_type(try parseString("()", allocator)));
    const list: *object.ObjHeader = try parseString("( 42 43 )", allocator);
    const car: *object.ObjHeader = object.get_car(list);
    const cadr: *object.ObjHeader = object.get_car(object.get_cdr(list));
    try std.testing.expectEqual(object.TYPE_NUMBER, object.obj_type(car));
    try std.testing.expectEqual(@intCast(i32, 42), object.as_number(car));
    try std.testing.expectEqual(object.TYPE_NUMBER, object.obj_type(cadr));
    try std.testing.expectEqual(@intCast(i32, 43), object.as_number(cadr));
    try std.testing.expectEqual(object.TYPE_NIL, object.obj_type(object.get_cdr(object.get_cdr(list))));
}

test "parse symbol" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    const sym = try parseString("aa", allocator);
    try std.testing.expectEqual(object.TYPE_SYMBOL, object.obj_type(sym));
    const slice = try object.as_symbol(sym, allocator);
    defer allocator.free(slice);
    const expected: [] const u8 = "aa";
    try std.testing.expectEqualStrings(expected, slice);
}


test "parse symbol +" {
    var genera_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator: std.mem.Allocator = genera_purpose_allocator.allocator();
    const sym = try parseString("+", allocator);
    try std.testing.expectEqual(object.TYPE_SYMBOL, object.obj_type(sym));
    const slice = try object.as_symbol(sym, allocator);
    defer allocator.free(slice);
    const expected: [] const u8 = "+";
    try std.testing.expectEqualStrings(expected, slice);
}