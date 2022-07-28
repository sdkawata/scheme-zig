const std = @import("std");
const object = @import("object.zig");

pub const ParseError  = error {
    UnexpectedToken,
    UnexpectedEOF,
    EOF,
};
pub const PeekError = error {
    EOF,
};

pub const Parser = struct {
    s: [] const u8,
    p: usize,
};

fn peek(p: *Parser, offset: usize) PeekError!u8 {
    if (p.s.len <= (p.p + offset)) {
        return PeekError.EOF;
    }
    return p.s[p.p + offset];
}

pub fn is_char_left(p: *Parser) bool {
    return p.s.len > p.p;
}

pub fn skip_whitespaces(p: *Parser) !void {
    while(true) {
        const peeked = peek(p, 0) catch return;
        if (peeked == ' ' or peeked == '\r' or peeked == '\n') {

            p.p+=1;
        } else {
            return;
        }
    }
}

fn parse_number(p: *Parser, pool: *object.ObjPool) !object.Obj {
    var val: i32 = 0;
    while(p.s.len > p.p and p.s[p.p] >= '0' and p.s[p.p] <= '9') {
        val = val * 10 + (p.s[p.p] - '0');
        p.p+=1;
    }
    return try object.create_number(pool, val);
}

fn parse_list(p: *Parser, pool: *object.ObjPool) anyerror!object.Obj {
    if ((try peek(p, 0)) == ')') {
        p.p+=1;
        return object.create_nil(pool);
    } else {
        const car: object.Obj = try parse_datum(p, pool);
        try skip_whitespaces(p);
        const cdr = try parse_list(p, pool);
        return object.create_cons(pool, car, cdr);
    }
}
fn is_special_initial(c: u8) bool {
    return (c == '!') or
        (c == '$') or
        (c == '%') or
        (c == '&') or
        (c == '*') or
        (c == '/') or
        (c == ':') or
        (c == '<') or
        (c == '=') or
        (c == '>') or
        (c == '?') or
        (c == '^') or
        (c == '_') or
        (c == '~');
}

fn parse_simple_symbol(p: *Parser, pool: *object.ObjPool, start_pos: usize) anyerror!object.Obj {
    while(true) {
        const peeked = peek(p,0) catch 0;
        if (
            (peeked >= 'a' and peeked <= 'z') or
            (peeked >= 'A' and peeked <= 'Z') or
            is_special_initial(peeked)
        ) {
            p.p+=1;
        } else {
            break;
        }
    }
    return try object.create_symbol(pool, p.s[start_pos..p.p]);
}

fn parse_datum(p:*Parser, pool: *object.ObjPool) anyerror!object.Obj {
    const peeked = try peek(p, 0);
    if (peeked == '#') {
        switch(peek(p, 1) catch return ParseError.UnexpectedEOF) {
            't' => {
                p.p+=2;
                return object.create_true(pool);
            },
            'f' => {
                p.p+=2;
                return object.create_false(pool);
            },
            else => return ParseError.UnexpectedToken,
        }
    } else if (peeked >= '0' and peeked <= '9') {
        return try parse_number(p, pool);
    } else if (peeked == '(') {
        p.p+=1;
        try skip_whitespaces(p);
        return try parse_list(p, pool);
    } else if (
        (peeked >= 'a' and peeked <= 'z') or
        (peeked >= 'A' and peeked <= 'Z') or
        is_special_initial(peeked)
    ) {
        const start_pos = p.p;
        p.p+=1;
        return try parse_simple_symbol(p, pool, start_pos);
    } else if ((peeked == '+' or peeked == '-')) {
        // TODO parse +100 and -100
        const symbol = object.create_symbol(pool, p.s[p.p..p.p+1]);
        p.p+=1;
        try skip_whitespaces(p);
        return symbol;
    }
    std.debug.print("unexpected token pos:{} token:{}\n", .{p.p, peeked});
    return ParseError.UnexpectedToken;
}

pub fn parse(p:*Parser, pool: *object.ObjPool) !object.Obj {
    try skip_whitespaces(p);
    return parse_datum(p, pool);
}


pub fn parse_string(s: [] const u8, pool: *object.ObjPool) !object.Obj {
    return try parse(
        &Parser{.s=s, .p=0,},
        pool
    );
}

test "parse true & false" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    const v_true = try parse_string("#t", pool);
    try std.testing.expectEqual(object.ObjType.b_true, object.obj_type(v_true));
    const v_false = try parse_string("#f", pool);
    try std.testing.expectEqual(object.ObjType.b_false, object.obj_type(v_false));
}

test "parse number" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    const number: object.Obj = try parse_string("42", pool);
    try std.testing.expectEqual(object.ObjType.number, object.obj_type(number));
    try std.testing.expectEqual(@intCast(i32, 42), object.as_number(number));
}
test "parse list" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    try std.testing.expectEqual(object.ObjType.nil, object.obj_type(try parse_string("()", pool)));
    const list: object.Obj = try parse_string("( 42 43 )", pool);
    const car: object.Obj = object.get_car(list);
    const cadr: object.Obj = object.get_car(object.get_cdr(list));
    try std.testing.expectEqual(object.ObjType.number, object.obj_type(car));
    try std.testing.expectEqual(@intCast(i32, 42), object.as_number(car));
    try std.testing.expectEqual(object.ObjType.number, object.obj_type(cadr));
    try std.testing.expectEqual(@intCast(i32, 43), object.as_number(cadr));
    try std.testing.expectEqual(object.ObjType.nil, object.obj_type(object.get_cdr(object.get_cdr(list))));
}

test "parse symbol" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    const sym = try parse_string("very_long_symbol_name", pool);
    try std.testing.expectEqual(object.ObjType.symbol, object.obj_type(sym));
    const expected: [] const u8 = "very_long_symbol_name";
    try std.testing.expectEqualStrings(expected, try object.as_symbol(pool, sym));
}


test "parse symbol +" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    const sym = try parse_string("+", pool);
    try std.testing.expectEqual(object.ObjType.symbol, object.obj_type(sym));
    const expected: [] const u8 = "+";
    try std.testing.expectEqualStrings(expected, try object.as_symbol(pool, sym));
}
