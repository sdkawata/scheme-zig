const std = @import("std");
const object = @import("object.zig");
const debug_print = @import("debug.zig").debug_print;

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

const SpecialCharPair = std.meta.Tuple(&.{u8, []const u8});

pub const special_chars = [_]SpecialCharPair{
    .{' ', "space"},
    .{'\n', "newline"},
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
        } else if (peeked == ';') {
            p.p+=1;
            while(true) {
                const peeked2 = peek(p,0) catch return;
                if (peeked2 == '\n') {
                    break;
                }
                p.p+=1;
            }
        } else {
            return;
        }
    }
}

fn parse_number(p: *Parser, pool: *object.ObjPool, sign: i32) !object.Obj {
    var is_float = false;
    const start = p.p;
    while(true) {
        const peeked = peek(p, 0) catch 0;
        if (peeked >= '0' and peeked <= '9') {
        } else if (peeked == '.' or peeked == 'e' or peeked == '-'  or peeked == '+') {
            is_float = true;
        } else {
            break;
        }
        p.p+=1;
    }
    if (is_float) {
        return try object.create_float(pool, (try std.fmt.parseFloat(f32, p.s[start..p.p])) * @intToFloat(f32, sign));
    } else {
        return try object.create_number(pool, (try std.fmt.parseInt(i32, p.s[start..p.p], 10)) * sign);
    }
}

fn parse_list(p: *Parser, pool: *object.ObjPool) anyerror!object.Obj {
    if ((try peek(p, 0)) == ')') {
        p.p+=1;
        return object.create_nil(pool);
    } else {
        var car: object.Obj = try parse_datum(p, pool);
        try skip_whitespaces(p);
        var cdr = try parse_list(p, pool);
        return object.create_cons(pool, &car, &cdr);
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

fn parse_char(p: *Parser, pool: *object.ObjPool) anyerror!object.Obj {
    const start_pos = p.p;
    const peeked = try peek(p,0);
    if (peeked >= 'a' and peeked <= 'z') {
        while (true) {
            const current_peeked = peek(p,0) catch 0;
            if (current_peeked >= 'a' and current_peeked <= 'z') {
                p.p+=1;
            } else {
                break;
            }
        }
    } else {
        p.p+=1;
    }
    const char_name = p.s[start_pos..p.p];
    if (char_name.len == 1) {
        return try object.create_char(pool, char_name[0]);
    } else {
        for (special_chars) |special_char| {
            if (std.mem.eql(u8, char_name, special_char[1])) {
                return try object.create_char(pool, special_char[0]);
            }
        }
        return ParseError.UnexpectedToken;
    }
    return ParseError.UnexpectedToken;
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
            '\\' => {
                p.p+=2;
                return try parse_char(p, pool);
            },
            else => return ParseError.UnexpectedToken,
        }
    } else if ((peeked >= '0' and peeked <= '9') or peeked == '.') {
        return try parse_number(p, pool, 1);
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
        const next = peek(p, 1) catch 0;
        if ('0' <= next and next <= '9') {
           p.p+=1;
           return try parse_number(p, pool, if (peeked == '+') 1 else -1);
        } else {
            const symbol = object.create_symbol(pool, p.s[p.p..p.p+1]);
            p.p+=1;
            try skip_whitespaces(p);
            return symbol;
        }
    }
    debug_print("unexpected token pos:{} token:{}\n", .{p.p, peeked});
    return ParseError.UnexpectedToken;
}

pub fn parse(p:*Parser, pool: *object.ObjPool) !object.Obj {
    const prev_gc_enabled = pool.gc_enabled;
    pool.gc_enabled = false;
    defer pool.gc_enabled = prev_gc_enabled;
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
    try std.testing.expectEqual(object.ObjType.b_true, object.obj_type(&v_true));
    const v_false = try parse_string("#f", pool);
    try std.testing.expectEqual(object.ObjType.b_false, object.obj_type(&v_false));
}

test "parse char" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    const char_a: object.Obj = try parse_string("#\\a", pool);
    try std.testing.expectEqual(object.ObjType.char, object.obj_type(&char_a));
    try std.testing.expectEqual(@intCast(u8, 'a'), object.get_char_value(&char_a));
    const char_space: object.Obj = try parse_string("#\\space", pool);
    try std.testing.expectEqual(object.ObjType.char, object.obj_type(&char_space));
    try std.testing.expectEqual(@intCast(u8, ' '), object.get_char_value(&char_space));
}

test "parse number with comment" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    const number: object.Obj = try parse_string("42 ; -> this is comment !! <-", pool);
    try std.testing.expectEqual(object.ObjType.number, object.obj_type(&number));
    try std.testing.expectEqual(@intCast(i32, 42), object.as_number(&number));
}

test "parse float" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    const number: object.Obj = try parse_string("1.5", pool);
    try std.testing.expectEqual(object.ObjType.float, object.obj_type(&number));
    try std.testing.expectEqual(@floatCast(f32, 1.5), object.as_float(&number));
    const number2: object.Obj = try parse_string(".5", pool);
    try std.testing.expectEqual(object.ObjType.float, object.obj_type(&number2));
    try std.testing.expectEqual(@floatCast(f32, 0.5), object.as_float(&number2));
}

test "parse list" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    const nil = try parse_string("()", pool);
    try std.testing.expectEqual(object.ObjType.nil, object.obj_type(&nil));
    const list: object.Obj = try parse_string("( 42 43 )", pool);
    const car: object.Obj = object.get_car(&list);
    const cadr: object.Obj = object.get_car(&object.get_cdr(&list));
    const cddr: object.Obj = object.get_cdr(&object.get_cdr(&list));
    try std.testing.expectEqual(object.ObjType.number, object.obj_type(&car));
    try std.testing.expectEqual(@intCast(i32, 42), object.as_number(&car));
    try std.testing.expectEqual(object.ObjType.number, object.obj_type(&cadr));
    try std.testing.expectEqual(@intCast(i32, 43), object.as_number(&cadr));
    try std.testing.expectEqual(object.ObjType.nil, object.obj_type(&cddr));
}

test "parse symbol" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    const sym = try parse_string("very_long_symbol_name", pool);
    try std.testing.expectEqual(object.ObjType.symbol, object.obj_type(&sym));
    const expected: [] const u8 = "very_long_symbol_name";
    try std.testing.expectEqualStrings(expected, try object.as_symbol(pool, &sym));
}


test "parse symbol +" {
    const allocator: std.mem.Allocator = std.testing.allocator;
    const pool = try object.create_obj_pool(allocator);
    defer object.destroy_obj_pool(pool);
    const sym = try parse_string("+", pool);
    try std.testing.expectEqual(object.ObjType.symbol, object.obj_type(&sym));
    const expected: [] const u8 = "+";
    try std.testing.expectEqualStrings(expected, try object.as_symbol(pool, &sym));
}
