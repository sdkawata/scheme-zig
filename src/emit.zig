const object = @import("object.zig");
const eval = @import("eval.zig");
const std = @import("std");

const EmitError = error {
    MalformError,
    UnexpectedIntervalValue,
};

fn emit_create_list(e: *eval.Evaluator, s: object.Obj, codes: *std.ArrayList(eval.OpCode)) anyerror!void {
    if (object.obj_type(&s) == .nil) {
        try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_nil});
        return;
    }
    const car = object.get_car(&s);
    const cdr = object.get_cdr(&s);
    try emit(e, car, codes, false);
    try emit_create_list(e, cdr, codes);
    try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .cons});
}

fn emit_closure(e: *eval.Evaluator, args: object.Obj, body: object.Obj, codes: *std.ArrayList(eval.OpCode), tail: bool) anyerror!void {
    const func_id = try emit_func(e, body, args);
    try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .closure, .operand = @intCast(i32, func_id)});
    try emit_tail(e, codes, tail);
    return;
}

fn emit_begin(e: *eval.Evaluator, s: object.Obj, codes:*std.ArrayList(eval.OpCode), tail: bool) anyerror!void {
    if (object.obj_type(&s) == .nil) {
        try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_undef});
        try emit_tail(e,codes, tail);
    } else {
        var current = s;
        while (object.obj_type(&current) != .nil) {
            const expr = object.get_car(&current);
            if (object.obj_type(&object.get_cdr(&current)) == .nil) {
                // last element
                try emit(e, expr, codes, tail);
                try emit_tail(e,codes, tail);
            } else {
                try emit(e, expr, codes, false);
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .discard});
            }
            current = object.get_cdr(&current);
        }
    }
}

fn emit_cons(e: *eval.Evaluator, s: object.Obj, codes: *std.ArrayList(eval.OpCode), tail: bool) anyerror!void {
    const car = object.get_car(&s);
    const cdr = object.get_cdr(&s);
    if (object.obj_type(&car) == .symbol) {
        const symbol_val = try object.as_symbol(e.pool, &car);
        if (std.mem.eql(u8, symbol_val, "quote")) {
            if ((try eval.list_length(cdr)) != 1) {
                std.debug.print("malformed quote: expect 1 args but got {}\n", .{try eval.list_length(cdr)});
                return EmitError.MalformError;
            }
            const idx = e.pool.consts.items.len;
            try std.ArrayList(object.Obj).append(&e.pool.consts, object.get_car(&cdr));
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_const, .operand = @intCast(i32, idx)});
            try emit_tail(e, codes, tail);
            return;
        } else if (std.mem.eql(u8, symbol_val, "define")) {
            if ((try eval.list_length(cdr)) < 2) {
                std.debug.print("define expect 2 args but got {}\n", .{try eval.list_length(cdr)});
                return EmitError.MalformError;
            }
            var key = eval.list_1st(cdr);
            if (object.obj_type(&key) == .symbol) {
                const body = eval.list_2nd(cdr);
                try emit(e, body, codes, false);
            } else if (object.obj_type(&key) == .cons) {
                const args = object.get_cdr(&key);
                key = object.get_car(&key);
                const body = eval.list_2nd(cdr);
                try emit_closure(e, args, body, codes, false);
            } else {
                return EmitError.MalformError;
            }
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_new_var_current, .operand = @intCast(i32, object.get_symbol_id(&key))});
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_undef});
            try emit_tail(e, codes, tail);
            return;
        } else if (std.mem.eql(u8, symbol_val, "lambda")) {
            if ((try eval.list_length(cdr)) < 2) {
                std.debug.print("lambda expect 2 args but got {}\n", .{try eval.list_length(cdr)});
                return EmitError.MalformError;
            }
            const args = eval.list_1st(cdr);
            const body = eval.list_2nd(cdr);
            try emit_closure(e, args, body, codes, tail);
            return;
        } else if (std.mem.eql(u8, symbol_val, "if")) {
            const length = try eval.list_length(cdr);
            if (length != 2 and length != 3) {
                std.debug.print("malformed if expect 2 or 3 args but got {}\n", .{try eval.list_length(cdr)});
                return EmitError.MalformError;
            }
            const cond = object.get_car(&cdr);
            try emit(e, cond, codes, false);
            const first_jmp = codes.items.len;
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .jmp_if_false, .operand = 0});

            // true branch
            try emit(e, eval.list_2nd(cdr), codes, tail);
            const true_branch_jp = codes.items.len;
            if (!tail) {
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .jmp, .operand = 0});
            }
            codes.items[first_jmp].operand = @intCast(i32, codes.items.len);

            //false branch
            if (length == 2) {
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_undef});
                try emit_tail(e,codes, tail);
            } else {
                try emit(e, eval.list_3rd(cdr), codes, tail);
            }

            if (!tail) {
                codes.items[true_branch_jp].operand = @intCast(i32, codes.items.len);
            }
            return;
        } else if (std.mem.eql(u8, symbol_val, "and")) {
            const length = try eval.list_length(cdr);
            if (length == 0) {
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_true});
                try emit_tail(e,codes, tail);
                return;
            }
            var jmpAddrs = std.ArrayList(usize).init(e.allocator);
            defer std.ArrayList(usize).deinit(jmpAddrs);
            var current_args = cdr;
            while (object.obj_type(&current_args) != .nil) {
                const cond = object.get_car(&current_args);
                if (object.obj_type(&object.get_cdr(&current_args)) != .nil) {
                    try emit(e, cond, codes, false);
                    try std.ArrayList(usize).append(&jmpAddrs, codes.items.len);
                    try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .jmp_if_false});
                } else {
                    try emit(e, cond, codes, tail);
                    try emit_tail(e,codes, tail);
                }
                current_args = object.get_cdr(&current_args);
            }
            for (jmpAddrs.items) |jmpAddr| {
                codes.items[jmpAddr].operand = @intCast(i32, codes.items.len);
            }
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_false});
            try emit_tail(e,codes, tail);
            return;
        } else if (std.mem.eql(u8, symbol_val, "or")) {
            const length = try eval.list_length(cdr);
            if (length == 0) {
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_false});
                try emit_tail(e,codes, tail);
                return;
            }
            var jmpAddrs = std.ArrayList(usize).init(e.allocator);
            defer std.ArrayList(usize).deinit(jmpAddrs);
            var current_args = cdr;
            while (object.obj_type(&current_args) != .nil) {
                const cond = object.get_car(&current_args);
                if (object.obj_type(&object.get_cdr(&current_args)) != .nil) {
                    try emit(e, cond, codes, false);
                    try std.ArrayList(usize).append(&jmpAddrs, codes.items.len);
                    try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .jmp_if_true_preserve_true});
                } else {
                    try emit(e, cond, codes, tail);
                }
                current_args = object.get_cdr(&current_args);
            }
            for (jmpAddrs.items) |jmpAddr| {
                codes.items[jmpAddr].operand = @intCast(i32, codes.items.len);
            }
            try emit_tail(e,codes, tail);
            return;
        } else if (std.mem.eql(u8, symbol_val, "let")) {
            const length = try eval.list_length(cdr);
            if (length != 2) {
                std.debug.print("let expect 2 but got {}\n", .{try eval.list_length(cdr)});
                return EmitError.MalformError;
            }

            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .new_frame});

            const bindlist = eval.list_1st(cdr);
            var current_binds = bindlist;
            while (object.obj_type(&current_binds) != .nil) {
                const bind_pair = object.get_car(&current_binds);
                const bind_pair_length = try eval.list_length(cdr);
                if (bind_pair_length != 2) {
                    std.debug.print("illegal let form\n", .{});
                    return EmitError.MalformError;
                }
                const symbol =eval.list_1st(bind_pair);
                const body = eval.list_2nd(bind_pair);
                try emit(e, body, codes, false);
                const symbol_id = object.get_symbol_id(&symbol);
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_new_var, .operand=@intCast(i32, symbol_id)});
                current_binds = object.get_cdr(&current_binds);
            }
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .set_frame});
            try emit_begin(e, object.get_cdr(&cdr), codes, tail);
            if (! tail) {
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .set_frame_previous});
            }
            return;
        } else if (std.mem.eql(u8, symbol_val, "begin")) {
            try emit_begin(e, cdr, codes, tail);
            return;
        } else if (std.mem.eql(u8, symbol_val, "letrec")) {
            const length = try eval.list_length(cdr);
            if (length != 2) {
                std.debug.print("letrec expect 2 but got {}\n", .{try eval.list_length(cdr)});
                return EmitError.MalformError;
            }

            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .new_frame});

            const bindlist = eval.list_1st(cdr);
            var current_binds = bindlist;
            while (object.obj_type(&current_binds) != .nil) {
                const bind_pair = object.get_car(&current_binds);
                const bind_pair_length = try eval.list_length(bind_pair);
                if (bind_pair_length != 2) {
                    std.debug.print("illegal letrec form\n", .{});
                    return EmitError.MalformError;
                }
                const symbol_id = object.get_symbol_id(&eval.list_1st(bind_pair));
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_undef});
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_new_var, .operand=@intCast(i32, symbol_id)});
                current_binds = object.get_cdr(&current_binds);
            }
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .set_frame});

            current_binds = bindlist;
            while (object.obj_type(&current_binds) != .nil) {
                const bind_pair = object.get_car(&current_binds);
                const symbol_id = object.get_symbol_id(&eval.list_1st(bind_pair));
                const body = eval.list_2nd(bind_pair);
                try emit(e, body, codes, false);
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_new_var_current, .operand=@intCast(i32, symbol_id)});
                current_binds = object.get_cdr(&current_binds);
            }
            try emit_begin(e, object.get_cdr(&cdr), codes, tail);
            if (! tail) {
                try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .set_frame_previous});
            }
            return;
        }
    }
    try emit(e, car, codes, false);
    try emit_create_list(e, cdr, codes);
    try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = if(tail) .tailcall else .call });
}

fn emit_tail(_: *eval.Evaluator, codes: *std.ArrayList(eval.OpCode), tail: bool) !void {
    if (tail) {
        try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .ret});
    }
}

fn emit(e: *eval.Evaluator, s: object.Obj, codes: *std.ArrayList(eval.OpCode), tail: bool) !void {
    return switch(object.obj_type(&s)) {
        .b_true => {
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_true});
            try emit_tail(e,codes,tail);
        },
        .b_false => {
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_false});
            try emit_tail(e,codes,tail);
        },
        .number => {
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_number, .operand = object.as_number(&s)});
            try emit_tail(e,codes,tail);
        },
        .nil => {
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_nil});
            try emit_tail(e,codes,tail);
        },
        .char => {
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .push_char, .operand = @intCast(i32, object.get_char_value(&s))});
            try emit_tail(e,codes,tail);
        },
        .buildin => EmitError.UnexpectedIntervalValue,
        .undef => EmitError.UnexpectedIntervalValue,
        .frame => EmitError.UnexpectedIntervalValue,
        .symbol => {
            try std.ArrayList(eval.OpCode).append(codes, eval.OpCode{.tag = .lookup, .operand = @intCast(i32, object.get_symbol_id(&s))});
            try emit_tail(e,codes,tail);
        },
        .func => EmitError.UnexpectedIntervalValue,
        .cons => {
            try emit_cons(e, s, codes, tail);
        },
    };
}

pub fn emit_func(e: *eval.Evaluator, s: object.Obj, args: object.Obj) !usize {
    var codes = std.ArrayList(eval.OpCode).init(e.allocator);
    defer std.ArrayList(eval.OpCode).deinit(codes);

    if (object.obj_type(&args) != .nil) {
        var current_args = args;
        while (true) {
            if (object.obj_type(&current_args) != .cons) {
                std.debug.print("illegal func arg param\n", .{});
                return EmitError.MalformError;
            }
            const symbol = object.get_car(&current_args);
            const symbol_id = object.get_symbol_id(&symbol);
            const cdr = object.get_cdr(&current_args);
            if (object.obj_type(&cdr) != .nil) {
                try std.ArrayList(eval.OpCode).append(&codes, eval.OpCode{.tag = .dup_car});
                try std.ArrayList(eval.OpCode).append(&codes, eval.OpCode{.tag = .push_new_var_current, .operand=@intCast(i32, symbol_id)});
                try std.ArrayList(eval.OpCode).append(&codes, eval.OpCode{.tag = .cdr});
                current_args = cdr;
                continue;
            } else {
                try std.ArrayList(eval.OpCode).append(&codes, eval.OpCode{.tag = .car});
                try std.ArrayList(eval.OpCode).append(&codes, eval.OpCode{.tag = .push_new_var_current, .operand=@intCast(i32, symbol_id)});
                break;
            }
        }
    }

    try emit(e, s, &codes, true);
    const idx = e.compiled_funcs.items.len;
    try std.ArrayList(eval.CompiledFunc).append(&e.compiled_funcs, eval.CompiledFunc {
        .codes = std.ArrayList(eval.OpCode).toOwnedSlice(&codes),
    });
    // eval.debug_print_func(e, idx);
    // eval.debug_print_symbols(e);
    return idx;
}
