const object = @import("object.zig");
const std = @import("std");

const EvalError  = error {
    VariableNotFound,
    IllegalApplication,
    IllegalParameter,
    UnexpectedIntervalValue,
    InternalError,
    MaxRecursiveCallExceeds,
};

// <- stack bottom stack top ->
const OpCodeTag = enum(u32) {
    ret, // operand: no stack: VAL ->
    call, // operand: no stack: FUNC, ARGS -> RET
    tailcall, // operand: no stack: FUNC, ARGS -> RET
    lookup, // operand: symbol number stack: -> VAL
    define, // operand: symbol number stack: VAL ->
    cons, // operand: no stack: CAR CDR -> CONS
    car, // operand: no stack: CONS -> CAR
    cdr, // operand: no stack: CONS -> CDR
    dup_car, // operand: no stack: CONS -> CONS CAR
    dup_cdr, // operand: no stack: CONS -> CONS CDR
    push_number, // operand: number stack: -> NUMBER
    push_true, //operand no stack: -> TRUE
    push_false, //operand no stack: -> FALSE
    push_nil, //operand no stack: -> NIL
    push_undef, //operand: no stack: -> UNDEF
    push_const, //operand: constno stack: -> VAL
    new_frame, //operand:no stack: -> FRAME
    push_new_var, // operand: symbol number stack: FRAME VAL -> FRAME
    push_new_var_current, // operand: symbol number stack: VAL ->
    set_frame, // operand no stack: FRAME ->
    closure, // operand: compiled_func_id stack: -> CLOSURE
    jmp, // operand: addr stack: none
    jmp_if_false, // operand: addr stack: VAL ->
};

const OpCode = packed struct {
    tag: OpCodeTag,
    operand: i32 = 0,
};

const CompiledFunc = struct {
    codes: [] OpCode,
    consts: [] object.Obj,
};

const FuncFrame = struct {
    ret_func: usize,
    ret_addr: usize,
    base_stack_pointer: usize, // stack pointer when enter this function
    base_env: object.Obj,
};

pub const Evaluator = struct {
    pool: *object.ObjPool,
    globals: object.Obj,
    compiled_funcs: std.ArrayList(CompiledFunc),
    allocator: std.mem.Allocator,
    func_frames: std.ArrayList(FuncFrame),
    program_pointer: usize,
    current_func: usize,
    current_env: object.Obj,
};

const BuildinFunc = enum(i32) {
    plus,
    equal,
    null_p,
    car,
    cdr,
    minus,
    cons,
};

const MAX_RECURSIVE_CALL = 100;

fn push_buildin_func(pool: *object.ObjPool, env: object.Obj, name: [] const u8, f: BuildinFunc) !void {
    const symbol = try object.create_symbol(pool, name);
    try object.push_frame_var(pool, env, symbol, try object.create_opaque(pool, @enumToInt(f)));
}

pub fn create_evaluator(allocator: std.mem.Allocator) !*Evaluator {
    const evaluator = try allocator.create(Evaluator);
    const pool = try object.create_obj_pool(allocator);
    evaluator.pool = pool;
    const g = try object.create_frame(evaluator.pool, try object.create_nil(evaluator.pool), try object.create_nil(evaluator.pool));
    evaluator.globals = g;
    try push_buildin_func(pool, g, "+", .plus);
    try push_buildin_func(pool, g, "=", .equal);
    try push_buildin_func(pool, g, "null?", .null_p);
    try push_buildin_func(pool, g, "car", .car);
    try push_buildin_func(pool, g, "cdr", .cdr);
    try push_buildin_func(pool, g, "-", .minus);
    try push_buildin_func(pool, g, "cons", .cons);
    evaluator.allocator = allocator;
    evaluator.compiled_funcs = std.ArrayList(CompiledFunc).init(allocator);
    evaluator.func_frames = std.ArrayList(FuncFrame).init(allocator);
    return evaluator;
}

pub fn destroy_evaluator(e: *Evaluator) void {
    object.destroy_obj_pool(e.pool);
    for (e.compiled_funcs.items) |func| {
        e.allocator.free(func.codes);
        e.allocator.free(func.consts);
    }
    std.ArrayList(CompiledFunc).deinit(e.compiled_funcs);
    std.ArrayList(FuncFrame).deinit(e.func_frames);
    const allocator = e.allocator;
    allocator.destroy(e);
}

fn lookup_buildin_func(f: BuildinFunc) fn(*Evaluator, object.Obj, object.Obj)anyerror!object.Obj {
    return switch(f) {
        .plus => apply_plus,
        .equal => apply_equal,
        .null_p => apply_null_p,
        .car => apply_car,
        .cdr => apply_cdr,
        .minus => apply_minus,
        .cons => apply_cons,
    };
}

fn peek_stack_top(e: *Evaluator) object.Obj {
    return e.pool.stack_frames.items[e.pool.stack_frames.items.len - 1];
}

fn apply_plus(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    var result: i32 = 0;
    var current = s;
    while (object.obj_type(current) == .cons) {
        const car = object.get_car(current);
        if (object.obj_type(car) != .number) {
            return EvalError.IllegalParameter;
        }
        result += object.as_number(car);
        current = object.get_cdr(current);
    }
    if (object.obj_type(current) != .nil) {
        return EvalError.IllegalParameter;
    }
    return object.create_number(e.pool, result);
}

fn apply_equal(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 2) {
        std.debug.print("= expect 2 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    const left = object.get_car(s);
    const right = object.get_car(object.get_cdr(s));
    if (object.obj_type(left) != .number or object.obj_type(right) != .number) {
        std.debug.print("must given number\n", .{});
        return EvalError.IllegalParameter;
    }
    if (object.as_number(left) == object.as_number(right)) {
        return object.create_true(e.pool);
    } else {
        return object.create_false(e.pool);
    }
}

fn apply_minus(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 2) {
        std.debug.print("= expect 2 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    const left = object.get_car(s);
    const right = object.get_car(object.get_cdr(s));
    if (object.obj_type(left) != .number or object.obj_type(right) != .number) {
        std.debug.print("must given number\n", .{});
        return EvalError.IllegalParameter;
    }
    return object.create_number(
        e.pool,
        object.as_number(left) - object.as_number(right)
    );
}


fn apply_null_p(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 1) {
        std.debug.print("nil? expect 1 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    if (object.obj_type(object.get_car(s)) == .nil) {
        return object.create_true(e.pool);
    } else {
        return object.create_false(e.pool);
    }
}

fn apply_car(_: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 1) {
        std.debug.print("car expect 1 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    const car = object.get_car(s);
    if (object.obj_type(car) != .cons) {
        std.debug.print("car got non-list\n", .{});
        return EvalError.IllegalParameter;
    }
    return object.get_car(car);
}

fn apply_cdr(_: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 1) {
        std.debug.print("cdr expect 1 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    const car = object.get_car(s);
    if (object.obj_type(car) != .cons) {
        std.debug.print("car got non-list\n", .{});
        return EvalError.IllegalParameter;
    }
    return object.get_cdr(car);
}

fn apply_cons(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 2) {
        std.debug.print("cons expect 2 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    const car = object.get_car(s);
    const cadr = object.get_car(object.get_cdr(s));
    return object.create_cons(e.pool, car, cadr);
}

fn list_length(s: object.Obj) anyerror!usize {
    if (object.obj_type(s) == .nil) {
        return @intCast(usize, 0);
    }
    if (object.obj_type(s) != .cons) {
        return EvalError.IllegalParameter;
    }
    return (try list_length(object.get_cdr(s))) + 1;
}

fn eval_apply_buildin(e:*Evaluator, func: object.Obj, args: object.Obj) !void {
    const buildin_func = lookup_buildin_func(@intToEnum(BuildinFunc, object.get_buildin_value(func)));
    const ret = try buildin_func(e, args, e.current_env);
    try push_stack(e, ret);
}

fn ret_to_previous_func(e: *Evaluator, retval: object.Obj) !void {
    const prev_func_frame = std.ArrayList(FuncFrame).pop(&e.func_frames);
    e.program_pointer = prev_func_frame.ret_addr;
    e.current_func = prev_func_frame.ret_func;
    try std.ArrayList(object.Obj).resize(&e.pool.stack_frames, prev_func_frame.base_stack_pointer);
    e.current_env = prev_func_frame.base_env;
    try push_stack(e, retval);
}

fn pop_stack(e: *Evaluator) object.Obj {
    return std.ArrayList(object.Obj).pop(&e.pool.stack_frames);
}

fn push_stack(e: *Evaluator, obj:object.Obj) !void {
    try std.ArrayList(object.Obj).append(&e.pool.stack_frames, obj);
}

fn eval_loop(e: *Evaluator) !object.Obj {
    while(true) {
        const current_opcode = e.compiled_funcs.items[e.current_func].codes[e.program_pointer];
        // std.debug.print("fun={} pp={} sp={} opcode={}\n", .{e.current_func, e.program_pointer, e.pool.stack_frames.items.len, current_opcode});
        switch (current_opcode.tag) {
            .call => {
                if (e.func_frames.items.len > MAX_RECURSIVE_CALL) {
                    return EvalError.MaxRecursiveCallExceeds;
                }
                const args = pop_stack(e);
                const func = pop_stack(e);
                switch (object.obj_type(func)) {
                    .buildin => try eval_apply_buildin(e, func, args),
                    .func => {
                        try std.ArrayList(FuncFrame).append(&e.func_frames, FuncFrame {
                            .ret_func = e.current_func,
                            .ret_addr = e.program_pointer,
                            .base_stack_pointer = e.pool.stack_frames.items.len,
                            .base_env = e.current_env,
                        });
                        const function_id = @intCast(usize, object.get_func_id(func));
                        e.current_env = object.get_func_env(func);
                        e.current_func = function_id;
                        e.program_pointer = 0;
                        try push_stack(e, args);
                        continue;
                    },
                    else => return EvalError.IllegalApplication,
                }
            },
            .tailcall => {
                const args = pop_stack(e);
                const func = pop_stack(e);
                switch (object.obj_type(func)) {
                    .buildin => {
                        try eval_apply_buildin(e, func, args);
                        const retval = pop_stack(e);
                        if (e.func_frames.items.len == 0) {
                            return retval;
                        }
                        try ret_to_previous_func(e, retval);
                    },
                    .func => {
                        if (e.func_frames.items.len > 0) {
                            const current_func_frame = e.func_frames.items[e.func_frames.items.len - 1];
                            try std.ArrayList(object.Obj).resize(&e.pool.stack_frames, current_func_frame.base_stack_pointer);
                        } else {
                            try std.ArrayList(object.Obj).resize(&e.pool.stack_frames, 0);
                        }
                        const function_id = @intCast(usize, object.get_func_id(func));
                        e.current_env = object.get_func_env(func);
                        e.current_func = function_id;
                        e.program_pointer = 0;
                        try push_stack(e, args);
                        continue;
                    },
                    else => return EvalError.IllegalApplication,
                }
            },
            .lookup => {
                const val = object.lookup_frame(e.current_env, @intCast(usize, current_opcode.operand)) catch |err| if (err == object.LookUpError.NotFound) {
                   return EvalError.VariableNotFound;
                } else {return err;};
                try std.ArrayList(object.Obj).append(
                    &e.pool.stack_frames,
                    val
                );
            },
            .define => {
                const val = pop_stack(e);
                const key = try object.create_symbol_by_id(e.pool, @intCast(usize, current_opcode.operand));
                try object.push_frame_var(e.pool, e.globals, key, val);
                try push_stack(e, try object.create_undef(e.pool));
            },
            .cons => {
                const cdr = pop_stack(e);
                const car = pop_stack(e);
                const cons = try object.create_cons(e.pool, car, cdr);
                try push_stack(e, cons);
            },
            .car => {
                const cons = pop_stack(e);
                const car = object.get_car(cons);
                try push_stack(e, car);
            },
            .cdr => {
                const cons = pop_stack(e);
                const cdr = object.get_cdr(cons);
                try push_stack(e, cdr);
            },
            .dup_car => {
                const cons = peek_stack_top(e);
                const car = object.get_car(cons);
                try push_stack(e, car);
            },
            .dup_cdr => {
                const cons = peek_stack_top(e);
                const cdr = object.get_cdr(cons);
                try push_stack(e, cdr);
            },
            .push_number => {
                try push_stack(e, try object.create_number(e.pool, current_opcode.operand));
            },
            .push_true => {
                try push_stack(e, try object.create_true(e.pool));
            },
            .push_false => {
                try push_stack(e, try object.create_false(e.pool));
            },
            .push_nil => {
                try push_stack(e, try object.create_nil(e.pool));
            },
            .push_undef => {
                try push_stack(e, try object.create_undef(e.pool));
            },
            .push_const => {
                try push_stack(e, e.compiled_funcs.items[e.current_func].consts[@intCast(usize, current_opcode.operand)]);
            },
            .new_frame => {
                const new_frame = try object.create_frame(e.pool, try object.create_nil(e.pool), e.current_env);
                try push_stack(e, new_frame);
            },
            .push_new_var => {
                const val = pop_stack(e);
                const frame = peek_stack_top(e);
                const key = try object.create_symbol_by_id(e.pool, @intCast(usize, current_opcode.operand));
                try object.push_frame_var(e.pool, frame, key, val);
            },
            .push_new_var_current => {
                const val = pop_stack(e);
                const key = try object.create_symbol_by_id(e.pool, @intCast(usize, current_opcode.operand));
                try object.push_frame_var(e.pool, e.current_env, key, val);
            },
            .set_frame => {
                const frame = pop_stack(e);
                std.debug.assert(object.obj_type(frame) == .frame);
                e.current_env = frame;
            },
            .closure => {
                const function_id = @intCast(usize, current_opcode.operand);
                const func = try object.create_func(e.pool, function_id, e.current_env);
                try push_stack(e, func);
            },
            .ret => {
                const retval = pop_stack(e);
                if (e.func_frames.items.len == 0) {
                    return retval;
                }
                try ret_to_previous_func(e, retval);
            },
            .jmp => {
                const ptr = @intCast(usize, current_opcode.operand);
                e.program_pointer = ptr;
                continue;
            },
            .jmp_if_false => {
                const ptr = @intCast(usize, current_opcode.operand);
                const val = pop_stack(e);
                if (object.obj_type(val) == .b_false) {
                    e.program_pointer = ptr;
                    continue;
                }
            }
        }
        e.program_pointer+=1;
    }
}

fn emit_create_list(e: *Evaluator, s: object.Obj, codes: *std.ArrayList(OpCode), consts: *std.ArrayList(object.Obj)) anyerror!void {
    if (object.obj_type(s) == .nil) {
        try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_nil});
        return;
    }
    const car = object.get_car(s);
    const cdr = object.get_cdr(s);
    try emit(e, car, codes, consts, false);
    try emit_create_list(e, cdr, codes, consts);
    try std.ArrayList(OpCode).append(codes, OpCode{.tag = .cons});
}

fn emit_cons(e: *Evaluator, s: object.Obj, codes: *std.ArrayList(OpCode), consts: *std.ArrayList(object.Obj), tail: bool) anyerror!void {
    const car = object.get_car(s);
    const cdr = object.get_cdr(s);
    if (object.obj_type(car) == .symbol) {
        const symbol_val = try object.as_symbol(e.pool, car);
        if (std.mem.eql(u8, symbol_val, "quote")) {
            if ((try list_length(cdr)) != 1) {
                std.debug.print("malformed quote: expect 1 args but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }
            const idx = consts.items.len;
            try std.ArrayList(object.Obj).append(consts, object.get_car(cdr));
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_const, .operand = @intCast(i32, idx)});
            try emit_tail(e, codes, consts, tail);
            return;
        } else if (std.mem.eql(u8, symbol_val, "define")) {
            if ((try list_length(cdr)) < 2) {
                std.debug.print("define expect 2 args but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }
            const key = object.get_car(cdr);
            if (object.obj_type(key) != .symbol) {
                return EvalError.IllegalParameter;
            }
            const caddr = object.get_car(object.get_cdr(cdr));
            try emit(e, caddr, codes, consts, false);
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .define, .operand = @intCast(i32, object.get_symbol_id(key))});
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_undef});
            try emit_tail(e, codes, consts, tail);
            return;
        } else if (std.mem.eql(u8, symbol_val, "lambda")) {
            if ((try list_length(cdr)) < 2) {
                std.debug.print("lambda expect 2 args but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }
            const args = object.get_car(cdr);
            const body = object.get_car(object.get_cdr(cdr));
            const func_id = try emit_func(e, body, args);
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .closure, .operand = @intCast(i32, func_id)});
            try emit_tail(e, codes, consts, tail);
            return;
        } else if (std.mem.eql(u8, symbol_val, "if")) {
            const length = try list_length(cdr);
            if (length != 2 and length != 3) {
                std.debug.print("malformed if expect 2 or 3 args but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }
            const cond = object.get_car(cdr);
            try emit(e, cond, codes, consts, false);
            const first_jmp = codes.items.len;
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .jmp_if_false, .operand = 0});

            // true branch
            const cddr = object.get_cdr(cdr);
            try emit(e, object.get_car(cddr), codes, consts, tail);
            const true_branch_jp = codes.items.len;
            if (!tail) {
                try std.ArrayList(OpCode).append(codes, OpCode{.tag = .jmp, .operand = 0});
            }
            codes.items[first_jmp].operand = @intCast(i32, codes.items.len);

            //false branch
            if (length == 2) {
                try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_undef});
                try emit_tail(e,codes,consts, tail);
            } else {
                const cadddr = object.get_car(object.get_cdr(cddr));
                try emit(e, cadddr, codes, consts, tail);
            }

            if (!tail) {
                codes.items[true_branch_jp].operand = @intCast(i32, codes.items.len);
            }
            return;
        } else if (std.mem.eql(u8, symbol_val, "let")) {
            const length = try list_length(cdr);
            if (length != 2) {
                std.debug.print("let expect 2 but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }

            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .new_frame});

            const bindlist = object.get_car(cdr);
            var current_binds = bindlist;
            while (object.obj_type(current_binds) != .nil) {
                const bind_pair = object.get_car(current_binds);
                const bind_pair_length = try list_length(cdr);
                if (bind_pair_length != 2) {
                    std.debug.print("illegal let form\n", .{});
                    return EvalError.IllegalParameter;
                }
                const symbol = object.get_car(bind_pair);
                const body = object.get_car(object.get_cdr(bind_pair));
                try emit(e, body, codes, consts, false);
                const symbol_id = object.get_symbol_id(symbol);
                try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_new_var, .operand=@intCast(i32, symbol_id)});
                current_binds = object.get_cdr(current_binds);
            }
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .set_frame});
            const body = object.get_car(object.get_cdr(cdr));
            try emit(e, body, codes, consts, tail);
            return;
        } else if (std.mem.eql(u8, symbol_val, "letrec")) {
            const length = try list_length(cdr);
            if (length != 2) {
                std.debug.print("letrec expect 2 but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }

            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .new_frame});

            const bindlist = object.get_car(cdr);
            var current_binds = bindlist;
            while (object.obj_type(current_binds) != .nil) {
                const bind_pair = object.get_car(current_binds);
                const bind_pair_length = try list_length(bind_pair);
                if (bind_pair_length != 2) {
                    std.debug.print("illegal letrec form\n", .{});
                    return EvalError.IllegalParameter;
                }
                const symbol = object.get_car(bind_pair);
                const symbol_id = object.get_symbol_id(symbol);
                try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_undef});
                try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_new_var, .operand=@intCast(i32, symbol_id)});
                current_binds = object.get_cdr(current_binds);
            }
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .set_frame});

            current_binds = bindlist;
            while (object.obj_type(current_binds) != .nil) {
                const bind_pair = object.get_car(current_binds);
                const symbol = object.get_car(bind_pair);
                const symbol_id = object.get_symbol_id(symbol);
                const body = object.get_car(object.get_cdr(bind_pair));
                try emit(e, body, codes, consts, false);
                try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_new_var_current, .operand=@intCast(i32, symbol_id)});
                current_binds = object.get_cdr(current_binds);
            }
            const body = object.get_car(object.get_cdr(cdr));
            try emit(e, body, codes, consts, tail);
            return;
        }
    }
    try emit(e, car, codes, consts, false);
    try emit_create_list(e, cdr, codes, consts);
    try std.ArrayList(OpCode).append(codes, OpCode{.tag = if(tail) .tailcall else .call });
}

fn emit_tail(_: *Evaluator, codes: *std.ArrayList(OpCode), _: *std.ArrayList(object.Obj), tail: bool) !void {
    if (tail) {
        try std.ArrayList(OpCode).append(codes, OpCode{.tag = .ret});
    }
}

fn emit(e: *Evaluator, s: object.Obj, codes: *std.ArrayList(OpCode), consts: *std.ArrayList(object.Obj), tail: bool) !void {
    return switch(object.obj_type(s)) {
        .b_true => {
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_true});
            try emit_tail(e,codes,consts,tail);
        },
        .b_false => {
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_false});
            try emit_tail(e,codes,consts,tail);
        },
        .number => {
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_number, .operand = object.as_number(s)});
            try emit_tail(e,codes,consts,tail);
        },
        .nil => {
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_nil});
            try emit_tail(e,codes,consts,tail);
        },
        .buildin => EvalError.UnexpectedIntervalValue,
        .undef => EvalError.UnexpectedIntervalValue,
        .frame => EvalError.UnexpectedIntervalValue,
        .symbol => {
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .lookup, .operand = @intCast(i32, object.get_symbol_id(s))});
            try emit_tail(e,codes,consts,tail);
        },
        .func => EvalError.UnexpectedIntervalValue,
        .cons => {
            try emit_cons(e, s, codes, consts, tail);
        },
    };
}

fn debug_print_func(e: *Evaluator, func_id: usize) void {
    std.debug.print("==function {}==\n", .{func_id});
    const func = e.compiled_funcs.items[func_id];
    for (func.codes) |code, i| {
        std.debug.print("p={} {}\n", .{i, code});
    }
    std.debug.print("==end function {}==\n", .{func_id});
}

pub fn emit_func(e: *Evaluator, s: object.Obj, args: object.Obj) !usize {
    var codes = std.ArrayList(OpCode).init(e.allocator);
    defer std.ArrayList(OpCode).deinit(codes);
    var consts = std.ArrayList(object.Obj).init(e.allocator);
    defer std.ArrayList(object.Obj).deinit(consts);

    if (object.obj_type(args) != .nil) {
        var current_args = args;
        while (true) {
            if (object.obj_type(current_args) != .cons) {
                std.debug.print("illegal func arg param\n", .{});
                return EvalError.IllegalParameter;
            }
            const symbol = object.get_car(current_args);
            const symbol_id = object.get_symbol_id(symbol);
            const cdr = object.get_cdr(current_args);
            if (object.obj_type(cdr) != .nil) {
                try std.ArrayList(OpCode).append(&codes, OpCode{.tag = .dup_car});
                try std.ArrayList(OpCode).append(&codes, OpCode{.tag = .push_new_var_current, .operand=@intCast(i32, symbol_id)});
                try std.ArrayList(OpCode).append(&codes, OpCode{.tag = .cdr});
                current_args = cdr;
                continue;
            } else {
                try std.ArrayList(OpCode).append(&codes, OpCode{.tag = .car});
                try std.ArrayList(OpCode).append(&codes, OpCode{.tag = .push_new_var_current, .operand=@intCast(i32, symbol_id)});
                break;
            }
        }
    }

    try emit(e, s, &codes, &consts, true);
    const idx = e.compiled_funcs.items.len;
    try std.ArrayList(CompiledFunc).append(&e.compiled_funcs, CompiledFunc {
        .codes = std.ArrayList(OpCode).toOwnedSlice(&codes),
        .consts = std.ArrayList(object.Obj).toOwnedSlice(&consts),
    });
    // debug_print_func(e, idx);
    return idx;
}

pub fn eval_compiled_global(e: *Evaluator, func_no: usize) !object.Obj {
    try std.ArrayList(object.Obj).resize(&e.pool.stack_frames, 0);
    try std.ArrayList(FuncFrame).resize(&e.func_frames, 0);
    e.current_func = func_no;
    e.program_pointer = 0;
    e.current_env = e.globals;
    return eval_loop(e);
}

pub fn eval_global(e: *Evaluator, s: object.Obj) !object.Obj {
    // object.debug_println_obj(e.pool, s);
    const idx = try emit_func(e, s, try object.create_nil(e.pool));
    return eval_compiled_global(e, idx);
}
