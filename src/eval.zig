const object = @import("object.zig");
const std = @import("std");

const EvalError  = error {
    VariableNotFound,
    IllegalApplication,
    IllegalParameter,
    UnexpectedIntervalValue,
    InternalError,
};

// <- stack bottom stack top ->
const OpCodeTag = enum(u32) {
    ret, // operand: no stack: VAL ->
    call, // operand: no stack: FUNC, ARGS -> RET
    lookup, // operand: symbol number stack: -> VAL
    define, // operand: symbol number stack: VAL ->
    cons, // operand: no stack: CAR CDR -> CONS
    car, // operand: no stack: CONS -> CAR
    cdr, // operand: no stack: CONS -> CDR
    // dup, // operand: no stack: VAL -> VAL VAL
    push_number, // operand: number stack: -> NUMBER
    push_true, //operand no stack: -> TRUE
    push_false, //operand no stack: -> FALSE
    push_nil, //operand no stack: -> NIL
    push_undef, //operand: no stack: -> UNDEF
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
};

pub const Evaluator = struct {
    pool: *object.ObjPool,
    globals: object.Obj,
    compiled_funcs: std.ArrayList(CompiledFunc),
    allocator: std.mem.Allocator,
    stack_frames: std.ArrayList(object.Obj),
    func_frames: std.ArrayList(FuncFrame),
    program_pointer: usize,
    current_func: usize,
    current_env: object.Obj,
};

const BuildinFunc = enum(i32) {
    plus,
    equal,
    nil_p,
    car,
    cdr,
    minus,
};

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
    try push_buildin_func(pool, g, "nil?", .nil_p);
    try push_buildin_func(pool, g, "car", .car);
    try push_buildin_func(pool, g, "cdr", .cdr);
    try push_buildin_func(pool, g, "-", .minus);
    evaluator.allocator = allocator;
    evaluator.compiled_funcs = std.ArrayList(CompiledFunc).init(allocator);
    evaluator.func_frames = std.ArrayList(FuncFrame).init(allocator);
    evaluator.stack_frames = std.ArrayList(object.Obj).init(allocator);
    return evaluator;
}

pub fn destroy_evaluator(evaluator: *Evaluator, allocator: std.mem.Allocator) void {
    object.destroy_obj_pool(evaluator.pool);
    std.ArrayList(CompiledFunc).deinit(evaluator.compiled_funcs);
    std.ArrayList(FuncFrame).deinit(evaluator.func_frames);
    std.ArrayList(object.Obj).deinit(evaluator.stack_frames);
    allocator.destroy(evaluator);
}

fn lookup_buildin_func(f: BuildinFunc) fn(*Evaluator, object.Obj, object.Obj)anyerror!object.Obj {
    return switch(f) {
        .plus => apply_plus,
        .equal => apply_equal,
        .nil_p => apply_nil_p,
        .car => apply_car,
        .cdr => apply_cdr,
        .minus => apply_minus,
    };
}

fn apply_plus(evaluator: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
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
    return object.create_number(evaluator.pool, result);
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


fn apply_nil_p(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
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

fn list_length(s: object.Obj) anyerror!usize {
    if (object.obj_type(s) == .nil) {
        return @intCast(usize, 0);
    }
    if (object.obj_type(s) != .cons) {
        return EvalError.IllegalParameter;
    }
    return (try list_length(object.get_cdr(s))) + 1;
}

fn eval_loop(e: *Evaluator) !object.Obj {
    while(true) {
        const current_opcode = e.compiled_funcs.items[e.current_func].codes[e.program_pointer];
        //std.debug.print("fun={} pp={} opcode={}\n", .{e.current_func, e.program_pointer, current_opcode});
        switch (current_opcode.tag) {
            .call => {
                const args = std.ArrayList(object.Obj).pop(&e.stack_frames);
                const func = std.ArrayList(object.Obj).pop(&e.stack_frames);
                switch (object.obj_type(func)) {
                    .buildin => {
                        const buildin_func = lookup_buildin_func(@intToEnum(BuildinFunc, object.get_buildin_value(func)));
                        const ret = try buildin_func(e, args, e.current_env);
                        try std.ArrayList(object.Obj).append(&e.stack_frames, ret);
                    },
                    else => return EvalError.IllegalApplication,
                }
            },
            .lookup => {
                try std.ArrayList(object.Obj).append(
                    &e.stack_frames,
                    object.lookup_frame(e.current_env, @intCast(usize, current_opcode.operand)) catch |err| if (err == object.LookUpError.NotFound) {
                        return EvalError.VariableNotFound;
                    } else {return err;}
                );
            },
            .define => {
                const val = std.ArrayList(object.Obj).pop(&e.stack_frames);
                const key = try object.create_symbol_by_id(e.pool, @intCast(usize, current_opcode.operand));
                try object.push_frame_var(e.pool, e.globals, key, val);
                try std.ArrayList(object.Obj).append(&e.stack_frames, try object.create_undef(e.pool));
            },
            .cons => {
                const cdr = std.ArrayList(object.Obj).pop(&e.stack_frames);
                const car = std.ArrayList(object.Obj).pop(&e.stack_frames);
                const cons = try object.create_cons(e.pool, car, cdr);
                try std.ArrayList(object.Obj).append(&e.stack_frames, cons);
            },
            .car => {
                const cons = std.ArrayList(object.Obj).pop(&e.stack_frames);
                const car = object.get_car(cons);
                try std.ArrayList(object.Obj).append(&e.stack_frames, car);
            },
            .cdr => {
                const cons = std.ArrayList(object.Obj).pop(&e.stack_frames);
                const cdr = object.get_cdr(cons);
                try std.ArrayList(object.Obj).append(&e.stack_frames, cdr);
            },
            .push_number => {
                try std.ArrayList(object.Obj).append(&e.stack_frames, try object.create_number(e.pool, current_opcode.operand));
            },
            .push_true => {
                try std.ArrayList(object.Obj).append(&e.stack_frames, try object.create_true(e.pool));
            },
            .push_false => {
                try std.ArrayList(object.Obj).append(&e.stack_frames, try object.create_false(e.pool));
            },
            .push_nil => {
                try std.ArrayList(object.Obj).append(&e.stack_frames, try object.create_nil(e.pool));
            },
            .push_undef => {
                try std.ArrayList(object.Obj).append(&e.stack_frames, try object.create_undef(e.pool));
            },
            .ret => {
                // TODO: handle when has stack frame
                const retval = std.ArrayList(object.Obj).pop(&e.stack_frames);
                return retval;
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
    try emit(e, car, codes, consts);
    try emit_create_list(e, cdr, codes, consts);
    try std.ArrayList(OpCode).append(codes, OpCode{.tag = .cons});
}

fn emit_cons(e: *Evaluator, s: object.Obj, codes: *std.ArrayList(OpCode), consts: *std.ArrayList(object.Obj)) anyerror!void {
    const car = object.get_car(s);
    const cdr = object.get_cdr(s);
    if (object.obj_type(car) == .symbol) {
        const symbol_val = try object.as_symbol(e.pool, car);
        if (std.mem.eql(u8, symbol_val, "quote")) {
            if ((try list_length(cdr)) != 1) {
                std.debug.print("malformed quote: expect 1 args but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }
            try std.ArrayList(object.Obj).append(consts, object.get_car(cdr));
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
            try emit(e, caddr, codes, consts);
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .define, .operand = @intCast(i32, object.get_symbol_id(key))});
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_undef});
            return;
        } else if (std.mem.eql(u8, symbol_val, "lambda")) {
            unreachable;
            // if ((try list_length(cdr)) < 2) {
            //     std.debug.print("lambda expect 2 args but got {}\n", .{try list_length(cdr)});
            //     return EvalError.IllegalParameter;
            // }
            // const args = object.get_car(cdr);
            // const body = object.get_car(object.get_cdr(cdr));
            // return object.create_func(e.pool, args, body, env);
        } else if (std.mem.eql(u8, symbol_val, "if")) {
            unreachable;
            // const length = try list_length(cdr);
            // if (length != 2 and length != 3) {
            //     std.debug.print("if expect 2 or 3 args but got {}\n", .{try list_length(cdr)});
            //     return EvalError.IllegalParameter;
            // }
            // const cadr = object.get_car(cdr);
            // const cond = try eval(e, cadr, env);
            // const cddr = object.get_cdr(cdr);
            // if (object.obj_type(cond) == .b_false) {
            //     if (length == 2) {
            //         return object.create_undef(e.pool);
            //     } else {
            //         return eval(e, object.get_car(object.get_cdr(cddr)), env);
            //     }
            // } else {
            //     return eval(e, object.get_car(cddr), env);
            // }
        } else if (std.mem.eql(u8, symbol_val, "let")) {
            unreachable;
            // const newframe = try object.create_frame(e.pool, try object.create_nil(e.pool), env);
            // const length = try list_length(cdr);
            // if (length != 2) {
            //     std.debug.print("let expect 2 but got {}\n", .{try list_length(cdr)});
            //     return EvalError.IllegalParameter;
            // }
            // const bindlist = object.get_car(cdr);
            // var current_binds = bindlist;
            // while (object.obj_type(current_binds) != .nil) {
            //     const bind_pair = object.get_car(current_binds);
            //     const bind_pair_length = try list_length(cdr);
            //     if (bind_pair_length != 2) {
            //         std.debug.print("illegal let form\n", .{});
            //         return EvalError.IllegalParameter;
            //     }
            //     const symbol = object.get_car(bind_pair);
            //     const body = object.get_car(object.get_cdr(bind_pair));
            //     const evaled_body = try eval(e, body, env);
            //     try object.push_frame_var(e.pool, newframe, symbol, evaled_body);
            //     current_binds = object.get_cdr(current_binds);
            // }
            // const body = object.get_car(object.get_cdr(cdr));
            // return eval(e, body, newframe);
        }
    }
    try emit(e, car, codes, consts);
    try emit_create_list(e, cdr, codes, consts);
    try std.ArrayList(OpCode).append(codes, OpCode{.tag = .call});
}

fn emit(e: *Evaluator, s: object.Obj, codes: *std.ArrayList(OpCode), consts: *std.ArrayList(object.Obj)) !void {
    return switch(object.obj_type(s)) {
        .b_true => std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_true}),
        .b_false => std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_false}),
        .number => std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_number, .operand = object.as_number(s)}),
        .nil => std.ArrayList(OpCode).append(codes, OpCode{.tag = .push_nil}),
        .buildin => EvalError.UnexpectedIntervalValue,
        .undef => EvalError.UnexpectedIntervalValue,
        .frame => EvalError.UnexpectedIntervalValue,
        .symbol => {
            try std.ArrayList(OpCode).append(codes, OpCode{.tag = .lookup, .operand = @intCast(i32, object.get_symbol_id(s))});
        },
        .func => EvalError.UnexpectedIntervalValue,
        .cons => {
            try emit_cons(e, s, codes, consts);
        },
    };
}

pub fn emit_func(e: *Evaluator, s: object.Obj) !usize {
    var codes = std.ArrayList(OpCode).init(e.allocator);
    defer std.ArrayList(OpCode).deinit(codes);
    var consts = std.ArrayList(object.Obj).init(e.allocator);
    defer std.ArrayList(object.Obj).deinit(consts);
    try emit(e, s, &codes, &consts);
    try std.ArrayList(OpCode).append(&codes, OpCode{.tag = .ret});
    const idx = e.compiled_funcs.items.len;
    try std.ArrayList(CompiledFunc).append(&e.compiled_funcs, CompiledFunc {
        .codes = std.ArrayList(OpCode).toOwnedSlice(&codes),
        .consts = std.ArrayList(object.Obj).toOwnedSlice(&consts),
    });
    return idx;
}

pub fn eval_compiled_global(e: *Evaluator, func_no: usize) !object.Obj {
    try std.ArrayList(object.Obj).resize(&e.stack_frames, 0);
    try std.ArrayList(FuncFrame).resize(&e.func_frames, 0);
    e.current_func = func_no;
    e.program_pointer = 0;
    e.current_env = e.globals;
    return eval_loop(e);
}

pub fn eval_global(e: *Evaluator, s: object.Obj) !object.Obj {
    const idx = try emit_func(e, s);
    return eval_compiled_global(e, idx);
}