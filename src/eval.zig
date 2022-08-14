const object = @import("object.zig");
const format = @import("format.zig");
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
pub const OpCodeTag = enum(u32) {
    ret, // operand: no stack: VAL ->
    call, // operand: no stack: FUNC, ARGS -> RET
    tailcall, // operand: no stack: FUNC, ARGS -> RET
    lookup, // operand: symbol number stack: -> VAL
    set_var, // operand symbol number stack: VAL ->
    cons, // operand: no stack: CAR CDR -> CONS
    car, // operand: no stack: CONS -> CAR
    cdr, // operand: no stack: CONS -> CDR
    dup_car, // operand: no stack: CONS -> CONS CAR
    dup_cdr, // operand: no stack: CONS -> CONS CDR
    push_number, // operand: number stack: -> NUMBER
    push_float, // operand: number stack: -> FLOAT
    push_char, //operand: number stack: -> CHAR
    push_true, //operand no stack: -> TRUE
    push_false, //operand no stack: -> FALSE
    push_nil, //operand no stack: -> NIL
    push_undef, //operand: no stack: -> UNDEF
    push_const, //operand: constno stack: -> VAL
    new_frame, //operand:no stack: -> FRAME
    push_new_var, // operand: symbol number stack: FRAME VAL -> FRAME
    push_new_var_current, // operand: symbol number stack: VAL ->
    set_frame, // operand no stack: FRAME ->
    set_frame_previous, // operand: no stack: none
    closure, // operand: compiled_func_id stack: -> CLOSURE
    discard, // operand: no stack: VAl -> 
    jmp, // operand: addr stack: none
    jmp_if_false, // operand: addr stack: VAL ->
    jmp_if_true_preserve_true, // operand: addr stack: if (VAL) { VAL -> VAL } else {VAL -> }
};

pub const OpCode = packed struct {
    tag: OpCodeTag,
    operand: i32 = 0,
};

pub const CompiledFunc = struct {
    codes: [] OpCode,
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
    func_frames: std.ArrayList(FuncFrame),
    program_pointer: usize,
    current_func: usize,
    displayer: fn(*Evaluator, object.Obj)anyerror!void,
};

const BuildinFunc = enum(i32) {
    plus,
    multiply,
    equal,
    null_p,
    car,
    cdr,
    minus,
    cons,
    display,
};

const MAX_RECURSIVE_CALL = 100;

fn push_buildin_func(pool: *object.ObjPool, env: *object.Obj, name: [] const u8, f: BuildinFunc) !void {
    var symbol = try object.create_symbol(pool, name);
    try object.push_frame_var(pool, env, &symbol, & try object.create_opaque(pool, @enumToInt(f)));
}

pub fn create_evaluator(allocator: std.mem.Allocator) !*Evaluator {
    const evaluator = try allocator.create(Evaluator);
    const pool = try object.create_obj_pool(allocator);
    evaluator.pool = pool;
    var nil = try object.create_nil(evaluator.pool);
    var g = try object.create_frame(evaluator.pool, &nil, &nil);
    try std.ArrayList(object.Obj).append(&evaluator.pool.consts, g);
    try push_buildin_func(pool, &g, "+", .plus);
    try push_buildin_func(pool, &g, "*", .multiply);
    try push_buildin_func(pool, &g, "=", .equal);
    try push_buildin_func(pool, &g, "null?", .null_p);
    try push_buildin_func(pool, &g, "car", .car);
    try push_buildin_func(pool, &g, "cdr", .cdr);
    try push_buildin_func(pool, &g, "-", .minus);
    try push_buildin_func(pool, &g, "cons", .cons);
    try push_buildin_func(pool, &g, "display", .display);
    evaluator.allocator = allocator;
    evaluator.compiled_funcs = std.ArrayList(CompiledFunc).init(allocator);
    evaluator.func_frames = std.ArrayList(FuncFrame).init(allocator);
    evaluator.displayer = debug_displayer;
    return evaluator;
}

pub fn destroy_evaluator(e: *Evaluator) void {
    object.destroy_obj_pool(e.pool);
    for (e.compiled_funcs.items) |func| {
        e.allocator.free(func.codes);
    }
    std.ArrayList(CompiledFunc).deinit(e.compiled_funcs);
    std.ArrayList(FuncFrame).deinit(e.func_frames);
    const allocator = e.allocator;
    allocator.destroy(e);
}

fn globals(e: *Evaluator) object.Obj {
    return e.pool.consts.items[0];
}

fn lookup_buildin_func(f: BuildinFunc) fn(*Evaluator, object.Obj, object.Obj)anyerror!object.Obj {
    return switch(f) {
        .plus => apply_plus,
        .multiply => apply_multiply,
        .equal => apply_equal,
        .null_p => apply_null_p,
        .car => apply_car,
        .cdr => apply_cdr,
        .minus => apply_minus,
        .cons => apply_cons,
        .display => apply_display,
    };
}

fn debug_displayer(e:*Evaluator, obj: object.Obj) anyerror!void {
    const formatted = try format.display(e.pool, obj, e.allocator);
    defer e.allocator.free(formatted);
    std.debug.print("{s}", .{formatted});
}

fn apply_display(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 1) {
        std.debug.print("display expect 1 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    try e.displayer(e, list_1st(s));
    return object.create_undef(e.pool);
}

fn apply_plus(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    var result_i32: i32 = 0;
    var result_f32: f32 = 0.0;
    var current_float = false;
    var current = s;
    while (object.obj_type(&current) == .cons) {
        const car = object.get_car(&current);
        
        if (! is_numeric_type(object.obj_type(&car))) {
            return EvalError.IllegalParameter;
        }
        if (object.obj_type(&car) == .float and ! current_float) {
            current_float = true;
            result_f32 = @intToFloat(f32, result_i32);
        }
        if (current_float) {
            result_f32 += cast_to_float(e, car);
        } else {
            result_i32 += object.as_number(&car);
        }
        current = object.get_cdr(&current);
    }
    if (object.obj_type(&current) != .nil) {
        return EvalError.IllegalParameter;
    }
    if (current_float) {
        return object.create_float(e.pool, result_f32);
    } else {
        return object.create_number(e.pool, result_i32);
    }
}

fn apply_multiply(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    var result_i32: i32 = 1;
    var result_f32: f32 = 1.0;
    var current_float = false;
    var current = s;
    while (object.obj_type(&current) == .cons) {
        const car = object.get_car(&current);
        if (! is_numeric_type(object.obj_type(&car))) {
            return EvalError.IllegalParameter;
        }
        if (object.obj_type(&car) == .float and ! current_float) {
            current_float = true;
            result_f32 = @intToFloat(f32, result_i32);
        }
        if (current_float) {
            result_f32 *= cast_to_float(e, car);
        } else {
            result_i32 *= object.as_number(&car);
        }
        current = object.get_cdr(&current);
    }
    if (object.obj_type(&current) != .nil) {
        return EvalError.IllegalParameter;
    }
    if (current_float) {
        return object.create_float(e.pool, result_f32);
    } else {
        return object.create_number(e.pool, result_i32);
    }
}

fn is_numeric_type(t: object.ObjType) bool {
    return t == .number or t == .float;
}

fn cast_to_float(_: *Evaluator, s: object.Obj) f32 {
    if (object.obj_type(&s) == .number) {
        return @intToFloat(f32, object.as_number(&s));
    } else if (object.obj_type(&s) == .float) {
        return object.as_float(&s);
    } else {
        unreachable;
    }
}

fn numeric_compare(e: *Evaluator, s1: object.Obj, s2:object.Obj) anyerror!i32 {
    const t1 = object.obj_type(&s1);
    const t2 = object.obj_type(&s2);
    if (t1 == .number and t2 == .number) {
        const n1 = object.as_number(&s1);
        const n2 = object.as_number(&s2);
        if (n1 == n2) {
            return 0;
        } else if (n1 < n2) {
            return -1;
        } else {
            return 1;
        }
    } else if (is_numeric_type(t1) and is_numeric_type(t2)) {
        const f1 = cast_to_float(e, s1);
        const f2 = cast_to_float(e, s2);
        if (f1 == f2) {
            return 0;
        } else if (f1 < f2) {
            return -1;
        } else {
            return 1;
        }
    }
    return EvalError.IllegalParameter;
}

fn apply_equal(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 2) {
        std.debug.print("= expect 2 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    const left = list_1st(s);
    const right = list_2nd(s);
    if ((try numeric_compare(e, left, right)) == 0) {
        return object.create_true(e.pool);
    } else {
        return object.create_false(e.pool);
    }
}

fn apply_minus(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    const length = try list_length(s);
    if (length == 0) {
        return EvalError.IllegalParameter;
    } else if (length == 1) {
        const first = object.get_car(&s);
        if (object.obj_type(&first) == .number) {
            return try object.create_number(e.pool, - object.as_number(&first));
        } else if (object.obj_type(&first) == .float) {
            return try object.create_float(e.pool, - object.as_float(&first));
        } else {
            return EvalError.IllegalParameter;
        }
    }
    var result_i32: i32 = 0;
    var result_f32: f32 = 0.0;
    var current_float = false;
    var current = object.get_cdr(&s);
    const first = object.get_car(&s);
    if (object.obj_type(&first) == .number) {
        result_i32 = object.as_number(&first);
    } else if (object.obj_type(&first) == .float) {
        current_float = true;
        result_f32 = object.as_float(&first);
    } else {
        return EvalError.IllegalParameter;
    }
    while (object.obj_type(&current) == .cons) {
        const car = object.get_car(&current);
        if (! is_numeric_type(object.obj_type(&car))) {
            return EvalError.IllegalParameter;
        }
        if (object.obj_type(&car) == .float and ! current_float) {
            current_float = true;
            result_f32 = @intToFloat(f32, result_i32);
        }
        if (current_float) {
            result_f32 -= cast_to_float(e, car);
        } else {
            result_i32 -= object.as_number(&car);
        }
        current = object.get_cdr(&current);
    }
    if (object.obj_type(&current) != .nil) {
        return EvalError.IllegalParameter;
    }
    if (current_float) {
        return object.create_float(e.pool, result_f32);
    } else {
        return object.create_number(e.pool, result_i32);
    }
}


fn apply_null_p(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 1) {
        std.debug.print("nil? expect 1 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    if (object.obj_type(&list_1st(s)) == .nil) {
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
    const car = object.get_car(&s);
    if (object.obj_type(&car) != .cons) {
        std.debug.print("car got non-list\n", .{});
        return EvalError.IllegalParameter;
    }
    return object.get_car(&car);
}

fn apply_cdr(_: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 1) {
        std.debug.print("cdr expect 1 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    const car = object.get_car(&s);
    if (object.obj_type(&car) != .cons) {
        std.debug.print("car got non-list\n", .{});
        return EvalError.IllegalParameter;
    }
    return object.get_cdr(&car);
}

fn apply_cons(e: *Evaluator, s: object.Obj, _: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) != 2) {
        std.debug.print("cons expect 2 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    var car = list_1st(s);
    var cadr = list_2nd(s);
    return object.create_cons(e.pool, &car, &cadr);
}

pub fn list_length(s: object.Obj) anyerror!usize {
    if (object.obj_type(&s) == .nil) {
        return @intCast(usize, 0);
    }
    if (object.obj_type(&s) != .cons) {
        return EvalError.IllegalParameter;
    }
    return (try list_length(object.get_cdr(&s))) + 1;
}

pub fn list_1st(s: object.Obj) object.Obj {
    return object.get_car(&s);
}

pub fn list_2nd(s: object.Obj) object.Obj {
    return object.get_car(&object.get_cdr(&s));
}

pub fn list_3rd(s: object.Obj) object.Obj {
    return object.get_car(&object.get_cdr(&object.get_cdr(&s)));
}

fn eval_apply_buildin(e:*Evaluator, func: object.Obj, args: object.Obj) !void {
    const buildin_func = lookup_buildin_func(@intToEnum(BuildinFunc, object.get_buildin_value(&func)));
    const ret = try buildin_func(e, args, e.pool.current_env);
    try push_stack(e, ret);
}

fn ret_to_previous_func(e: *Evaluator, retval: object.Obj) !void {
    const prev_func_frame = std.ArrayList(FuncFrame).pop(&e.func_frames);
    e.program_pointer = prev_func_frame.ret_addr;
    e.current_func = prev_func_frame.ret_func;
    try std.ArrayList(object.Obj).resize(&e.pool.stack_frames, prev_func_frame.base_stack_pointer);
    e.pool.current_env = pop_stack(e);
    try push_stack(e, retval);
}

fn jmp_to_func(e: *Evaluator, func: *object.Obj, args: *object.Obj) !void {
    var func_env = object.get_func_env(func);
    e.pool.current_env = try object.create_frame(e.pool, &try object.create_nil(e.pool), &func_env);
    e.current_func = @intCast(usize, object.get_func_id(func));
    e.program_pointer = 0;
    try push_stack(e, args.*);
}

fn pop_stack(e: *Evaluator) object.Obj {
    return std.ArrayList(object.Obj).pop(&e.pool.stack_frames);
}

fn peek_stack_top(e: *Evaluator) *object.Obj {
    return &e.pool.stack_frames.items[e.pool.stack_frames.items.len - 1];
}
fn peek_stack_2nd(e: *Evaluator) *object.Obj {
    return &e.pool.stack_frames.items[e.pool.stack_frames.items.len - 2];
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
                var args = pop_stack(e);
                var func = pop_stack(e);
                try object.push_root(e.pool, &args);
                try object.push_root(e.pool, &func);
                defer object.pop_root(e.pool);
                defer object.pop_root(e.pool);
                switch (object.obj_type(&func)) {
                    .buildin => try eval_apply_buildin(e, func, args),
                    .func => {
                        try push_stack(e, e.pool.current_env);
                        try std.ArrayList(FuncFrame).append(&e.func_frames, FuncFrame {
                            .ret_func = e.current_func,
                            .ret_addr = e.program_pointer,
                            .base_stack_pointer = e.pool.stack_frames.items.len,
                        });
                        try jmp_to_func(e, &func, &args);
                        continue;
                    },
                    else => return EvalError.IllegalApplication,
                }
            },
            .tailcall => {
                var args = pop_stack(e);
                var func = pop_stack(e);
                try object.push_root(e.pool, &args);
                try object.push_root(e.pool, &func);
                defer object.pop_root(e.pool);
                defer object.pop_root(e.pool);
                switch (object.obj_type(&func)) {
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
                        try jmp_to_func(e, &func, &args);
                        continue;
                    },
                    else => return EvalError.IllegalApplication,
                }
            },
            .lookup => {
                const idx = @intCast(usize, current_opcode.operand);
                const val = object.lookup_frame(&e.pool.current_env, idx) catch |err| if (err == object.LookUpError.NotFound) {
                   std.debug.print("variable not found {s}\n", .{e.pool.symbol_table.items[idx]});
                   return EvalError.VariableNotFound;
                } else {return err;};
                try std.ArrayList(object.Obj).append(
                    &e.pool.stack_frames,
                    val
                );
            },
            .cons => {
                var cdr = pop_stack(e);
                var car = pop_stack(e);
                const cons = try object.create_cons(e.pool, &car, &cdr);
                try push_stack(e, cons);
            },
            .car => {
                const cons = pop_stack(e);
                const car = object.get_car(&cons);
                try push_stack(e, car);
            },
            .cdr => {
                const cons = pop_stack(e);
                const cdr = object.get_cdr(&cons);
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
            .push_float => {
                try push_stack(e, try object.create_float(e.pool, @bitCast(f32, current_opcode.operand)));
            },
            .push_char => {
                try push_stack(e, try object.create_char(e.pool, @intCast(u8, current_opcode.operand)));
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
                try push_stack(e, e.pool.consts.items[@intCast(usize, current_opcode.operand)]);
            },
            .new_frame => {
                const new_frame = try object.create_frame(e.pool, &try object.create_nil(e.pool), &e.pool.current_env);
                try push_stack(e, new_frame);
            },
            .push_new_var => {
                var val = pop_stack(e);
                var frame = peek_stack_top(e);
                var key = try object.create_symbol_by_id(e.pool, @intCast(usize, current_opcode.operand));
                try object.push_frame_var(e.pool, frame, &key, &val);
            },
            .push_new_var_current => {
                var val = pop_stack(e);
                var key = try object.create_symbol_by_id(e.pool, @intCast(usize, current_opcode.operand));
                try object.push_frame_var(e.pool, &e.pool.current_env, &key, &val);
            },
            .set_var => {
                var val = pop_stack(e);
                var symbol_id = @intCast(usize, current_opcode.operand);
                try object.set_frame_var(&e.pool.current_env, symbol_id, &val);
            },
            .set_frame => {
                const frame = pop_stack(e);
                std.debug.assert(object.obj_type(&frame) == .frame);
                e.pool.current_env = frame;
            },
            .set_frame_previous => {
                const prev_frame = object.get_frame_previous(&e.pool.current_env);
                e.pool.current_env = prev_frame;
            },
            .closure => {
                const function_id = @intCast(usize, current_opcode.operand);
                const func = try object.create_func(e.pool, function_id, &e.pool.current_env);
                try push_stack(e, func);
            },
            .ret => {
                const retval = pop_stack(e);
                if (e.func_frames.items.len == 0) {
                    return retval;
                }
                try ret_to_previous_func(e, retval);
            },
            .discard => {
                _ = pop_stack(e);
            },
            .jmp => {
                const ptr = @intCast(usize, current_opcode.operand);
                e.program_pointer = ptr;
                continue;
            },
            .jmp_if_false => {
                const ptr = @intCast(usize, current_opcode.operand);
                const val = pop_stack(e);
                if (object.obj_type(&val) == .b_false) {
                    e.program_pointer = ptr;
                    continue;
                }
            },
            .jmp_if_true_preserve_true => {
                const ptr = @intCast(usize, current_opcode.operand);
                const val = pop_stack(e);
                if (object.obj_type(&val) != .b_false) {
                    try push_stack(e, val);
                    e.program_pointer = ptr;
                    continue;
                }
            }
        }
        e.program_pointer+=1;
    }
}


pub fn debug_print_func(e: *Evaluator, func_id: usize) void {
    std.debug.print("==function {}==\n", .{func_id});
    const func = e.compiled_funcs.items[func_id];
    for (func.codes) |code, i| {
        std.debug.print("p={} {}\n", .{i, code});
    }
    std.debug.print("==end function {}==\n", .{func_id});
}

pub fn debug_print_symbols(e: *Evaluator) void {
    std.debug.print("==symbol table==\n", .{});
    for (e.pool.symbol_table.items) |symbol, i| {
        std.debug.print("i={} {s}\n", .{i, symbol});
    }
    std.debug.print("==end symbol table==\n", .{});
}

pub fn eval_compiled_global(e: *Evaluator, func_no: usize) !object.Obj {
    try std.ArrayList(object.Obj).resize(&e.pool.stack_frames, 0);
    try std.ArrayList(FuncFrame).resize(&e.func_frames, 0);
    e.current_func = func_no;
    e.program_pointer = 0;
    e.pool.current_env = globals(e);
    return eval_loop(e);
}
