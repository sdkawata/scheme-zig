const object = @import("object.zig");
const std = @import("std");


const EvalError  = error {
    VariableNotFound,
    IllegalApplication,
    IllegalParameter,
    UnexpectedIntervalValue,
};

pub const Evaluator = struct {
    pool: *object.ObjPool,
    globals: object.Obj,
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
    return evaluator;
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

pub fn destroy_evaluator(evaluator: *Evaluator, allocator: std.mem.Allocator) void {
    object.destroy_obj_pool(evaluator.pool);
    allocator.destroy(evaluator);
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

fn eval_every_in_list(e: *Evaluator, s: object.Obj, env: object.Obj) anyerror!object.Obj {
    if (object.obj_type(s) == .nil) {
        return s;
    }
    if (object.obj_type(s) != .cons) {
        std.debug.print("not list\n", .{});
        return EvalError.IllegalParameter;
    }
    const evaled_car = try eval(e, object.get_car(s), env);
    const evaled_cdr = try eval_every_in_list(e, object.get_cdr(s), env);
    return object.create_cons(e.pool, evaled_car, evaled_cdr);
}

fn apply_buildin(e: *Evaluator, f: BuildinFunc, s: object.Obj, env: object.Obj)  anyerror!object.Obj {
    const func = lookup_buildin_func(f);
    const evaled_arg = try eval_every_in_list(e, s, env);
    return func(e, evaled_arg, env);
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

fn apply_func(e: *Evaluator, func: object.Obj, params: object.Obj, env: object.Obj) anyerror!object.Obj {
    const newframe = try object.create_frame(e.pool, try object.create_nil(e.pool), object.get_func_env(func));
    const args = object.get_func_args(func);
    var rest_params = params;
    var rest_args = args;
    while(true) {
        if (object.obj_type(rest_args) == .nil and object.obj_type(rest_params) == .nil) {
            break;
        } else if (object.obj_type(rest_args) == .nil or object.obj_type(rest_params) == .nil) {
            return EvalError.IllegalApplication;
        }
        if (object.obj_type(rest_params) != .cons) {
            return EvalError.IllegalApplication;
        }
        const evaled_param = try eval(e, object.get_car(rest_params), env);
        if (object.obj_type(rest_args) != .cons) {
            return EvalError.IllegalApplication;
        }
        const arg = object.get_car(rest_args);
        if (object.obj_type(arg) != .symbol) {
            return EvalError.IllegalApplication;
        }
        try object.push_frame_var(e.pool, newframe, arg, evaled_param);
        rest_args = object.get_cdr(rest_args);
        rest_params = object.get_cdr(rest_params);
    }
    return eval(e, object.get_func_body(func), newframe);
}

fn eval_list(e: *Evaluator, s:object.Obj, env: object.Obj) anyerror!object.Obj {
    const car = object.get_car(s);
    const cdr = object.get_cdr(s);
    if (object.obj_type(car) == .symbol) {
        const symbol_val = try object.as_symbol(e.pool, car);
        if (std.mem.eql(u8, symbol_val, "quote")) {
            if ((try list_length(cdr)) != 1) {
                std.debug.print("quote expect 1 args but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }
            return object.get_car(cdr);
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
            const body = try eval(e, caddr, env);
            try object.push_frame_var(e.pool, env, key, body);
            return object.create_nil(e.pool);
        } else if (std.mem.eql(u8, symbol_val, "lambda")) {
            if ((try list_length(cdr)) < 2) {
                std.debug.print("lambda expect 2 args but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }
            const args = object.get_car(cdr);
            const body = object.get_car(object.get_cdr(cdr));
            return object.create_func(e.pool, args, body, env);
        } else if (std.mem.eql(u8, symbol_val, "if")) {
            const length = try list_length(cdr);
            if (length != 2 and length != 3) {
                std.debug.print("if expect 2 or 3 args but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }
            const cadr = object.get_car(cdr);
            const cond = try eval(e, cadr, env);
            const cddr = object.get_cdr(cdr);
            if (object.obj_type(cond) == .b_false) {
                if (length == 2) {
                    return object.create_undef(e.pool);
                } else {
                    return eval(e, object.get_car(object.get_cdr(cddr)), env);
                }
            } else {
                return eval(e, object.get_car(cddr), env);
            }
        } else if (std.mem.eql(u8, symbol_val, "let")) {
            const newframe = try object.create_frame(e.pool, try object.create_nil(e.pool), env);
            const length = try list_length(cdr);
            if (length != 2) {
                std.debug.print("let expect 2 but got {}\n", .{try list_length(cdr)});
                return EvalError.IllegalParameter;
            }
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
                const evaled_body = try eval(e, body, env);
                try object.push_frame_var(e.pool, newframe, symbol, evaled_body);
                current_binds = object.get_cdr(current_binds);
            }
            const body = object.get_car(object.get_cdr(cdr));
            return eval(e, body, newframe);
        }
    }
    const procedure = try eval(e, car, env);
    return switch(object.obj_type(procedure)) {
        .func => apply_func(e, procedure, cdr, env),
        .buildin => apply_buildin(e, @intToEnum(BuildinFunc, object.get_buildin_value(procedure)), cdr, env),
        else => EvalError.IllegalApplication,
    };
}

fn lookup_symbol(_: *Evaluator, s: object.Obj, env: object.Obj) !object.Obj {
    return object.lookup_frame(env, s) catch |err| if (err == object.LookUpError.NotFound) {
        return EvalError.VariableNotFound;
    } else {return err;};
}

fn eval(e: *Evaluator, s: object.Obj, env: object.Obj) !object.Obj {
    return switch(object.obj_type(s)) {
        .b_true => s,
        .b_false => s,
        .number => s,
        .nil => s,
        .buildin => s,
        .func => s,
        .undef => s,
        .frame => EvalError.UnexpectedIntervalValue,
        .symbol => lookup_symbol(e, s, env),
        .cons => eval_list(e, s, env),
    };
}

pub fn eval_global(e: *Evaluator, s: object.Obj) !object.Obj {
    return eval(e, s, e.globals);
}
