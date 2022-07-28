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

const OpaqueSymbol = enum(i32) {
    plus,
    equal,
};

pub fn create_evaluator(allocator: std.mem.Allocator) !*Evaluator {
    const evaluator = try allocator.create(Evaluator);
    evaluator.pool = try object.create_obj_pool(allocator);
    evaluator.globals = try object.create_frame(evaluator.pool, try object.create_nil(evaluator.pool), try object.create_nil(evaluator.pool));
    return evaluator;
}

pub fn destroy_evaluator(evaluator: *Evaluator, allocator: std.mem.Allocator) void {
    object.destroy_obj_pool(evaluator.pool);
    allocator.destroy(evaluator);
}

fn apply_plus(evaluator: *Evaluator, s: object.Obj, env: object.Obj) anyerror!object.Obj {
    var result: i32 = 0;
    var current = s;
    while (object.obj_type(current) == .cons) {
        const car = object.get_car(current);
        const car_result = try eval(evaluator, car, env);
        if (object.obj_type(car_result) != .number) {
            return EvalError.IllegalParameter;
        }
        result += object.as_number(car_result);
        current = object.get_cdr(current);
    }
    if (object.obj_type(current) != .nil) {
        return EvalError.IllegalParameter;
    }
    return object.create_number(evaluator.pool, result);
}

fn apply_equal(e: *Evaluator, s: object.Obj, env: object.Obj) anyerror!object.Obj {
    if ((try list_length(s)) < 2) {
        std.debug.print("= expect 2 args but got {}\n", .{try list_length(s)});
        return EvalError.IllegalParameter;
    }
    const left = object.get_car(s);
    const evaled_left = try eval(e, left, env);
    const right = object.get_car(object.get_cdr(s));
    const evaled_right = try eval(e, right, env);
    if (object.obj_type(evaled_left) != .number or object.obj_type(evaled_right) != .number) {
        std.debug.print("must given number\n", .{});
        return EvalError.IllegalParameter;
    }
    if (object.as_number(evaled_left) == object.as_number(evaled_right)) {
        return object.create_true(e.pool);
    } else {
        return object.create_false(e.pool);
    }
}

fn apply_opaque(e: *Evaluator, o: OpaqueSymbol, s: object.Obj, env: object.Obj)  anyerror!object.Obj {
    return switch(o) {
        .plus => apply_plus(e, s, env),
        .equal => apply_equal(e, s, env),
    };
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
        if (std.mem.eql(u8, symbol_val, "define")) {
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
        .buildin => apply_opaque(e, @intToEnum(OpaqueSymbol, object.get_buildin_value(procedure)), cdr, env),
        else => EvalError.IllegalApplication,
    };
}

fn lookup_symbol(e: *Evaluator, s: object.Obj, env: object.Obj) !object.Obj {
    const symbol = try object.as_symbol(e.pool, s);
    if (std.mem.eql(u8, symbol, "+")) {
        return object.create_opaque(e.pool, @enumToInt(OpaqueSymbol.plus));
    } else if (std.mem.eql(u8, symbol, "=")) {
        return object.create_opaque(e.pool, @enumToInt(OpaqueSymbol.equal));
    } else {
        return object.lookup_frame(env, s) catch |err| if (err == object.LookUpError.NotFound) {
            return EvalError.VariableNotFound;
        } else {return err;};
    }
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
