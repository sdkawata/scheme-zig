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

fn apply_opaque(e: *Evaluator, o: OpaqueSymbol, s: object.Obj, env: object.Obj)  anyerror!object.Obj {
    return switch(o) {
        .plus => apply_plus(e, s, env),
    };
}

fn eval_list(e: *Evaluator, s:object.Obj, env: object.Obj) anyerror!object.Obj {
    const car = object.get_car(s);
    const cdr = object.get_cdr(s);
    if (object.obj_type(car) == .symbol) {
        const symbol_val = try object.as_symbol(e.pool, car);
        if (std.mem.eql(u8, symbol_val, "define")) {
            if (object.obj_type(cdr) != .cons) {
                return EvalError.IllegalParameter;
            }
            const key = object.get_car(cdr);
            if (object.obj_type(key) != .symbol) {
                return EvalError.IllegalParameter;
            }
            const cddr = object.get_cdr(cdr);
            if (object.obj_type(cddr) != .cons) {
                return EvalError.IllegalParameter;
            }
            const caddr = object.get_car(cddr);
            const body = try eval(e, caddr, env);
            try object.push_frame_var(e.pool, env, key, body);
            return object.create_nil(e.pool);
        }
    }
    const procedure = try eval(e, car, env);
    return switch(object.obj_type(procedure)) {
        .buildin => apply_opaque(e, @intToEnum(OpaqueSymbol, object.get_buildin_value(procedure)), cdr, env),
        else => EvalError.IllegalApplication,
    };
}

fn lookup_symbol(e: *Evaluator, s: object.Obj, env: object.Obj) !object.Obj {
    const symbol = try object.as_symbol(e.pool, s);
    if (std.mem.eql(u8, symbol, "+")) {
        return object.create_opaque(e.pool, @enumToInt(OpaqueSymbol.plus));
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
        .frame => EvalError.UnexpectedIntervalValue,
        .symbol => lookup_symbol(e, s, env),
        .cons => eval_list(e, s, env),
    };
}

pub fn eval_global(e: *Evaluator, s: object.Obj) !object.Obj {
    return eval(e, s, e.globals);
}
