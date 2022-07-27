const object = @import("object.zig");
const std = @import("std");


const EvalError  = error {
    VariableNotFound,
    IllegalApplication,
    IllegalParameter,
};

pub const Evaluator = struct {
    pool: *object.ObjPool,
};

const OpaqueSymbol = enum(i32) {
    plus,
};

pub fn create_evaluator(allocator: std.mem.Allocator) !*Evaluator {
    const evaluator = try allocator.create(Evaluator);
    evaluator.pool = try object.create_obj_pool(allocator);
    return evaluator;
}

pub fn destroy_evaluator(evaluator: *Evaluator, allocator: std.mem.Allocator) void {
    object.destroy_obj_pool(evaluator.pool, allocator);
    allocator.destroy(evaluator);
}

fn apply_plus(evaluator: *Evaluator, s: object.Obj) anyerror!object.Obj {
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

fn apply_opaque(e: *Evaluator, o: OpaqueSymbol, s: object.Obj)  anyerror!object.Obj {
    return switch(o) {
        .plus => apply_plus(e, s),
    };
}

fn eval_list(e: *Evaluator, s:object.Obj) anyerror!object.Obj {
    const car = object.get_car(s);
    const cdr = object.get_cdr(s);
    const procedure = try eval(e, car);
    return switch(object.obj_type(procedure)) {
        .b_opaque => apply_opaque(e, @intToEnum(OpaqueSymbol, object.get_opaque_value(procedure)), cdr),
        else => EvalError.IllegalApplication,
    };
}

fn lookup_symbol(e: *Evaluator, s: object.Obj) !object.Obj {
    // TODO get from env
    const symbol = try object.as_symbol_noalloc(s);
    if (std.mem.eql(u8, symbol, "+")) {
        return object.create_opaque(e.pool, @enumToInt(OpaqueSymbol.plus));
    }
    return EvalError.VariableNotFound;
}

pub fn eval(e: *Evaluator, s: object.Obj) !object.Obj {
    return switch(object.obj_type(s)) {
        .b_true => s,
        .b_false => s,
        .number => s,
        .nil => s,
        .b_opaque => s,
        .symbol => lookup_symbol(e, s),
        .cons => eval_list(e, s),
    };
}