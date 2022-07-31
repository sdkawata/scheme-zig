const std = @import("std");
const assert = std.debug.assert;

pub const Obj = u64;

// value is pointer or value
// value layout
// [<- value(32bit)->][<- type(16bit) ->][<- reserved ->][<- value bit(1 for value)(1bit) ->]
// note: pointer is always aligned by 8
// type ture value:none
// type false value:none
// type nil value: none
// type number(32bit) value: number
// type buildin value: number which meaning is given by executor
// type symbol value: symbol id
// memory layout
// |<- obj value(32bit) -> |<- obj tag(32 bit) -> |<- additional (any byte) >|
// TYPE cons cell value:none additional:CAR pointer(8byte) + CDR pointer(8byte)
// TYPE env frame value:none additional:vars pointer(8byte, list of pair of key and value) + previous frame (8btye)
// TYPE env frame value:none additional:func id(8byte) + env (8btye)


const ObjHeader = packed struct {
    i: u64,
};

const ObjConsCell = packed struct {
    header: ObjHeader,
    car: Obj,
    cdr: Obj,
};

const ObjFrame = packed struct {
    header: ObjHeader,
    vars: Obj,
    previous: Obj,
};

const ObjFunc = packed struct {
    header: ObjHeader,
    func_id: usize,
    env: Obj,
};


const ObjForwarded = packed struct {
    header: ObjHeader,
    forwarding_addr: Obj,
};

const ObjValueType = enum(u16) {
    b_true,
    b_false,
    number_i32,
    nil,
    buildin,
    symbol,
    undef,
};

const ObjRefType = enum(u32) {
    cons,
    frame,
    func,
    forwarded,
};

pub const ObjType =  enum(u32) {
    b_true,
    b_false,
    undef,
    number,
    cons,
    nil,
    symbol,
    buildin,
    frame,
    func,
};

const INITIAL_BUF_SIZE = 1000000;

pub const ObjPool = struct {
    buf: [] u8,
    current: [*] u8,
    from_space: [*] u8,
    to_space: [*] u8,
    space_size: usize,
    symbol_table: std.ArrayList([] const u8),
    allocator: std.mem.Allocator,
    stack_frames: std.ArrayList(Obj),
    consts: std.ArrayList(Obj),
    root: std.ArrayList(*Obj),
    gc_enabled: bool,
    gc_every_time: bool,
    current_env: Obj,
};

fn is_value (obj:* const Obj) bool {
    return obj.* & 0x1 == 1;
}
fn as_obj_header(obj:* const Obj) *ObjHeader {
    return @intToPtr(*ObjHeader, @intCast(usize, obj.*));
}
fn as_obj(header: *ObjHeader) Obj {
    return @intCast(u64, @ptrToInt(header));
}

fn obj_ref_type_from_header(header: *ObjHeader) ObjRefType {
    return @intToEnum(ObjRefType, @intCast(u32, header.i & 0xffff));
}

fn obj_ref_type(obj:* const Obj) ObjRefType {
    assert(!is_value(obj));
    return obj_ref_type_from_header(as_obj_header(obj));
}

fn obj_value_type(obj: * const Obj) ObjValueType {
    assert(is_value(obj));
    return @intToEnum(ObjValueType, @intCast(u16, obj.* >> 16 & 0xff));
}

pub fn obj_type (obj: * const Obj) ObjType {
    if (is_value(obj)) {
        return switch(obj_value_type(obj)) {
            .b_true => .b_true,
            .b_false => .b_false,
            .undef => .undef,
            .nil => .nil,
            .number_i32 => .number,
            .buildin => .buildin,
            .symbol => .symbol,
        };
    } else {
        return switch(obj_ref_type(obj)) {
            .cons => .cons,
            .frame => .frame,
            .func => .func,
            .forwarded => unreachable,
        };
    }
}

fn obj_value(obj: * const Obj) i32 {
    assert(is_value(obj));
    return @intCast(i32, obj.* >> 32);
}

fn obj_ref_value(header: *ObjHeader) i32 {
    return @intCast(i32, header.i >> 32);
} 

pub fn as_number(obj: * const Obj) i32 {
    assert(is_value(obj) and obj_value_type(obj) == .number_i32);
    return obj_value(obj);
}
pub fn get_buildin_value(obj: * const Obj) i32 {
    assert(is_value(obj) and obj_value_type(obj) == .buildin);
    return obj_value(obj);
}

pub fn get_symbol_id(obj: * const Obj) usize {
    assert(is_value(obj) and obj_value_type(obj) == .symbol);
    return @intCast(usize, obj_value(obj));
}

pub fn as_symbol(pool: *ObjPool, obj: * const Obj) ![] const u8 {
    return pool.symbol_table.items[get_symbol_id(obj)];
}

pub fn get_car(obj: * const Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .cons);
    const header = as_obj_header(obj);
    const consCell = @ptrCast(*ObjConsCell, header);
    return consCell.car;
}
 
pub fn get_cdr(obj: * const Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .cons);
    const header = as_obj_header(obj);
    const consCell = @ptrCast(*ObjConsCell, header);
    return consCell.cdr;
}

pub fn get_func_id(obj: * const Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .func);
    const header = as_obj_header(obj);
    const func = @ptrCast(*ObjFunc, header);
    return func.func_id;
}

pub fn get_func_env(obj: * const Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .func);
    const header = as_obj_header(obj);
    const func = @ptrCast(*ObjFunc, header);
    return func.env;
}

pub fn get_frame_vars(obj: * const Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .frame);
    const header = as_obj_header(obj);
    const frame = @ptrCast(*ObjFrame, header);
    return frame.vars;
}

pub fn get_frame_previous(obj: * const Obj) Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .frame);
    const header = as_obj_header(obj);
    const frame = @ptrCast(*ObjFrame, header);
    return frame.previous;
}

pub fn push_frame_var(pool: *ObjPool, obj: *Obj, key: *Obj, value: *Obj) !void {
    assert(!is_value(obj) and obj_ref_type(obj) == .frame);
    var pair = try create_cons(pool, key, value);
    const new_vars = try create_cons(pool, &pair, &try create_nil(pool));
    // replace cdr here to prevent vars changed during create_cons
    const frame = @ptrCast(*ObjFrame, as_obj_header(obj));
    @ptrCast(*ObjConsCell, as_obj_header(&new_vars)).cdr = frame.vars;
    frame.vars = new_vars;
}

pub const LookUpError = error {
    NotFound,
};

pub fn lookup_frame(obj: * const Obj, symbol_id: usize) !Obj {
    assert(!is_value(obj) and obj_ref_type(obj) == .frame);
    var currentFrame = obj.*;
    while (obj_type(&currentFrame) != .nil) {
        var currentVars = get_frame_vars(&currentFrame);
        while (obj_type(&currentVars) != .nil) {
            const varPair = get_car(&currentVars);
            if (symbol_id == get_symbol_id(&get_car(&varPair))) {
                return get_cdr(&varPair);
            }
            currentVars = get_cdr(&currentVars);
        }
        currentFrame = get_frame_previous(&currentFrame);
    }
    return LookUpError.NotFound;
}

pub fn create_obj_pool(allocator: std.mem.Allocator) !*ObjPool {
    const pool = try allocator.create(ObjPool);
    pool.buf = try allocator.alloc(u8, INITIAL_BUF_SIZE);
    pool.space_size = INITIAL_BUF_SIZE / 2;
    pool.from_space = @ptrCast([*] u8, &pool.buf[0]);
    pool.current = pool.from_space;
    pool.to_space = pool.from_space + pool.space_size;
    pool.symbol_table = std.ArrayList([] const u8).init(allocator);
    pool.stack_frames = std.ArrayList(Obj).init(allocator);
    pool.consts = std.ArrayList(Obj).init(allocator);
    pool.root = std.ArrayList(*Obj).init(allocator);
    pool.gc_enabled = true;
    pool.gc_every_time = false;
    pool.allocator = allocator;
    return pool;
}

pub fn destroy_obj_pool(pool: *ObjPool) void {
    pool.allocator.free(pool.buf);
    for (pool.symbol_table.items) |sym| {
        pool.allocator.free(sym);
    }
    std.ArrayList([] const u8).deinit(pool.symbol_table);
    std.ArrayList(Obj).deinit(pool.stack_frames);
    std.ArrayList(Obj).deinit(pool.consts);
    std.ArrayList(*Obj).deinit(pool.root);
    pool.allocator.destroy(pool);
}

fn align_size(size: usize) usize {
    return (size + (8-1)) / 8 * 8;
}

const CreateError = error {
    NoMemoryError,
};

fn create(pool: *ObjPool, comptime T: type) !*T {
    const size = align_size(@sizeOf(T));
    return @ptrCast(*T, @alignCast(@alignOf(T), try alloc(pool, size)));
}

fn space_left(pool: *ObjPool, size: usize) bool {
    return @ptrToInt(pool.current) + size - @ptrToInt(pool.from_space) >=  pool.space_size;
}

fn alloc(pool: *ObjPool, size: usize) ![*]u8 {
    // std.debug.print("alloc from:{*} to:{*} current:{*}\n", .{pool.from_space, pool.to_space, pool.current});
    if ((space_left(pool, size) or pool.gc_every_time) and pool.gc_enabled) {
        try start_gc(pool);
    }
    if (space_left(pool, size)) {
        return CreateError.NoMemoryError;
    }
    const ptr = @ptrCast([*]u8, pool.current);
    pool.current+= align_size(size);
    return ptr;
}

fn obj_size(obj:Obj) Obj {
    return switch(obj_ref_type(&obj)) {
        .cons => @sizeOf(ObjConsCell),
        .frame => @sizeOf(ObjFrame),
        .func => @sizeOf(ObjFunc),
        .forwarded => unreachable,
    };
}

fn copy_obj(pool: *ObjPool, obj:Obj) ! Obj {
    if (is_value(&obj)) {
        return obj;
    }
    if (@ptrToInt(pool.from_space) <= obj and obj < (@ptrToInt(pool.from_space) + pool.space_size)) {
        // already copied
        return obj;
    }
    if (obj_ref_type(&obj) == .forwarded) {
        return @ptrCast(*ObjForwarded, as_obj_header(&obj)).forwarding_addr;
    }
    assert(@ptrToInt(pool.to_space) <= obj and obj < (@ptrToInt(pool.to_space) + pool.space_size));
    const size = obj_size(obj);
    // std.debug.print("type:{} size:{} copy {*} to {*}\n", .{obj_ref_type(&obj), size, @intToPtr([*] u8, obj), pool.current});
    @memcpy(pool.current, @intToPtr([*] u8, obj), size);
    try init_header(as_obj_header(&obj), ObjRefType.forwarded, 0);
    const forwarding_addr = @intCast(Obj, @ptrToInt(pool.current));
    @ptrCast(*ObjForwarded, as_obj_header(&obj)).forwarding_addr = forwarding_addr;
    pool.current += size;
    return forwarding_addr;
}

fn start_gc(pool: *ObjPool) ! void {
    // std.debug.print("gc started from:{*} to:{*}\n", .{pool.from_space, pool.to_space});
    const tmp = pool.from_space;
    pool.from_space = pool.to_space;
    pool.to_space = tmp;
    pool.current = pool.from_space;
    for (pool.root.items) |obj_ptr, i| {
        pool.root.items[i].* = try copy_obj(pool, obj_ptr.*);
    }
    for (pool.stack_frames.items) |obj, i| {
        pool.stack_frames.items[i] = try copy_obj(pool, obj);
    }
    for (pool.consts.items) |obj, i| {
        pool.consts.items[i] = try copy_obj(pool, obj);
    }
    pool.current_env = try copy_obj(pool, pool.current_env);
    var scan_ptr = pool.from_space;
    while (@ptrToInt(scan_ptr) < @ptrToInt(pool.current)) {
        const obj = @intCast(Obj, @ptrToInt(scan_ptr));
        // std.debug.print("scanning {} {*}\n", .{obj_ref_type(&obj), scan_ptr});
        switch(obj_ref_type(&obj)) {
            .cons => {
                const cons = @ptrCast(*ObjConsCell, @intToPtr(*u8, obj));
                cons.car = try copy_obj(pool,cons.car);
                cons.cdr = try copy_obj(pool,cons.cdr);
            },
            .frame => {
                const frame = @ptrCast(*ObjFrame, @intToPtr(*u8, obj));
                frame.vars = try copy_obj(pool,frame.vars);
                frame.previous = try copy_obj(pool,frame.previous);
            },
            .func => {
                const func = @ptrCast(*ObjFunc, @intToPtr(*u8, obj));
                func.env = try copy_obj(pool,func.env);
            },
            .forwarded => unreachable,
        }
        scan_ptr += obj_size(obj);
    }
    // std.debug.print("gc finished pool size:{}\n", .{@ptrToInt(pool.current) - @ptrToInt(pool.from_space)});
}

fn push_root(pool: *ObjPool, obj:* Obj) ! void {
    try std.ArrayList(*Obj).append(&pool.root, obj);
}
fn pop_root(pool: *ObjPool) void {
    _ = std.ArrayList(*Obj).pop(&pool.root);
}

pub fn create_symbol_by_id(_: *ObjPool, id: usize) !Obj {
    return create_value(ObjValueType.symbol, @intCast(i32, id));
}

pub fn create_symbol(pool: *ObjPool, str: [] const u8) !Obj {
    for (pool.symbol_table.items) |sym, i| {
        if (std.mem.eql(u8, sym, str)) {
            return create_value(ObjValueType.symbol, @intCast(i32, i));
        }
    }
    const dest = try pool.allocator.alloc(u8, str.len);
    for(str[0..str.len]) |b, i| dest[i] = b;
    const idx = pool.symbol_table.items.len;
    try std.ArrayList([] const u8).append(&pool.symbol_table, dest);
    return create_symbol_by_id(pool, idx);
}

fn init_header(header:*ObjHeader, o_type: ObjRefType, value: i32) !void {
    header.i = @intCast(u64, @enumToInt(o_type)) + (@intCast(u64, value) << 32);
}

pub fn create_value(o_type: ObjValueType, value: i32) Obj {
    return (@intCast(u64, value) << 32) + (@intCast(u64, @enumToInt(o_type)) << 16) + 1;
}

pub fn create_true(_: *ObjPool) !Obj {
    return create_value(ObjValueType.b_true, 0);
}

pub fn create_false(_: *ObjPool) !Obj {
    return create_value(ObjValueType.b_false, 0);
}

pub fn create_undef(_: *ObjPool) !Obj {
    return create_value(ObjValueType.undef, 0);
}

pub fn create_number(_: *ObjPool, n: i32) !Obj {
    return create_value(ObjValueType.number_i32, n);
}

pub fn create_opaque(_: *ObjPool, n: i32) !Obj {
    return create_value(ObjValueType.buildin, n);
}

pub fn create_nil(_: *ObjPool) !Obj {
    return create_value(ObjValueType.nil, 0);
}

pub fn create_cons(pool: *ObjPool, car: *Obj, cdr: *Obj) !Obj {
    try push_root(pool, car);
    try push_root(pool, cdr);
    defer pop_root(pool);
    defer pop_root(pool);
    const cell: *ObjConsCell = try create(pool, ObjConsCell);
    try init_header(&cell.header, ObjRefType.cons, 0);
    cell.car = car.*;
    cell.cdr = cdr.*;
    return as_obj(@ptrCast(*ObjHeader, cell));
}

pub fn create_frame(pool: *ObjPool, vars: *Obj, previous: *Obj) !Obj {
    try push_root(pool, vars);
    try push_root(pool, previous);
    defer pop_root(pool);
    defer pop_root(pool);
    const frame = try create(pool, ObjFrame);
    try init_header(&frame.header, ObjRefType.frame, 0);
    frame.vars = vars.*;
    frame.previous = previous.*;
    return as_obj(@ptrCast(*ObjHeader, frame));
}

pub fn create_func(pool: *ObjPool, func_id: usize, env: *Obj) !Obj {
    try push_root(pool, env);
    defer pop_root(pool);
    const func = try create(pool, ObjFunc);
    try init_header(&func.header, ObjRefType.func, 0);
    func.func_id = func_id;
    func.env = env.*;
    return as_obj(@ptrCast(*ObjHeader, func));
}

pub fn equal(obj1: * const Obj, obj2: * const Obj) bool {
    const type1 = obj_type(obj1);
    const type2 = obj_type(obj2);
    if (type1 == .b_true and type2 == .b_true) {
        return true;
    } else if  (type1 == .b_false and type2 == .b_false) {
        return true;
    } else if (type1 == .symbol and type2 == .symbol) {
        return get_symbol_id(obj1) == get_symbol_id(obj2);
    } else if (type1 == .cons and type2 == .cons) {
        return equal(&get_car(obj1), &get_car(obj2)) and equal(&get_cdr(obj1), &get_cdr(obj2));
    } else if (type1 == .nil and type2 == .nil) {
        return true;
    } else if (type1 == .number and type2 == .number) {
        return as_number(obj1) == as_number(obj2);
    }
    return false;
}
