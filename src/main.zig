const std = @import("std");

const allocator = std.heap.page_allocator;

const Error = error{
    OutOfMemory,
    ParseError,
    CompileError,
    UnknownVariable,
    StackOverflow,
    StackUnderflow,
    DivisionByZero,
};

const TokenKind = enum {
    whitespace,
    ident,
    integer,

    keyword_true,
    keyword_false,
    keyword_if,
    keyword_else,
    keyword_output,

    @"{",
    @"}",
    @"(",
    @")",
    @"=>",

    @"=",
    @"+=",
    @"-=",
    @"*=",
    @"/=",
    @"%=",
    @"&=",
    @"|=",
    @"^=",
    @"<<=",
    @">>=",

    @"+",
    @"-",
    @"*",
    @"/",
    @"%",
    @"&",
    @"|",
    @"^",
    @"<<",
    @">>",

    @"!",
    @"!=",
    @"==",
    @"<",
    @"<=",
    @">",
    @">=",

    eof,
};

const keywords =
    std.StaticStringMap(TokenKind).initComptime(.{
        .{ "true", .keyword_true },
        .{ "false", .keyword_false },
        .{ "if", .keyword_if },
        .{ "else", .keyword_else },
        .{ "output", .keyword_output },
    });

const Token = struct {
    source: []const u8,
    kind: TokenKind,
};

const Ast = union(enum) {
    variable: []const u8,
    integer: i64,
    unary: struct { op: TokenKind, x: *Ast },
    binary: struct { op: TokenKind, a: *Ast, b: *Ast },
    if_else_cond: struct { true: *Ast, false: *Ast },
    output: struct { x: *Ast },
};

const Parser = struct {
    buf: []const u8,
    idx: usize = 0,
    next_token: ?Token = null,

    fn skipByte(self: *Parser) void {
        std.debug.assert(self.idx < self.buf.len);
        self.idx += 1;
    }

    fn peekByte(self: *Parser) u8 {
        if (self.idx >= self.buf.len) return '\x00';
        return self.buf[self.idx];
    }

    fn nextByte(self: *Parser) u8 {
        if (self.idx >= self.buf.len) return '\x00';
        const c = self.buf[self.idx];
        self.idx += 1;
        return c;
    }

    fn takeByte(self: *Parser, target: u8) bool {
        if (self.peekByte() != target) return false;
        self.skipByte();
        return true;
    }

    fn token(self: *Parser) Error!Token {
        if (self.next_token) |t| {
            self.next_token = null;
            return t;
        }
        const start = self.idx;
        const first = self.nextByte();
        const kind: TokenKind = b: switch (first) {
            ' ', '\t'...'\n' => {
                while (true) {
                    const c = self.peekByte();
                    switch (c) {
                        ' ', '\t'...'\n' => self.skipByte(),
                        else => break,
                    }
                }
                break :b .whitespace;
            },
            '_', 'a'...'z', 'A'...'Z' => {
                while (true) {
                    const c = self.peekByte();
                    switch (c) {
                        '_', 'a'...'z', 'A'...'Z', '0'...'9' => self.skipByte(),
                        else => break,
                    }
                }
                break :b keywords.get(self.buf[start..self.idx]) orelse .ident;
            },
            '0'...'9' => {
                while (true) {
                    const c = self.peekByte();
                    switch (c) {
                        '0'...'9' => self.skipByte(),
                        else => break,
                    }
                }
                break :b .integer;
            },

            '=' => if (self.takeByte('=')) .@"==" else if (self.takeByte('>')) .@"=>" else .@"=",
            '!' => if (self.takeByte('=')) .@"!=" else .@"!",
            '<' => if (self.takeByte('<'))
                if (self.takeByte('=')) .@"<<=" else .@"<<"
            else if (self.takeByte('='))
                .@"<="
            else
                .@"<",
            '>' => if (self.takeByte('>'))
                if (self.takeByte('=')) .@">>=" else .@">>"
            else if (self.takeByte('='))
                .@">="
            else
                .@">",
            '+' => if (self.takeByte('=')) .@"+=" else .@"+",
            '-' => if (self.takeByte('=')) .@"-=" else .@"-",
            '*' => if (self.takeByte('=')) .@"*=" else .@"*",
            '/' => if (self.takeByte('=')) .@"/=" else .@"/",
            '%' => if (self.takeByte('=')) .@"%=" else .@"%",
            '&' => if (self.takeByte('=')) .@"&=" else .@"&",
            '|' => if (self.takeByte('=')) .@"|=" else .@"|",
            '^' => if (self.takeByte('=')) .@"^=" else .@"^",

            '{' => .@"{",
            '}' => .@"}",
            '(' => .@"(",
            ')' => .@")",

            '\x00' => .eof,

            else => std.debug.panic("Unknown character: '{f}'", .{std.zig.fmtChar(first)}),
        };
        const source = self.buf[start..self.idx];
        return Token{ .source = source, .kind = kind };
    }

    fn peek(self: *Parser) Error!Token {
        const t = try self.token();
        self.next_token = t;
        return t;
    }

    fn skip(self: *Parser) void {
        std.debug.assert(self.next_token != null);
        self.next_token = null;
    }

    fn take(self: *Parser, kind: TokenKind) !?[]const u8 {
        const t = try self.peek();
        if (t.kind != kind) return null;
        self.skip();
        return t.source;
    }

    fn skipWhitespace(self: *Parser) Error!void {
        while (true) {
            const t = try self.peek();
            if (t.kind != .whitespace) break;
            self.skip();
        }
    }

    fn atom(self: *Parser) Error!*Ast {
        const t = try self.token();
        const ast = try allocator.create(Ast);
        switch (t.kind) {
            .@"!", .@"-" => {
                const inner = try self.atom();
                ast.* = Ast{ .unary = .{ .op = t.kind, .x = inner } };
            },
            .ident => ast.* = Ast{ .variable = t.source },
            .integer => {
                const value = std.fmt.parseInt(i64, t.source, 10) catch {
                    return error.ParseError;
                };
                ast.* = Ast{ .integer = value };
            },
            .keyword_output => {
                const inner = try self.expr();
                try self.skipWhitespace();
                ast.* = Ast{ .output = .{ .x = inner } };
            },
            .@"(" => {
                allocator.destroy(ast);

                try self.skipWhitespace();
                const inner = try self.expr();
                try self.skipWhitespace();

                _ = try self.take(.@")") orelse {
                    return error.ParseError;
                };

                return inner;
            },
            else => @panic("err"),
        }
        return ast;
    }

    fn exprBp(self: *Parser, min_bp: u8) Error!*Ast {
        try self.skipWhitespace();

        var lhs = try self.atom();

        while (true) {
            try self.skipWhitespace();

            const t = try self.peek();
            const l_bp: u8, const r_bp: u8 = switch (t.kind) {
                .@"=", .@"+=", .@"-=", .@"*=", .@"/=", .@"%=", .@"&=", .@"|=", .@"^=", .@"<<=", .@">>=" => .{ 1, 0 }, // right-assoc
                .@"|" => .{ 2, 3 },
                .@"^" => .{ 4, 5 },
                .@"&" => .{ 6, 7 },
                .@"==", .@"!=" => .{ 8, 9 },
                .@"<", .@"<=", .@">", .@">=" => .{ 10, 11 },
                .@"<<", .@">>" => .{ 12, 13 },
                .@"+", .@"-" => .{ 14, 15 },
                .@"*", .@"/", .@"%" => .{ 16, 17 },
                else => break,
            };

            if (l_bp < min_bp) break;

            self.skip();

            const rhs = try self.exprBp(r_bp);

            const ast = try allocator.create(Ast);
            ast.* = Ast{ .binary = .{ .op = t.kind, .a = lhs, .b = rhs } };
            lhs = ast;
        }

        return lhs;
    }

    fn expr(self: *Parser) Error!*Ast {
        return self.exprBp(0);
    }
};

const EvalFn = *const fn (vm: *VM, stack_ptr: usize, data: [*]const Expr) VM.Error!void;

const ExprData = union {
    void: void,
    int: i64,
    variable: []const u8,
    offset: usize,
};

const Expr = struct {
    eval: EvalFn,
    data: ExprData = .{ .void = {} },
};

const Value = i64;

const VM = struct {
    pub const Error = error{EvalError};

    variables: std.StringHashMapUnmanaged(Value) = .empty,
    // stack: std.ArrayList(Value) = .empty,
    stack: [1024]Value = undefined,

    pub fn create() !*VM {
        const self = try allocator.create(VM);
        self.* = .{};
        return self;
    }

    pub fn set(self: *VM, name: []const u8, value: Value) VM.Error!void {
        self.variables.put(allocator, name, value) catch {
            return error.EvalError;
        };
    }

    pub fn get(self: *VM, name: []const u8) VM.Error!Value {
        return self.variables.get(name) orelse {
            return error.EvalError;
        };
    }

    pub inline fn ensureStackBounds(self: *VM, stack_ptr: usize, comptime min: usize, comptime max: usize) bool {
        return min <= stack_ptr and self.stack.len - stack_ptr >= max;
    }

    pub inline fn push(self: *VM, stack_ptr: *usize, value: Value) void {
        self.stack[stack_ptr.*] = value;
        stack_ptr.* += 1;
    }

    pub inline fn pop(self: *VM, stack_ptr: *usize) Value {
        stack_ptr.* -= 1;
        return self.stack[stack_ptr.*];
    }

    // pub fn push(self: *VM, value: Value) VM.Error!void {
    //     // std.debug.print("{any}\n", .{self.stack.items});
    //     self.stack.appendBounded(value) catch {
    //         return error.EvalError;
    //     };
    // }

    // pub fn pop(self: *VM) VM.Error!Value {
    //     // std.debug.print("{any}\n", .{self.stack.items});
    //     return self.stack.pop() orelse {
    //         return error.EvalError;
    //     };
    // }
};

const impl = struct {
    fn integer(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        vm.push(&sp, ops[0].data.int);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const value = vm.pop(&sp);
        try vm.set(ops[0].data.variable, value);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn get(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const value = try vm.get(ops[0].data.variable);
        vm.push(&sp, value);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn add(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, a +% b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn sub(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, a -% b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn mul(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, a *% b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn div(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        if (b == 0) return error.EvalError;
        vm.push(&sp, @divTrunc(a, b));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn rem(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        if (b == 0) return error.EvalError;
        vm.push(&sp, @rem(a, b));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn output(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const value = vm.pop(&sp);
        std.debug.print("debug: {}\n", .{value});
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn stop(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        _ = vm;
        _ = stack_ptr;
        _ = ops;
    }
};

const Compiler = struct {
    exprs: std.ArrayList(Expr) = .empty,

    pub fn expr(self: *Compiler, ast: *const Ast) Error!void {
        switch (ast.*) {
            .variable => |name| {
                try self.exprs.append(allocator, Expr{
                    .eval = impl.get,
                    .data = .{ .variable = name },
                });
                return;
            },
            .integer => |value| {
                try self.exprs.append(allocator, Expr{
                    .eval = impl.integer,
                    .data = .{ .int = value },
                });
                return;
            },
            .binary => |info| {
                try self.expr(info.a);
                try self.expr(info.b);
                const eval: EvalFn = switch (info.op) {
                    .@"+" => impl.add,
                    .@"-" => impl.sub,
                    .@"*" => impl.mul,
                    .@"/" => impl.div,
                    .@"%" => impl.rem,
                    else => @panic("err"),
                };
                try self.exprs.append(allocator, Expr{
                    .eval = eval,
                });
                return;
            },
            else => {},
        }
        @panic("unsupported");
    }

    pub fn stmt(self: *Compiler, ast: *const Ast) Error!void {
        switch (ast.*) {
            .binary => |info| {
                switch (info.op) {
                    .@"=" => {
                        if (info.a.* != .variable) return error.CompileError;
                        try self.expr(info.b);
                        try self.exprs.append(allocator, Expr{
                            .eval = impl.set,
                            .data = .{ .variable = info.a.variable },
                        });
                        return;
                    },
                    else => @panic("err"),
                }
            },
            .output => |info| {
                try self.expr(info.x);
                try self.exprs.append(allocator, Expr{
                    .eval = impl.output,
                });
                return;
            },
            else => {},
        }
        @panic("unsupported");
    }
};

pub fn main() !void {
    const cwd = std.fs.cwd();
    const code = try cwd.readFileAlloc(allocator, "test.code", std.math.maxInt(usize));

    var parser = Parser{ .buf = code };

    var asts: std.ArrayList(*Ast) = .empty;

    while (true) {
        try parser.skipWhitespace();
        if (try parser.take(.eof)) |_| break;
        try asts.append(allocator, try parser.expr());
    }

    for (asts.items) |ast| {
        std.debug.print("{}\n", .{ast});
    }

    var compiler = Compiler{};

    for (asts.items) |ast| {
        try compiler.stmt(ast);
    }

    try compiler.exprs.append(allocator, Expr{
        .eval = impl.stop,
    });

    for (compiler.exprs.items) |expr| {
        std.debug.print("{}\n", .{expr});
    }

    // const stack = try std.ArrayList(Value).initCapacity(allocator, 1024);
    // var vm = VM{ .stack = stack };
    const vm = try VM.create();

    const inst = compiler.exprs.items.ptr;
    try inst[0].eval(vm, 0, inst);
}
