const std = @import("std");

const allocator = std.heap.page_allocator;

const Error = error{
    OutOfMemory,
    ParseError,
    CompileError,
};

const TokenKind = enum {
    whitespace,
    comment,
    ident,
    integer,

    keyword_false,
    keyword_true,
    keyword_if,
    keyword_else,
    keyword_output,

    @"{",
    @"}",
    @"(",
    @")",

    @"~",

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
        .{ "false", .keyword_false },
        .{ "true", .keyword_true },
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
    if_else: struct { cond: *Ast, true: []*Ast, false: []*Ast },
};

const ErrorSource = struct {
    source: []const u8,
    message: []const u8,
};

const Parser = struct {
    buf: []const u8,
    idx: usize = 0,
    next_token: ?Token = null,
    errors: std.ArrayList(ErrorSource) = .empty,

    fn pushErr(self: *Parser, source: []const u8, comptime fmt: []const u8, args: anytype) Error!void {
        const message = try std.fmt.allocPrint(allocator, fmt, args);
        try self.errors.append(allocator, ErrorSource{
            .source = source,
            .message = message,
        });
    }

    fn err(self: *Parser, source: []const u8, comptime fmt: []const u8, args: anytype) Error {
        try self.pushErr(source, fmt, args);
        return error.ParseError;
    }

    fn errNext(self: *Parser, comptime fmt: []const u8, args: anytype) Error {
        const source = self.next_token.?.source;
        return self.err(source, fmt, args);
    }

    fn errContext(self: *Parser, comptime fmt: []const u8, args: anytype) Error!void {
        const last = self.errors.getLastOrNull() orelse return;
        try self.pushErr(last.source, fmt, args);
    }

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
            '#' => {
                while (true) {
                    switch (self.nextByte()) {
                        '\n', '\x00' => break,
                        else => {},
                    }
                }
                break :b .comment;
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

            '=' => if (self.takeByte('=')) .@"==" else .@"=",
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
            '~' => .@"~",

            '{' => .@"{",
            '}' => .@"}",
            '(' => .@"(",
            ')' => .@")",

            '\x00' => .eof,

            else => {
                const source = self.buf[start..self.idx];
                self.idx = start;
                return self.err(source, "Unknown character", .{});
            },
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

    fn skipWhitespace(self: *Parser) void {
        while (true) {
            const t = self.peek() catch {
                return self.clearErrors();
            };
            switch (t.kind) {
                .whitespace, .comment => {},
                else => break,
            }
            self.skip();
        }
    }

    fn allocAst(self: *Parser, value: Ast) Error!*Ast {
        _ = self;
        const ast = try allocator.create(Ast);
        ast.* = value;
        return ast;
    }

    fn atom(self: *Parser) Error!*Ast {
        const t = try self.token();
        switch (t.kind) {
            .@"!", .@"-", .@"~" => {
                const inner = self.atom() catch |e| {
                    try self.pushErr(t.source, "Expected a value after unary operator", .{});
                    return e;
                };

                return self.allocAst(Ast{ .unary = .{ .op = t.kind, .x = inner } });
            },
            .ident => return self.allocAst(Ast{ .variable = t.source }),
            .keyword_false => return self.allocAst(Ast{ .integer = 0 }),
            .keyword_true => return self.allocAst(Ast{ .integer = 1 }),
            .integer => {
                const value = std.fmt.parseInt(i64, t.source, 10) catch {
                    return self.err(t.source, "Invalid integer value", .{});
                };

                return self.allocAst(Ast{ .integer = value });
            },
            .keyword_output => {
                const inner = try self.expr();
                self.skipWhitespace();

                return self.allocAst(Ast{ .output = .{ .x = inner } });
            },
            .keyword_if => {
                const cond = try self.expr();
                self.skipWhitespace();
                _ = try self.take(.@"{") orelse {
                    return self.errNext("Missing opening {{", .{});
                };
                const true_block = try self.parseBlock();
                self.skipWhitespace();
                var false_block: []*Ast = &.{};
                if (try self.take(.keyword_else)) |_| {
                    self.skipWhitespace();
                    _ = try self.take(.@"{") orelse {
                        return self.errNext("Missing opening {{", .{});
                    };
                    false_block = try self.parseBlock();
                }
                return self.allocAst(Ast{ .if_else = .{
                    .cond = cond,
                    .true = true_block,
                    .false = false_block,
                } });
            },
            .@"(" => {
                self.skipWhitespace();
                const inner = self.expr() catch |e| {
                    try self.errContext("Expected an expression within parentheses", .{});
                    return e;
                };
                self.skipWhitespace();

                _ = try self.take(.@")") orelse {
                    return self.errNext("Missing closing )", .{});
                };

                return inner;
            },
            else => return self.err(t.source, "Unexpected token", .{}),
        }
    }

    fn exprBp(self: *Parser, min_bp: u8) Error!*Ast {
        self.skipWhitespace();

        var lhs = try self.atom();

        while (true) {
            self.skipWhitespace();

            const t = self.peek() catch |e| {
                try self.errContext("Expected a binary operator after an expression", .{});
                return e;
            };
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

            const rhs = self.exprBp(r_bp) catch |e| {
                try self.pushErr(t.source, "Expected a value after a binary operator", .{});
                return e;
            };

            lhs = try self.allocAst(Ast{ .binary = .{
                .op = t.kind,
                .a = lhs,
                .b = rhs,
            } });
        }

        return lhs;
    }

    fn expr(self: *Parser) Error!*Ast {
        return self.exprBp(0);
    }

    fn parseBlock(self: *Parser) Error![]*Ast {
        var asts: std.ArrayList(*Ast) = .empty;

        while (true) {
            self.skipWhitespace();
            if (try self.take(.@"}")) |_| break;
            const ast = self.expr() catch |e| {
                try self.errContext("Expected a statement", .{});
                return e;
            };
            try asts.append(allocator, ast);
        }

        return try asts.toOwnedSlice(allocator);
    }

    fn clearErrors(self: *Parser) void {
        for (self.errors.items) |e| {
            allocator.free(e.message);
        }
        self.errors.clearRetainingCapacity();
    }

    fn outputErrorMessages(self: *Parser) void {
        for (self.errors.items) |e| {
            const start_idx = @intFromPtr(e.source.ptr) - @intFromPtr(self.buf.ptr);

            var line_count: usize = 1;
            var line_start: usize = 0;
            for (self.buf[0..start_idx], 0..) |c, i| {
                if (c == '\n') {
                    line_start = i + 1;
                    line_count += 1;
                }
            }

            var linue_number = line_count;

            for (e.source) |c| {
                if (c == '\n') {
                    line_count += 1;
                }
            }

            const end_idx = start_idx + e.source.len;

            var line_end: usize = end_idx;
            for (self.buf[line_end..], line_end..) |c, i| {
                if (c == '\n') {
                    line_end = i;
                    break;
                }
            } else {
                line_end = self.buf.len;
            }

            const max_line_number_width = std.math.log10(line_count) + 1;

            var lines = std.mem.splitScalar(u8, self.buf[line_start..line_end], '\n');
            while (lines.next()) |line| : (linue_number += 1) {
                std.debug.print("{}", .{linue_number});
                for (0..max_line_number_width - (std.math.log10(linue_number) + 1)) |_| {
                    std.debug.print(" ", .{});
                }
                std.debug.print(" | {s}\n", .{line});

                const overlap_start = @max(@intFromPtr(line.ptr), @intFromPtr(e.source.ptr));
                const overlap_end = @min(@intFromPtr(line.ptr) + line.len, @intFromPtr(e.source.ptr) + e.source.len);
                if (overlap_start < overlap_end) {
                    var pad = max_line_number_width + 3;
                    if (@intFromPtr(line.ptr) < overlap_start) {
                        pad += overlap_start - @intFromPtr(line.ptr);
                    }
                    for (0..pad) |_| {
                        std.debug.print(" ", .{});
                    }
                    for (0..overlap_end - overlap_start) |_| {
                        std.debug.print("~", .{});
                    }
                    std.debug.print("\n", .{});
                }
            }

            std.debug.print("error: {s}\n\n", .{e.message});
        }
    }

    fn parseProgram(self: *Parser) Error!std.ArrayList(*Ast) {
        var asts: std.ArrayList(*Ast) = .empty;

        while (true) {
            self.skipWhitespace();
            if (try self.take(.eof)) |_| break;
            const ast = self.expr() catch |e| {
                try self.errContext("Expected a statement", .{});
                return e;
            };
            try asts.append(allocator, ast);
        }

        for (asts.items) |ast| {
            std.debug.print("{}\n", .{ast});
        }

        return asts;
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

    fn bitand(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, a & b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn bitor(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, a | b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn bitxor(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, a ^ b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn shl(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, a << @truncate(@as(u64, @bitCast(b))));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn shr(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, a >> @truncate(@as(u64, @bitCast(b))));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set_add(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const a = try vm.get(ops[0].data.variable);
        const b = vm.pop(&sp);
        try vm.set(ops[0].data.variable, a +% b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set_sub(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const a = try vm.get(ops[0].data.variable);
        const b = vm.pop(&sp);
        try vm.set(ops[0].data.variable, a -% b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set_mul(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const a = try vm.get(ops[0].data.variable);
        const b = vm.pop(&sp);
        try vm.set(ops[0].data.variable, a *% b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set_div(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const a = try vm.get(ops[0].data.variable);
        const b = vm.pop(&sp);
        if (b == 0) return error.EvalError;
        try vm.set(ops[0].data.variable, @divTrunc(a, b));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set_rem(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const a = try vm.get(ops[0].data.variable);
        const b = vm.pop(&sp);
        if (b == 0) return error.EvalError;
        try vm.set(ops[0].data.variable, @rem(a, b));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set_bitand(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const a = try vm.get(ops[0].data.variable);
        const b = vm.pop(&sp);
        try vm.set(ops[0].data.variable, a & b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set_bitor(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const a = try vm.get(ops[0].data.variable);
        const b = vm.pop(&sp);
        try vm.set(ops[0].data.variable, a | b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set_bitxor(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const a = try vm.get(ops[0].data.variable);
        const b = vm.pop(&sp);
        try vm.set(ops[0].data.variable, a ^ b);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set_shl(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const a = try vm.get(ops[0].data.variable);
        const b = vm.pop(&sp);
        try vm.set(ops[0].data.variable, a << @truncate(@as(u64, @bitCast(b))));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn set_shr(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const a = try vm.get(ops[0].data.variable);
        const b = vm.pop(&sp);
        try vm.set(ops[0].data.variable, a >> @truncate(@as(u64, @bitCast(b))));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn eq(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, @intFromBool(a == b));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn ne(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, @intFromBool(a != b));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn lt(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, @intFromBool(a < b));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn le(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, @intFromBool(a <= b));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn gt(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, @intFromBool(a > b));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn ge(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const b = vm.pop(&sp);
        const a = vm.pop(&sp);
        vm.push(&sp, @intFromBool(a >= b));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn eqz(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const x = vm.pop(&sp);
        vm.push(&sp, @intFromBool(x == 0));
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn neg(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const x = vm.pop(&sp);
        vm.push(&sp, -%x);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn not(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const x = vm.pop(&sp);
        vm.push(&sp, ~x);
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn output(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const value = vm.pop(&sp);
        std.debug.print("debug: {}\n", .{value});
        return @call(.always_tail, ops[1].eval, .{ vm, sp, ops + 1 });
    }

    fn jump_if_not(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        var sp = stack_ptr;
        const x = vm.pop(&sp);
        const next_op: [*]const Expr = @ptrFromInt(@intFromPtr(ops) +% if (x == 0) ops[0].data.offset else @sizeOf(Expr));
        return @call(.always_tail, next_op[0].eval, .{ vm, sp, next_op });
    }

    fn jump(vm: *VM, stack_ptr: usize, ops: [*]const Expr) VM.Error!void {
        const next_op: [*]const Expr = @ptrFromInt(@intFromPtr(ops) +% ops[0].data.offset);
        return @call(.always_tail, next_op[0].eval, .{ vm, stack_ptr, next_op });
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
            .unary => |info| {
                try self.expr(info.x);
                const eval: EvalFn = switch (info.op) {
                    .@"!" => impl.eqz,
                    .@"-" => impl.neg,
                    .@"~" => impl.not,
                    else => @panic("err"),
                };
                try self.exprs.append(allocator, Expr{
                    .eval = eval,
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
                    .@"&" => impl.bitand,
                    .@"|" => impl.bitor,
                    .@"^" => impl.bitxor,
                    .@"<<" => impl.shl,
                    .@">>" => impl.shr,
                    .@"!=" => impl.ne,
                    .@"<" => impl.lt,
                    .@"<=" => impl.le,
                    .@">" => impl.gt,
                    .@">=" => impl.ge,
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
                const eval: EvalFn = switch (info.op) {
                    .@"=" => impl.set,
                    .@"+=" => impl.set_add,
                    .@"-=" => impl.set_sub,
                    .@"*=" => impl.set_mul,
                    .@"/=" => impl.set_div,
                    .@"%=" => impl.set_rem,
                    .@"&=" => impl.set_bitand,
                    .@"|=" => impl.set_bitor,
                    .@"^=" => impl.set_bitxor,
                    .@"<<=" => impl.set_shl,
                    .@">>=" => impl.set_shr,
                    else => @panic("err"),
                };

                if (info.a.* != .variable) return error.CompileError;
                try self.expr(info.b);
                try self.exprs.append(allocator, Expr{
                    .eval = eval,
                    .data = .{ .variable = info.a.variable },
                });
                return;
            },
            .output => |info| {
                try self.expr(info.x);
                try self.exprs.append(allocator, Expr{
                    .eval = impl.output,
                });
                return;
            },
            .if_else => |info| {
                try self.expr(info.cond);

                // Jump to the false condition body if the condition is false and continue as is otherwise
                const true_start_expr = self.exprs.items.len;
                var skip_true_expr = try self.exprs.addOne(allocator);
                skip_true_expr.eval = impl.jump_if_not;

                for (info.true) |inner| {
                    try self.stmt(inner);
                }

                const has_false = info.false.len > 0;
                const true_insts = self.exprs.items.len - true_start_expr + @intFromBool(has_false);
                skip_true_expr.data = .{ .offset = @sizeOf(Expr) * true_insts };

                // At the end of the true condition body skip the false condition body if it exists
                if (has_false) {
                    const false_start_expr = self.exprs.items.len;
                    var skip_false_expr = try self.exprs.addOne(allocator);
                    skip_false_expr.eval = impl.jump;

                    for (info.false) |inner| {
                        try self.stmt(inner);
                    }

                    const false_insts = self.exprs.items.len - false_start_expr;
                    skip_false_expr.data = .{ .offset = @sizeOf(Expr) * false_insts };
                }

                return;
            },
            else => {},
        }
        @panic("unsupported");
    }
};

pub fn main() !void {
    const cwd = std.fs.cwd();
    const code = try cwd.readFileAlloc(
        allocator,
        "test.code",
        std.math.maxInt(usize),
    );

    var parser = Parser{ .buf = code };

    const asts = parser.parseProgram() catch |e| {
        if (e == error.ParseError) {
            parser.outputErrorMessages();
            return;
        }
        return e;
    };

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

    const vm = try VM.create();

    const inst = compiler.exprs.items.ptr;
    try inst[0].eval(vm, 0, inst);
}
