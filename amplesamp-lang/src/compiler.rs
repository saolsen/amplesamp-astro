use std::collections::HashMap;

use crate::ast;
use crate::bytecode as bc;

struct Compiler {
    program: bc::Program,
}

fn loc(src: &ast::SourceInfo) -> bc::Loc {
    bc::Loc {
        len: src.fragment.len(),
        line: src.line,
        col: src.col,
    }
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            program: bc::Program::new(),
        }
    }

    fn push_op(&mut self, loc: bc::Loc, op: bc::Op) {
        self.program.code_locs.push(loc);
        self.program.code.push(op);
    }

    fn push_constant(&mut self, loc: bc::Loc, val: bc::Value) {
        let idx = self.program.constants.len();
        self.program.constants.push(val);
        self.push_op(loc, bc::Op::Constant(idx));
    }

    // Walk the AST and compile to bytecode
    pub(crate) fn compile(mut self, input: ast::Src<ast::Program>) -> bc::Program {
        for decl in input.node {
            self.declaration(decl);
        }
        self.push_op(loc(&input.src), bc::Op::Halt);
        self.program
    }

    fn declaration(&mut self, decl: ast::Src<ast::Decl>) {
        match decl.node {
            ast::Decl::Stmt(stmt) => self.statement(stmt),
            ast::Decl::Type(src_typ) => {
                let typ_loc = loc(&src_typ.src);
                let typ = src_typ.node;
                let name = self.program.intern_type_name(&typ.name);
                let mut fields = HashMap::new();
                for field in typ.fields {
                    let _field_loc = loc(&src_typ.src);
                    let field = field.node;
                    let field_name = self.program.intern_field_name(field.name);
                    let field_type = self.program.intern_type_name(field.kind);
                    fields.insert(field_name, field_type);
                }
                self.program.types.insert(name, bc::Type { name, fields });
                self.program.type_locs.insert(name, typ_loc);
            }
            ast::Decl::VarDecl(src_vardecl) => {
                let vardecl_loc = loc(&src_vardecl.src);
                let vardecl = src_vardecl.node;
                let var_name = self.program.intern_variable_name(vardecl.name);
                if let Some(expr) = vardecl.expr {
                    self.expression(expr);
                } else {
                    self.push_constant(vardecl_loc.clone(), bc::Value::Nil);
                }
                self.push_op(vardecl_loc, bc::Op::DefineGlobal(var_name));
            }
        }
    }

    // fn var_declaration(&mut self, vardecl: Src<VarDecl>) {
    //     todo!();
    // }

    fn statement(&mut self, stmt: ast::Src<ast::Stmt>) {
        let loc = loc(&stmt.src);
        match stmt.node {
            ast::Stmt::Print(expr) => self.print_statement(loc, expr),
            ast::Stmt::Assign { target, expr } => {
                if target.len() > 1 {
                    todo!("setters");
                }
                assert!(target.len() == 1);
                let target = target.into_iter().next().unwrap();
                let name = self.program.intern_variable_name(target.node);
                self.expression(expr);
                self.push_op(loc, bc::Op::Assign(name));
            }
            ast::Stmt::Expression(expr) => {
                self.expression(expr);
                self.push_op(loc, bc::Op::Pop);
            }
        }
    }

    fn print_statement(&mut self, loc: bc::Loc, expr: ast::Src<ast::Expr>) {
        self.expression(expr);
        self.push_op(loc, bc::Op::Print);
    }

    fn expression(&mut self, expr: ast::Src<ast::Expr>) {
        let expr_loc = loc(&expr.src);
        match expr.node {
            ast::Expr::Int(i) => self.push_constant(expr_loc, bc::Value::Int(i.node)),
            ast::Expr::Dec(d) => self.push_constant(expr_loc, bc::Value::Dec(d.node)),
            ast::Expr::Var(var) => {
                let name = self.program.intern_variable_name(var);
                self.push_op(expr_loc, bc::Op::Read(name));
            }
            ast::Expr::Wildcard(type_name) => {
                let name = self.program.intern_type_name(type_name);
                self.push_op(expr_loc, bc::Op::Generate(name));
            }
            ast::Expr::Get(expr, name) => {
                self.expression(*expr);
                let name = self.program.intern_field_name(name.node);
                self.push_op(expr_loc, bc::Op::Get(name));
            }
            ast::Expr::Op2(op, lhs, rhs) => {
                self.expression(*lhs);
                self.expression(*rhs);
                let op = match op.node {
                    ast::Op2::Add => bc::Op::Add,
                    ast::Op2::Sub => bc::Op::Sub,
                    ast::Op2::Mul => bc::Op::Mul,
                    ast::Op2::Div => bc::Op::Div,
                    ast::Op2::Mod => bc::Op::Mod,
                    ast::Op2::Eq => bc::Op::Eq,
                    ast::Op2::Neq => bc::Op::Neq,
                    ast::Op2::Lt => bc::Op::Lt,
                    ast::Op2::Gt => bc::Op::Gt,
                    ast::Op2::Lte => bc::Op::Lte,
                    ast::Op2::Gte => bc::Op::Gte,
                    ast::Op2::And => bc::Op::And,
                    ast::Op2::Or => bc::Op::Or,
                };
                self.push_op(expr_loc, op);
            }
            ast::Expr::Op(op, arg) => {
                self.expression(*arg);
                let op = match op.node {
                    ast::Op::Not => bc::Op::Not,
                    ast::Op::Neg => bc::Op::Neg,
                };
                self.push_op(expr_loc, op);
            }
            ast::Expr::Create(obj) => {
                let type_name = self.program.intern_type_name(obj.node.kind);
                for field in obj.node.fields {
                    let field_loc = loc(&field.src);
                    let field = field.node;
                    let name = self.program.intern_field_name(field.name);
                    self.expression(field.value);
                    self.push_op(field_loc, bc::Op::Tag(name));
                }
                self.push_op(expr_loc, bc::Op::Create(type_name));
            }
        }
    }
}

pub fn compile(program: ast::Src<ast::Program>) -> bc::Program {
    let compiler = Compiler::new();
    compiler.compile(program)
}

#[cfg(test)]
mod tests {
    // Not super easy to test the compiler, easier to test the parser
    // and then test that programs return the results we expect.
    // Here's just a big program with everything in it to at least
    // try and hit all the compiler branches.
    use super::*;

    #[test]
    fn test_compiles() {
        let program = r#"
        type Foo { id: Int };
        type Bar { id: Int, foo: Foo };
        print 1;
        var x;
        var y = 1;
        x = 2;
        1+2+3;
        var foo = create Foo { id: 1 };
        var bar = create Bar { id: 2, foo: foo };
        "#;
        let ast = crate::parser::parse_program(program).unwrap();
        let program = compile(ast);
        program.decompile();
    }
}
