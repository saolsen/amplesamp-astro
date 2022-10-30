use std::rc::Rc;

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct SourceInfo<'a> {
    pub fragment: &'a str,
    pub line: u32,
    pub col: u32,
}

#[derive(Debug, Clone)]
pub struct Src<'a, T: PartialEq> {
    pub src: SourceInfo<'a>,
    pub node: T,
}

impl<'a, T: PartialEq> PartialEq for Src<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

pub type Program<'a> = Vec<Src<'a, Decl<'a>>>;

#[derive(PartialEq, Debug, Clone)]
pub enum Decl<'a> {
    VarDecl(Src<'a, VarDecl<'a>>),
    Type(Src<'a, Type<'a>>),
    Stmt(Src<'a, Stmt<'a>>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Type<'a> {
    pub name: &'a str,
    pub fields: Vec<Src<'a, TypeField<'a>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypeField<'a> {
    pub name: &'a str,
    pub kind: &'a str,
}

/// Variable declaration
#[derive(PartialEq, Debug, Clone)]
pub struct VarDecl<'a> {
    pub name: &'a str,
    pub expr: Option<Src<'a, Expr<'a>>>,
}

/// Statement
#[derive(PartialEq, Debug, Clone)]
pub enum Stmt<'a> {
    Print(Src<'a, Expr<'a>>),
    Assign {
        target: Vec<Src<'a, &'a str>>, // eg [foo, bar, baz] for foo.bar.baz
        expr: Src<'a, Expr<'a>>,
    },
    Expression(Src<'a, Expr<'a>>), // top level expression statement, result is thrown away
}

#[derive(PartialEq, Debug, Clone)]
pub struct Object<'a> {
    pub kind: &'a str,
    pub fields: Vec<Src<'a, ObjectField<'a>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ObjectField<'a> {
    pub name: &'a str,
    pub value: Src<'a, Expr<'a>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Op {
    Not,
    Neg,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Op2 {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
}

/// Expression (not sure if this needs a seperate value type or not)
#[derive(PartialEq, Debug, Clone)]
pub enum Expr<'a> {
    Int(Src<'a, i64>),
    Dec(Src<'a, f64>),
    Var(&'a str),
    Wildcard(&'a str),
    Get(Box<Src<'a, Expr<'a>>>, Src<'a, &'a str>),

    Op2(Src<'a, Op2>, Box<Src<'a, Expr<'a>>>, Box<Src<'a, Expr<'a>>>),
    Op(Src<'a, Op>, Box<Src<'a, Expr<'a>>>),

    Create(Src<'a, Object<'a>>),
    Query(Src<'a, Object<'a>>),
}

// x
// x.y
// x.y.z
// x + y.z * 100 / (4 + 9)
// -12 is also a thing
// so there is such a thing as an assignable expression that is just
// foo
// $.foo

// the fold thing is how you get thing binding right for the binary expressions, but what about unary expressions?

// what is like the assignable thing, does that matter for eqality parsing, is = an expression??????

// exp9 = ( exp ) | value
// exp8 = exp9 opt([( args )]) | exp9 . identifier // dot has the same precidence as call
// exp7 = exp8 [. name] // is there more to this?
// exp6 = opt([-|+|!]) exp7 // this one is right to left associative!
// exp5 = exp6 [(*|/) exp6]*
// exp4 = exp5 [(+|-) exp5]*
// exp3 = exp4 [(<|>|<=|>=) exp4]*
// exp2 = exp3 [(==|!=) exp3]*
// exp1 = exp2 [&& exp2]*
// exp = exp1 [|| exp1]*

// setter is different than getter
//assignment = ((call .)?  identifier = assignment) | exp

// equality, and it's right to left associative also the left hand side has to be assignable.
// I can write tests with parens to make sure the bindings are what I would expect them to be.
