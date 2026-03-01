#[derive(Debug, Clone, PartialEq, Default)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl Program {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self { statements }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    // Explicit declaration statement.
    Let {
        name: String,
        value: Expr,
    },
    // TFEL assignment shape: `10 = x` (normal equivalent is `x = 10`).
    Assign {
        name: String,
        value: Expr,
    },
    Print {
        value: Expr,
    },
    Expr(Expr),
    If {
        condition: Expr,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    While {
        condition: Expr,
        body: Block,
    },
    For {
        name: String,
        iterable: Expr,
        body: Block,
    },
    FunctionDef {
        name: String,
        params: Vec<String>,
        body: Block,
    },
    Return(Option<Expr>),
    Break,
    Continue,
    Export {
        names: Vec<String>,
    },
    Import {
        module: String,
        item: Option<String>,
    },
}

pub type Block = Vec<Stmt>;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Identifier(String),
    Number(f64),
    String(String),
    Boolean(bool),
    Array(Vec<Expr>),
    Object(Vec<(String, Expr)>),
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Prefix {
        op: PrefixOp,
        rhs: Box<Expr>,
    },
    Infix {
        lhs: Box<Expr>,
        op: InfixOp,
        rhs: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    Not,
    Negate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOp {
    And,
    Or,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
}
