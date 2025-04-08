#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    Int,
    Void,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    VarDecl(VarDeclaration),
    FunDecl(FunDeclaration),
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct VarDeclaration {
    pub type_spec: TypeSpecifier,
    pub id: String,
    pub array_size: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct FunDeclaration {
    pub return_type: TypeSpecifier,
    pub id: String,
    pub params: Vec<Param>,
    pub body: CompoundStmt,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Param {
    pub type_spec: TypeSpecifier,
    pub id: String,
    pub is_array: bool,
}

#[derive(Debug, Clone)]
pub struct CompoundStmt {
    pub local_decls: Vec<VarDeclaration>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExpressionStmt(Option<Expression>),
    CompoundStmt(CompoundStmt),
    SelectionStmt {
        condition: Expression,
        then_stmt: Box<Statement>,
        else_stmt: Option<Box<Statement>>,
    },
    IterationStmt {
        condition: Expression,
        body: Box<Statement>,
    },
    ReturnStmt(Option<Expression>),
    OutputStmt(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Assign {
        var: Var,
        expr: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    Call {
        id: String,
        args: Vec<Expression>,
    },
    Var(Var),
    Num(i32),
    StringLit(String),
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    RelOp(String),
}

#[derive(Debug, Clone)]
pub enum Var {
    Id(String),
    ArrayAccess {
        id: String,
        index: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}
