/*
<program> ::= <function>
<function> ::= "int" <id> "(" ")" "{" <statement> "}"
<statement> ::= "return" <expression> ";"
<expression> ::= <unary_op> <exp> | <int>
<unary_op> ::= "!" | "~" | "-"
*/

use std::rc::Rc;

pub mod lexer;
pub mod parser;



//----------------------AST----------------------------------------------------
#[derive(Debug,PartialEq,Clone)]
pub struct Ast{
    program: AstProgram,
}

impl Ast {
    pub fn new(program: AstProgram)->Self{
        Self{
            program,
        }
    }

    pub fn get_program(&self)->&AstProgram{
        &self.program
    }
}


//----------------------ASTProgram--------------------------------------------------
#[derive(Debug,PartialEq,Clone)]
pub struct AstProgram{
    function: AstFunction,
}

impl AstProgram{
    pub fn new(function: AstFunction)->Self{
        Self{
            function
        }
    }

    pub fn get_function(&self)->&AstFunction{
        &self.function
    }
}

#[derive(Debug,PartialEq,Clone)]
pub struct AstFunction{
    id: String,
    statement: AstStatement,
}


//----------------------ASTFunction--------------------------------------------------
impl AstFunction{
    pub fn new(id: String, statement: AstStatement)->Self{
        Self{
            id,
            statement,
        }
    }

    pub fn get_id(&self)->String{
        self.id.clone()
    }

    pub fn get_statement(&self)->&AstStatement{
        &self.statement
    }
}


//----------------------ASTStatement--------------------------------------------------
#[derive(Debug,PartialEq,Clone)]
pub struct AstStatement{
    expression: AstExpression,
}

impl AstStatement {
    pub fn new(expression: AstExpression)->Self{
        Self { 
            expression, 
        }
    }

    pub fn get_expression(&self)->&AstExpression{
        &self.expression
    }

}


//----------------------ASTExpression--------------------------------------------------
#[derive(Debug,PartialEq,Clone)]
pub enum AstExpression{
    UnaryOpExpression(AstExpressionUnaryOpExpression),
    Int(AstExpressionInt),
}

#[derive(Debug,PartialEq,Clone)]
pub struct AstExpressionUnaryOpExpression{
    op: AstUnaryOp,
    expression: Rc<AstExpression>,
}

impl AstExpressionUnaryOpExpression{
    pub fn new(op: AstUnaryOp, expression: AstExpression)->Self{
        Self { 
            op, 
            expression: Rc::new(expression)
        }
    }

    pub fn get_op(&self)->AstUnaryOp{
        self.op.clone()
    }

    pub fn get_expression(&self)->Rc<AstExpression>{
        Rc::clone(&self.expression)
    }
}

#[derive(Debug,PartialEq,Clone)]
pub struct AstExpressionInt{
    val: usize,

}

impl AstExpressionInt {
    pub fn new(val: usize)->Self{
        Self{
            val,
        }
    }

    pub fn get_val(&self)->usize{
        self.val
    }
}


//----------------------ASTUnaryOp--------------------------------------------------
#[derive(Debug,PartialEq,Clone,Copy)]
pub enum UnaryOpType{
    Minus,
    Complement,
    LogicNegation,
}

#[derive(Debug,PartialEq,Clone)]
pub struct AstUnaryOp{
    op_type: UnaryOpType,
}

impl AstUnaryOp {
    pub fn new(op_type: UnaryOpType)->Self{
        Self{
            op_type,
        }
    }

    pub fn get_type(&self)->UnaryOpType{
        self.op_type
    }
}