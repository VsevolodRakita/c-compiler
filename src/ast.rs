/*
<program> ::= <function>
<function> ::= "int" <id> "(" ")" "{" <statement> "}"
<statement> ::= "return" <expression> ";"
<expression> ::= <term> | <term><expression_aux>
<expression_aux> ::= ("+" | "-") <term> | ("+" | "-") <term><expression_aux>
<term> ::= <factor> |<factor><term_aux>
<term_aux> ::= ("*" | "/") <factor> | ("*" | "/") <factor><term_aux>
<factor> ::= "(" <expression> ")" | <unary_op> <factor> | <int>
<unary_op> ::= "!" | "~" | "-"
*/

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
    Term(Box<AstTerm>),
    TermExpAux(Box<AstTerm>,Box<AstExpressionAux>),
}

#[derive(Debug,PartialEq,Clone)]
pub enum AstExpressionAux{
    PlusTerm(Box<AstTerm>),
    MinusTerm(Box<AstTerm>),
    PlusTermAux(Box<AstTerm>,Box<AstExpressionAux>),
    MinusTermAux(Box<AstTerm>,Box<AstExpressionAux>),
}


//----------------------ASTTerm--------------------------------------------------
#[derive(Debug,PartialEq,Clone)]
pub enum AstTerm{
    Factor(Box<AstFactor>),
    FactorAux(Box<AstFactor>,Box<AstTermAux>)
}

#[derive(Debug,PartialEq,Clone)]
pub enum AstTermAux{
    TimesFactor(Box<AstFactor>),
    DivideFactor(Box<AstFactor>),
    TimesFactorAux(Box<AstFactor>,Box<AstTermAux>),
    DivideFactorAux(Box<AstFactor>,Box<AstTermAux>),
}

//----------------------ASTFactor--------------------------------------------------

#[derive(Debug,PartialEq,Clone)]
pub enum AstFactor{
    Expression(Box<AstExpression>),
    UnaryOpFactor(AstUnaryOp,Box<AstFactor>),
    Int(usize),
}





//----------------------ASTUnaryOp--------------------------------------------------
#[derive(Debug,PartialEq,Clone,Copy)]
pub enum AstUnaryOp{
    Minus,
    Complement,
    LogicNegation,
}