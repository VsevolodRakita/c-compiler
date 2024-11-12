/*
<program> ::= <function>
<function> ::= "int" <id> "(" ")" "{" <function-aux> "}"
<function-aux> ::= <statement> || <statement> <function-aux>
<statement> ::= "return" <expression> ";" || <expression> ";" || "int" <id>";" || "int" <id> "=" <expression>";"
<expression> ::= <id> "=" <expression> || <logical-or-exp>
<logical-or-exp> ::= <logical-and-exp> || <logical-and-exp> <logical-or-exp-aux>
<logical-or-exp-aux> ::= "||" <logical-and-exp> ||  "||" <logical-and-exp><logical-or-exp-aux>
<logical-and-exp> ::=<bit-or-exp>  || <bit-or-exp> <logical-and-exp-aux>
<logical-and-exp-aux> ::= "&&"<bit-or-exp> || "&&"<bit-or-exp><logical-and-exp-aux>
<bit-or-exp> ::= <bit-xor-exp> || <bit-xor-exp> <bit-or-exp-aux>
<bit-or-exp-aux> ::= ("|")<bit-xor-exp> || ("|")<bit-xor-exp><bit-or-exp-aux>
<bit-xor-exp> ::= <bit-and-exp> || <bit-and-exp> <bit-xor-exp-aux>
<bit-xor-exp-aux> ::= ("^") <bit-and-exp> || ("^") <bit-and-exp><bit-xor-exp-aux>
<bit-and-exp> ::= <equality-exp>|| <equality-exp> <bit-and-exp-aux>
<bit-and-exp-aux> ::= ("&") <equality-exp> || ("&") <equality-exp><bit-and-exp-aux>
<equality-exp> ::= <relational-exp> || <relational-exp> <equality-exp-aux>
<equality-exp-aux> ::= ("!=" | "==") <relational-exp> || ("!=" | "==") <relational-exp><equality-exp-aux>
<relational-exp> ::= <bit-shift-exp> || <bit-shift-exp> <relational-exp-aux>
<relational-exp-aux> ::= ("<" | ">" | "<=" | ">=") <bit-shift-exp> || ("<" | ">" | "<=" | ">=") <bit-shift-exp><relational-exp-aux>
<bit-shift-exp> ::= <additive-exp> || <additive-exp> <bit-shift-exp-aux>
<bit-shift-exp-aux>::= ("<<"|">>") <additive-exp> || ("<<"|">>") <additive-exp><bit-shift-exp-aux>
<additive-exp> ::= <term> || <term> <additive-exp-aux>
<additive-exp-aux>::= ("+" | "-" ) <term> || ("+" | "-" ) <term> <additive-exp-aux>
<term> ::= <factor> || <factor> <term-aux>
<term-aux> ::=   ("*" | "/"| "%") <factor> || ("*" | "/"| "%") <factor> <term-aux>
<factor> ::= "(" <expression> ")" | <unary_op> <factor> | <int> | <id>
<unary_op> ::= "!" | "~" | "-"
*/

use std::collections::HashMap;

pub mod lexer;
pub mod parser;

/// Macro for creating ast node types for recursive rules similar to
/// <term> ::= <factor> || <factor> <term-aux>
/// <term-aux> ::=   ("*" | "/"| "%") <factor> || ("*" | "/"| "%") <factor> <term-aux>
/// 
/// #Arguments
/// - `$name` - the name of the node type (in the rules above: `AstTerm`)
/// - `$name_aux` - the name of the auxiliary node type (in the rules above: `AstTermAux`)
/// - `$child_name` - the name of the first derivation of the first rule (in the rules above: `Factor`)
/// - `$child_name_aux` - the name of the second derivation of the first rule (in the rules above: `FactorAux`)
/// - `$child_type` - the type of node in the derivations ( in the rules above: `AstFactor`)
/// - `$connecting_token_name` - the name of the first derivation of the second rule (in the rules above: `StarFactor`, `DivideFactor`, and `ModFactor)
/// - `$connecting_token_name_child` - the name of the second derivation of the second rule (in the rules above: `StarFactorAux`, `DivideFactorAux`, and `ModFactorAux`)
/// - `$connecting_token` - the connecting token in this derivation (in the rules above: `TokenKind::Star`, `TokenKind::Divide`, `TokenKind::Mod`)
macro_rules! syntax_recursive {
    ($name: ident, $name_aux: ident, $child_name: ident, $child_name_aux: ident, $child_type: ty, 
        $($connecting_token_name: ident, $connecting_token_name_child: ident, $connecting_token: expr),*) => {
        #[derive(Debug,PartialEq,Clone)]
        pub enum $name{
            $child_name(Box<$child_type>),
            $child_name_aux(Box<$child_type>, Box<$name_aux>),
        }

        #[derive(Debug,PartialEq,Clone)]
        pub enum $name_aux{
            $(
                $connecting_token_name(Box<$child_type>),
                $connecting_token_name_child(Box<$child_type>,Box<$name_aux>),
            )*
        }
    };
}




//----------------------AST----------------------------------------------------
#[derive(Debug,PartialEq,Clone)]
pub struct Ast{
    program: AstProgram,
    variables: HashMap<String,bool>,
}

impl Ast {
    pub fn new(program: AstProgram, variables: HashMap<String,bool>)->Self{
        Self{
            program,
            variables,
        }
    }

    pub fn get_program(&self)->&AstProgram{
        &self.program
    }

    pub fn get_variables(&self)->HashMap<String,bool>{
        self.variables.clone()
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
pub enum AstFunction {
    IdFunctionAux(String, Box<AstFunctionAux>),
}

#[derive(Debug,PartialEq,Clone)]
pub enum AstFunctionAux {
    Statement(Box<AstStatement>),
    StatementAux(Box<AstStatement>,Box<AstFunctionAux>),
}

//----------------------ASTStatement--------------------------------------------------
#[derive(Debug,PartialEq,Clone)]
pub enum AstStatement{
    ReturnExpression(Box<AstExpression>),
    Expression(Box<AstExpression>),
    Id(String),
    IdAssignment(String, Box<AstExpression>),
}

#[derive(Debug,PartialEq,Clone)]
pub enum AstExpression {
    IdExpression(String, Box<AstExpression>),
    LogicOrExp(Box<AstLogicOrExp>),
}

syntax_recursive!(AstLogicOrExp, AstLogicOrExpAux, LogicAndExp, LogicAndExpAux, AstLogicAndExp,
    LogicOrLogicAndExp, LogicOrLogicAndExpAux, TokenKind::LogicOr);

syntax_recursive!(AstLogicAndExp, AstLogicAndExpAux, BitOrExp, BitOrExpAux, AstBitOrExp,
    LogicAndBitOrExp, LogicAndBitOrExpAux, TokenKind::LogicAnd);

syntax_recursive!(AstBitOrExp, AstBitOrExpAux, BitXorExp, BitXorExpAux, AstBitXorExp,
    BitOrBitXorExp, BitOrBitXorExpAux, TokenKind::BitOr);

syntax_recursive!(AstBitXorExp, AstBitXorExpAux, BitAndExp, BitAndExpAux, AstBitAndExp,
    BitXorBitAndExp, BitXorBitAndExpAux, TokenKind::BitXor);

syntax_recursive!(AstBitAndExp, AstBitAndExpAux, EqualityExp, EqualityExpAux, AstEqualityExp,
    BitAndEqualityExp, BitAndEqualityExpAux, TokenKind::BitAnd);

syntax_recursive!(AstEqualityExp, AstEqualityExpAux, RelationalExp, RelationalExpAux, AstRelationalExp,
    NeqRelationalExp, NeqRelationalExpAux, TokenKind::Neq, EqRelationalExp, EqRelationalExpAux, TokenKind::Eq);

syntax_recursive!(AstRelationalExp, AstRelationalExpAux, BitShiftExp, BitShiftExpAux, AstBitShiftExp,
    LessBitShiftExp, LessBitShiftExpAux, TokenKind::Lesser, GreaterBitShiftExp, GreaterBitShiftExpAux, TokenKind::Greater, 
    LeqBitShiftExp, LeqBitShiftExpAux, TokenKind::Leq, GeqBitShiftExp, GeqBitShiftExpAux, TokenKind::Geq);

syntax_recursive!(AstBitShiftExp, AstBitShiftExpAux, AdditiveExp, AdditiveExpAux, AstAdditiveExp,
    BitShiftLeftAdditiveExp, BitShiftLeftAdditiveExpAux, TokenKind::BitShiftLeft, BitShiftRightAdditiveExp, BitShiftRighttAdditiveExpAux, TokenKind::BitShiftRight);

syntax_recursive!(AstAdditiveExp, AstAdditiveExpAux, Term, TermAux, AstTerm,
    PlusTerm, PlusTermAux, TokenKind::Plus, MinusTerm, MinusTermAux, TokenKind::Minus);

syntax_recursive!(AstTerm, AstTermAux, Factor, FactorAux, AstFactor, 
    StarFactor, StarFactorAux, TokenKind::Star, DivideFactor, DivideFactorAux, TokenKind::Divide, ModFactor, ModFactorAux, TokenKind::Mod);
//----------------------ASTFactor--------------------------------------------------

#[derive(Debug,PartialEq,Clone)]
pub enum AstFactor{
    Expression(Box<AstExpression>),
    UnaryOpFactor(AstUnaryOp,Box<AstFactor>),
    Int(usize),
    Id(String),
}





//----------------------ASTUnaryOp--------------------------------------------------
#[derive(Debug,PartialEq,Clone,Copy)]
pub enum AstUnaryOp{
    Minus,
    Complement,
    LogicNegation,
}