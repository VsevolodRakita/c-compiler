use lexer::{Keyword, Token, TokenKind};

use std::collections::HashMap;

use super::lexer::Lexer;
use super::*;

#[derive(Debug,PartialEq,Clone)]
pub struct Parser{
    tokens: Vec<Token>,
    current_pos: usize,
    variables: HashMap<String,bool>
}

/// Macro for parsing recursive rules similar to
/// <term> ::= <factor> || <factor> <term-aux>
/// <term-aux> ::=   ("*" | "/"| "%") <factor> || ("*" | "/"| "%") <factor> <term-aux>
/// 
/// # Arguments
/// - `$method_name` - the name of the method to be created (in the rule above: `parse_term`).
/// - `$aux_method_name` - the name of the auxiliary method to be created (in the rule above: `parse_term_aux`).
/// - `$return_type` - the type of ast node to be returned (in the rule above: `AstTerm`).
/// - `$aux_return_type` - the type of auxiliary ast node to be returned (in the rule above: `AstTermAux`).
/// - `$variant1` - the name of the first derivation of the first rule (in the rules above: `Factor`)
/// - `$variant2` - the name of the second derivation of the first rule (in the rules above: `FactorAux`)
/// - `$child_method` - the method used to parse the inner node (in the rule above: `parse_factor`).
/// - `$connecting_token` - the connecting token in this derivation (in the rules above: `TokenKind::Star`, `TokenKind::Divide`, `TokenKind::Mod`)
/// - `$variant_no_rec` - the name of the first derivation of the second rule (in the rules above: `StarFactor`, `DivideFactor`, and `ModFactor)
/// - `$variant_rec` - the name of the second derivation of the second rule (in the rules above: `StarFactorAux`, `DivideFactorAux`, and `ModFactorAux`)
macro_rules! parse_recursive {
    ($method_name: ident, $aux_method_name: ident, $return_type: ty, $aux_return_type: ty, $variant1: expr, $variant2: expr, $child_method: ident, 
        $($variant_no_rec: expr, $variant_rec: expr, $connecting_token: expr),*) => {
        fn $method_name(&mut self)->Option<$return_type>{
            if let Some(x)=self.$child_method(){
                if let Some(aux)=self.$aux_method_name(){
                    return Some($variant2(Box::new(x),Box::new(aux)));
                }
                return Some($variant1(Box::new(x)));
            }
            None
        }

        fn $aux_method_name(&mut self)->Option<$aux_return_type>{
            let original_position=self.current_pos;
            if self.parser_finished() || self.tokens[self.current_pos].is_identifier(){
                return None;
            }
            $(
                if self.tokens[self.current_pos].get_kind()==$connecting_token{
                        self.current_pos+=1;
                        if let Some(x)=self.$child_method(){
                            if let Some(aux)=self.$aux_method_name(){
                                return  Some($variant_rec(Box::new(x),Box::new(aux)));
                            }
                            return Some($variant_no_rec(Box::new(x)));
                        }
                        self.current_pos=original_position;
                        return None;
                }
            )*
            None
        }
    }
}

impl Parser {
    pub fn new(lex: Lexer)->Self{
        Self{
            tokens: lex.collect(),
            current_pos: 0,
            variables: HashMap::new(),
        }
    }

    pub fn get_ast(&mut self)->Option<Ast>{
        match self.parse_program() {
            Some(program)=> return Some(Ast::new(program, self.variables.clone())),
            None => return None,            
        }
    }

    fn parser_finished(&self)->bool{
        self.current_pos>=self.tokens.len()
    }

    fn parse_program(&mut self)->Option<AstProgram>{
        match self.parse_function() {
            Some(function)=> Some(AstProgram::new(function)),
            None => None,
        }
    }

    fn parse_function(&mut self)->Option<AstFunction>{
        let original_index=self.current_pos;
        if self.current_pos+4>=self.tokens.len()||self.tokens[self.current_pos].get_kind()!=TokenKind::Keyword(Keyword::Int) || 
            !self.tokens[self.current_pos+1].is_identifier() || self.tokens[self.current_pos+2].get_kind()!=TokenKind::OpenParenthesis ||
            self.tokens[self.current_pos+3].get_kind()!=TokenKind::CloseParenthesis || self.tokens[self.current_pos+4].get_kind()!=TokenKind::OpenBrace{
                return None;
        }
        let func_name=self.tokens[self.current_pos+1].get_identifier().unwrap();
        self.current_pos+=5;
        match self.parse_function_aux() {
            Some(function_aux)=> {
                if !self.parser_finished() && !self.tokens[self.current_pos].is_identifier() && self.tokens[self.current_pos].get_kind()==TokenKind::CloseBrace{
                    return Some(AstFunction::IdFunctionAux(func_name, Box::new(function_aux)));
                }
                else{
                    self.current_pos=original_index;
                    return None;
                }
            }
            None => {self.current_pos=original_index; return None;}
        }
    }

    fn parse_function_aux(&mut self)->Option<AstFunctionAux>{
        if let Some(ast_block_item)=self.parse_block_item(){
            if let Some(ast_function_aux) =self.parse_function_aux(){
                return Some(AstFunctionAux::BlockItemAux(Box::new(ast_block_item), Box::new(ast_function_aux)));
            }
            return Some(AstFunctionAux::BlockItem(Box::new(ast_block_item)));
        }
        None
    }

    fn parse_block_item(&mut self)->Option<AstBlockItem>{
        if let Some(ast_decleration) = self.parse_decleration(){
            return Some(AstBlockItem::Declaration(Box::new(ast_decleration)));
        }
        if let Some(ast_statement) = self.parse_statement(){
            return Some(AstBlockItem::Statement(Box::new(ast_statement)));
        }
        None
    }

    fn parse_decleration(&mut self)->Option<AstDeclaration>{
        let original_index=self.current_pos;
        if self.parser_finished() || self.tokens[self.current_pos].is_identifier(){
            return None;
        }
        if TokenKind::Keyword(Keyword::Int)  ==  self.tokens[self.current_pos].get_kind(){
            self.current_pos+=1;
            if !self.parser_finished() && self.tokens[self.current_pos].is_identifier(){
                let s = self.tokens[self.current_pos].get_identifier().unwrap();
                self.current_pos+=1;
                if self.parser_finished(){
                    self.current_pos=original_index;
                    return None;
                }
                match self.tokens[self.current_pos].get_kind(){
                    TokenKind::Semicolon => {
                        self.current_pos+=1;
                        if self.variables.contains_key(&s){
                            println!("{}", "Compilation Failed! Double decleration of variable ".to_string()+&s);
                            return None;
                        }
                        self.variables.insert(s.clone(), false);
                        return Some(AstDeclaration::Id(s));
                    }
                    TokenKind::Assignment =>{
                        self.current_pos+=1;
                        if let Some(ast_exp)=self.parse_expression(){
                            if !self.parser_finished() && self.tokens[self.current_pos].get_kind()==TokenKind::Semicolon{
                                self.current_pos+=1;
                                if self.variables.contains_key(&s){
                                    println!("{}", "Compilation Failed! Double decleration of variable ".to_string()+&s);
                                    return None;
                                }
                                self.variables.insert(s.clone(), true);
                                return Some(AstDeclaration::IdAssignment(s, Box::new(ast_exp)));
                            }
                        }
                            
                    }
                    _=> {self.current_pos=original_index; return None;}
                }
            }
            self.current_pos=original_index;
            return None;
        }
        None
    }

    fn parse_statement(&mut self)->Option<AstStatement>{
        let original_index=self.current_pos;
        if self.parser_finished(){
            return None;
        }
        if let Some(ast_expression)=self.parse_expression(){
            if !self.parser_finished() && self.tokens[self.current_pos].get_kind()==TokenKind::Semicolon{
                self.current_pos+=1;
                return Some(AstStatement::Expression(Box::new(ast_expression)));
            }
            self.current_pos=original_index;
            return None;
        }
        match self.tokens[self.current_pos].get_kind(){
            TokenKind::Keyword(Keyword::Return) => {
                self.current_pos+=1;
                if let Some(ast_expression)=self.parse_expression(){
                    if !self.parser_finished() && !self.tokens[self.current_pos].is_identifier() && self.tokens[self.current_pos].get_kind()==TokenKind::Semicolon{
                        self.current_pos+=1;
                        return Some(AstStatement::ReturnExpression(Box::new(ast_expression)));
                    }
                }
                self.current_pos=original_index;
                return None;
            },
            TokenKind::Keyword(Keyword::If) =>{
                self.current_pos+=1;
                if !self.parser_finished() && !self.tokens[self.current_pos].is_identifier() && self.tokens[self.current_pos].get_kind()==TokenKind::OpenParenthesis{
                    self.current_pos+=1;
                    if let Some(ast_expression) = self.parse_expression(){
                        if !self.parser_finished() && !self.tokens[self.current_pos].is_identifier() && self.tokens[self.current_pos].get_kind()==TokenKind::CloseParenthesis{
                            self.current_pos+=1;
                            if let Some(ast_statement) = self.parse_statement() {
                                let new_pos=self.current_pos;
                                if !self.parser_finished() && !self.tokens[self.current_pos].is_identifier() && 
                                        self.tokens[self.current_pos].get_kind()==TokenKind::Keyword(Keyword::Else){
                                    self.current_pos+=1;
                                    if let Some(ast_statement2) = self.parse_statement(){
                                        return Some(AstStatement::IfExpressionStatementElseStatement(Box::new(ast_expression), Box::new(ast_statement), Box::new(ast_statement2)));
                                    }
                                }
                                self.current_pos=new_pos;
                                return Some(AstStatement::IfExpressionStatement(Box::new(ast_expression), Box::new(ast_statement)));
                            }
                        }
                    }
                }
                self.current_pos=original_index;
                return None;
            },
            _ => return None
        }
    }

    fn parse_expression(&mut self)->Option<AstExpression>{
        let original_position=self.current_pos;
        if self.parser_finished(){
            return None;
        }
        if let TokenKind::Identifier(s)= self.tokens[self.current_pos].get_kind() {
            self.current_pos+=1;
            if self.parser_finished(){
                self.current_pos=original_position;
                return None;
            }
            if self.tokens[self.current_pos].get_kind()==TokenKind::Assignment{
                self.current_pos+=1;
                if self.parser_finished(){
                    self.current_pos=original_position;
                    return None;
                }
                if let Some(exp) = self.parse_expression(){
                    if self.variables.insert(s.clone(), true).is_none(){
                        println!("{}", "Assignment to variable ".to_string() +&s+" before decleration.");
                        return None;
                    }
                    return Some(AstExpression::IdExpression(s, Box::new(exp)));
                }
            }
            self.current_pos=original_position;
        }
        match  self.parse_conditional_exp(){
            Some(ast_conditional_exp) => Some(AstExpression::ConditionalExp(Box::new(ast_conditional_exp))),
            None => None,
        }
    }

    fn parse_conditional_exp(&mut self)->Option<AstConditionalExp>{
        if self.parser_finished(){
            return None;
        }
        if let Some(ast_logic_or_exp) = self.parse_logical_or_exp(){
            if !self.parser_finished() && !self.tokens[self.current_pos].is_identifier() && 
                    self.tokens[self.current_pos].get_kind() == TokenKind::QuestionMark{
                let new_pos=self.current_pos;
                self.current_pos+=1;
                if let Some(ast_expression) = self.parse_expression(){
                    if self.tokens[self.current_pos].get_kind() == TokenKind::Colon{
                        self.current_pos+=1;
                        if let Some(ast_conditional_exp) = self.parse_conditional_exp(){
                            return Some(AstConditionalExp::LogicOrExpExpConditionalExp(Box::new(ast_logic_or_exp), Box::new(ast_expression), 
                                Box::new(ast_conditional_exp)));
                        }
                    }
                }
                self.current_pos=new_pos;
            }
            return Some(AstConditionalExp::LogicOrExp(Box::new(ast_logic_or_exp)));
        }
        None
    }

    parse_recursive!(parse_logical_or_exp, parse_logical_or_exp_aux, AstLogicOrExp, AstLogicOrExpAux, AstLogicOrExp::LogicAndExp, AstLogicOrExp::LogicAndExpAux, parse_logical_and_exp,
        AstLogicOrExpAux::LogicOrLogicAndExp, AstLogicOrExpAux::LogicOrLogicAndExpAux, TokenKind::LogicOr);

    parse_recursive!(parse_logical_and_exp, parse_logical_and_exp_aux, AstLogicAndExp, AstLogicAndExpAux, AstLogicAndExp::BitOrExp, AstLogicAndExp::BitOrExpAux, parse_bit_or_exp,
        AstLogicAndExpAux::LogicAndBitOrExp, AstLogicAndExpAux::LogicAndBitOrExpAux, TokenKind::LogicAnd);

    parse_recursive!(parse_bit_or_exp, parse_bit_or_exp_aux, AstBitOrExp, AstBitOrExpAux, AstBitOrExp::BitXorExp, AstBitOrExp::BitXorExpAux, parse_bit_xor_exp,
        AstBitOrExpAux::BitOrBitXorExp, AstBitOrExpAux::BitOrBitXorExpAux, TokenKind::BitOr);

    parse_recursive!(parse_bit_xor_exp, parse_bit_xor_exp_aux, AstBitXorExp, AstBitXorExpAux, AstBitXorExp::BitAndExp, AstBitXorExp::BitAndExpAux, parse_bit_and_exp,
        AstBitXorExpAux::BitXorBitAndExp, AstBitXorExpAux::BitXorBitAndExpAux, TokenKind::BitXor);

    parse_recursive!(parse_bit_and_exp, parse_bit_and_exp_aux, AstBitAndExp, AstBitAndExpAux, AstBitAndExp::EqualityExp, AstBitAndExp::EqualityExpAux, parse_equality_exp,
        AstBitAndExpAux::BitAndEqualityExp, AstBitAndExpAux::BitAndEqualityExpAux, TokenKind::BitAnd);

    parse_recursive!(parse_equality_exp, parse_equality_exp_aux, AstEqualityExp, AstEqualityExpAux, 
        AstEqualityExp::RelationalExp, AstEqualityExp::RelationalExpAux, parse_relational_exp,
        AstEqualityExpAux::NeqRelationalExp, AstEqualityExpAux::NeqRelationalExpAux, TokenKind::Neq, 
        AstEqualityExpAux::EqRelationalExp, AstEqualityExpAux::EqRelationalExpAux, TokenKind::Eq);

    parse_recursive!(parse_relational_exp, parse_relational_exp_aux, AstRelationalExp, AstRelationalExpAux, 
        AstRelationalExp::BitShiftExp, AstRelationalExp::BitShiftExpAux, parse_bit_shift_exp,
        AstRelationalExpAux::LessBitShiftExp, AstRelationalExpAux::LessBitShiftExpAux, TokenKind::Lesser, 
        AstRelationalExpAux::GreaterBitShiftExp, AstRelationalExpAux::GreaterBitShiftExpAux, TokenKind::Greater, 
        AstRelationalExpAux::LeqBitShiftExp, AstRelationalExpAux::LeqBitShiftExpAux, TokenKind::Leq, 
        AstRelationalExpAux::GeqBitShiftExp, AstRelationalExpAux::GeqBitShiftExpAux, TokenKind::Geq);

    parse_recursive!(parse_bit_shift_exp, parse_bit_shift_exp_aux, AstBitShiftExp,AstBitShiftExpAux, AstBitShiftExp::AdditiveExp, AstBitShiftExp::AdditiveExpAux, parse_additive_exp,
        AstBitShiftExpAux::BitShiftLeftAdditiveExp, AstBitShiftExpAux::BitShiftLeftAdditiveExpAux, TokenKind::BitShiftLeft, 
        AstBitShiftExpAux::BitShiftRightAdditiveExp, AstBitShiftExpAux::BitShiftRighttAdditiveExpAux, TokenKind::BitShiftRight);

    parse_recursive!(parse_additive_exp, parse_additive_exp_aux, AstAdditiveExp, AstAdditiveExpAux, AstAdditiveExp::Term, AstAdditiveExp::TermAux, parse_term, 
        AstAdditiveExpAux::PlusTerm, AstAdditiveExpAux::PlusTermAux, TokenKind::Plus, AstAdditiveExpAux::MinusTerm, AstAdditiveExpAux::MinusTermAux, TokenKind::Minus);
    

    parse_recursive!(parse_term, parse_term_aux, AstTerm, AstTermAux, AstTerm::Factor, AstTerm::FactorAux, parse_factor, 
        AstTermAux::StarFactor, AstTermAux::StarFactorAux, TokenKind::Star, AstTermAux::DivideFactor, AstTermAux::DivideFactorAux, TokenKind::Division, 
        AstTermAux::ModFactor, AstTermAux::ModFactorAux, TokenKind::Modulo);

    fn parse_factor(&mut self)->Option<AstFactor>{
        let original_position=self.current_pos;
        if self.parser_finished(){
            return None;
        }
        if let Some(op)=self.parse_unaryop(){
            if let Some(fact)=self.parse_factor(){
                return Some(AstFactor::UnaryOpFactor(op, Box::new(fact)));
            }
        }
        match self.tokens[self.current_pos].get_kind(){
            TokenKind::OpenParenthesis =>{
                self.current_pos+=1;
                if let Some(exp)=self.parse_expression(){
                    if self.tokens[self.current_pos].get_kind()==TokenKind::CloseParenthesis{
                        self.current_pos+=1;
                        return Some(AstFactor::Expression(Box::new(exp)));
                    }
                    else{
                        self.current_pos=original_position;
                        return None;
                    }
                }
                else{
                    self.current_pos=original_position;
                    return None;
                }
            }
            TokenKind::Number(x) => {self.current_pos+=1; return Some(AstFactor::Int(x));}
            TokenKind::Identifier(s) => {
                match self.variables.insert(s.clone(), true) {
                    Some(b) => {
                        if b{
                            self.current_pos+=1;
                            return Some(AstFactor::Id(s));
                        }
                        println!("{}", "Access to uninitialized variable ".to_string() +&s);
                        return None;
                    }
                    None => {
                        println!("{}", "Access to undeclared variable ".to_string() +&s);
                        return None;
                    }
                }
                 
            }
            _ => return None,
        }
    }

    fn parse_unaryop(&mut self)->Option<AstUnaryOp>{
        if self.parser_finished() || self.tokens[self.current_pos].is_identifier(){
            return None;
        }
        match self.tokens[self.current_pos].get_kind(){
            TokenKind::Minus => {self.current_pos+=1; return Some(AstUnaryOp::Minus)},
            TokenKind::Complement => {self.current_pos+=1; return Some(AstUnaryOp::Complement)},
            TokenKind::LogicNegation => {self.current_pos+=1; return Some(AstUnaryOp::LogicNegation)},
            _=> return None
        }
    }
    
}





#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser1(){
        let input="int main(){return 7;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }

    #[test]
    fn parser2(){
        let input="int blah(){return 9;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }

    #[test]
    fn parser3(){
        let input="int blah(){return 9}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_none());
    }

    #[test]
    fn parser4(){
        let input="int blah(){return -!~~9;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }

    #[test]
    fn parser5(){
        let input="int blah(){return -!~~;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_none());
    }
    #[test]
    fn parser6(){
        let input="int blah(){return 1+2+5;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }

    #[test]
    fn parser7(){
        let input="int blah(){return 1+-2+5;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }

    #[test]
    fn parser8(){
        let input="int blah(){return (5-!3)/5;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }

    #[test]
    fn parser9(){
        let input="int blah(){return 1*7;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }

    #[test]
    fn parser10(){
        let input="int blah(){return 1+-2*(2/4+1)-~4;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }

    #[test]
    fn parser11(){
        let input="int blah(){return 1+-2*(2/4++1)-~4;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_none());
    }

    #[test]
    fn parser12(){
        let input="int blah(){return 1!=2||3=2&&1==1>2+3+2*2*(4+2)*!4;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_none());
    }

    #[test]
    fn parser13(){
        let input="int blah(){return 1!=2||3==2&&1==1>2+3+2*2*(4+2)*!4;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }

    #[test]
    fn parser14(){
        let input="int blah(){return 45%3&3|2^5;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }

    #[test]
    fn parser15(){
        let input="int blah(){return 5^4%3^8^8^8&9;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }
    #[test]
    fn parser16(){
        let input="int blah(){return 4<<3>>3>>3<<2>>1<1>3<6|3&5;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
    }
    #[test]
    fn parser17(){
        let input="int main(){int a; return 1+2;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
        let mut ht=HashMap::new();
        ht.insert("a".to_string(), false);
        //ht.insert("b".to_string(), true);
        assert_eq!(ht, ast.unwrap().get_variables());
    }

    #[test]
    fn parser18(){
        let input="int main(){int b=3; return 1+2;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
        let mut ht=HashMap::new();
        ht.insert("b".to_string(), true);
        assert_eq!(ht, ast.unwrap().get_variables());
    }

    #[test]
    fn parser19(){
        let input="int main(){int b=3; return b+2;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
        let mut ht=HashMap::new();
        ht.insert("b".to_string(), true);
        assert_eq!(ht, ast.unwrap().get_variables());
    }
    #[test]
    fn parser20(){
        let input="int main(){int b=3;int a; return b+2;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
        let mut ht=HashMap::new();
        ht.insert("b".to_string(), true);
        ht.insert("a".to_string(), false);
        assert_eq!(ht, ast.unwrap().get_variables());
    }

    #[test]
    fn parser21(){
        let input="int main(){int b=3;int a; return a+2;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_none());
    }

    #[test]
    fn parser22(){
        let input="int main(){int b;int a=5;b=9; return b+a;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
        let mut ht=HashMap::new();
        ht.insert("b".to_string(), true);
        ht.insert("a".to_string(), true);
        assert_eq!(ht, ast.unwrap().get_variables());
    }

    #[test]
    fn parser23(){
        let input="int main(){int b;int a=5;b=9; b+a;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast();
        //println!("{:?}",&ast);
        assert!(ast.is_some());
        let mut ht=HashMap::new();
        ht.insert("b".to_string(), true);
        ht.insert("a".to_string(), true);
        assert_eq!(ht, ast.unwrap().get_variables());
    }
}