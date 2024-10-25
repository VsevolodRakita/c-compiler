use lexer::{Keyword, Token, TokenKind};

use super::lexer::Lexer;
use super::*;

#[derive(Debug,PartialEq,Clone)]
pub struct Parser{
    tokens: Vec<Token>,
    current_pos: usize,
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
        }
    }

    pub fn get_ast(&mut self)->Option<Ast>{
        match self.parse_program() {
            Some(program)=> return Some(Ast::new(program)),
            None => return None,            
        }
    }

    fn parser_finished(&self)->bool{
        self.current_pos>=self.tokens.len()
    }

    fn parse_program(&mut self)->Option<AstProgram>{
        match self.parse_function() {
            Some(function)=> return Some(AstProgram::new(function)),
            None => return None,
        }
    }

    fn parse_function(&mut self)->Option<AstFunction>{
        let original_index=self.current_pos;
        if self.current_pos+4>=self.tokens.len()||self.tokens[self.current_pos].get_kind()!=TokenKind::Keyword(Keyword::Int) || 
            !self.tokens[self.current_pos+1].is_identifier() || self.tokens[self.current_pos+2].get_kind()!=TokenKind::OpenParenthesis ||
            self.tokens[self.current_pos+3].get_kind()!=TokenKind::CloseParenthesis || self.tokens[self.current_pos+4].get_kind()!=TokenKind::OpenBrace{
                return None;
        }
        self.current_pos+=5;
        match self.parse_statement() {
            Some(statement)=> {
                if !self.parser_finished() && !self.tokens[self.current_pos].is_identifier() && self.tokens[self.current_pos].get_kind()==TokenKind::CloseBrace{
                    match self.tokens[original_index+1].get_kind(){
                        TokenKind::Identifier(s) =>{self.current_pos+=1; return Some(AstFunction::new(s, statement));}
                        _ => {assert!(false); return None},
                    }
                }
                else{
                    self.current_pos=original_index;
                    return None;
                }
            }
            None => {self.current_pos=original_index; return None;}
        }
    }

    fn parse_statement(&mut self)->Option<AstStatement>{
        let original_index=self.current_pos;
        if self.parser_finished() || self.tokens[self.current_pos].is_identifier(){
            return None;
        }
        match self.tokens[self.current_pos].get_kind(){
            TokenKind::Keyword(Keyword::Return) => self.current_pos+=1,
            _ => return None
        }
        match self.parse_expression() {
            Some(ast_exp)=>{
                if self.parser_finished() || self.tokens[self.current_pos].is_identifier(){
                    self.current_pos=original_index;
                    return None;
                }
                match self.tokens[self.current_pos].get_kind() {
                    TokenKind::Semicolon => {self.current_pos+=1; return Some(AstStatement::new(ast_exp));}
                    _ => {self.current_pos=original_index; return None;}
                }
            }
            None => {self.current_pos-=1; return None}
        }
    }

    parse_recursive!(parse_expression, parse_expression_aux, AstExpression, AstExpressionAux, AstExpression::LogicAndExp, AstExpression::LogicAndExpAux, parse_logical_and_exp,
        AstExpressionAux::LogicOrLogicAndExp, AstExpressionAux::LogicOrLogicAndExpAux, TokenKind::LogicOr);

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
        if self.parser_finished() || self.tokens[self.current_pos].is_identifier(){
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
}