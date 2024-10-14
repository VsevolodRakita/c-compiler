use lexer::{Keyword, Token, TokenKind};

use super::lexer::Lexer;
use super::*;

#[derive(Debug,PartialEq,Clone)]
pub struct Parser{
    tokens: Vec<Token>,
    current_pos: usize,
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

    fn parse_expression(&mut self)->Option<AstExpression>{
        let original_position=self.current_pos;
        if self.parser_finished() || self.tokens[self.current_pos].is_identifier(){
            return None;
        }
        if let Some(unary)=self.parse_unaryop(){
            if let Some(exp)=self.parse_expression(){
                return Some(AstExpression::UnaryOpExpression(AstExpressionUnaryOpExpression::new(unary, exp)));
            }
            else {
                self.current_pos=original_position;
                return None;
            }
        }
        else{
            match self.tokens[self.current_pos].get_kind() {
                TokenKind::Number(x) => {
                    self.current_pos+=1; 
                    return Some(AstExpression::Int(AstExpressionInt::new(x)));
                    },
                _ => {self.current_pos=original_position; return None;}
            }

        }
    }

    fn parse_unaryop(&mut self)->Option<AstUnaryOp>{
        if self.parser_finished() || self.tokens[self.current_pos].is_identifier(){
            return None;
        }
        match self.tokens[self.current_pos].get_kind(){
            TokenKind::Minus => {self.current_pos+=1; return Some(AstUnaryOp::new(UnaryOpType::Minus))},
            TokenKind::Complement => {self.current_pos+=1; return Some(AstUnaryOp::new(UnaryOpType::Complement))},
            TokenKind::LogicNegation => {self.current_pos+=1; return Some(AstUnaryOp::new(UnaryOpType::LogicNegation))},
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
}