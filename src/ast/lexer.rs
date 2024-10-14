use std::{iter::Peekable, str::Chars};
use std::collections::HashSet;
use std::collections::HashMap;

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum Keyword{
    Int,
    Return,
}

#[derive(Debug,PartialEq,Clone)]
pub enum TokenKind{
    OpenBrace,//
    CloseBrace,//
    OpenParenthesis,//
    CloseParenthesis,//
    Semicolon,//
    Keyword(Keyword),
    Number(usize),
    Identifier(String),
    Plus,//
    Minus,//
    Complement,
    LogicNegation,
    Bad,
}

#[derive(Debug,PartialEq,Clone)]
pub struct Token{
    kind: TokenKind,

}

impl Token{
    pub fn new(kind: TokenKind)-> Self{
        Token{
            kind
        }
    }

    pub fn is_identifier(&self)->bool{
        std::mem::discriminant(&self.kind)==std::mem::discriminant(&TokenKind::Identifier("".to_string()))
    }

    pub fn get_kind(&self)->TokenKind{
        self.kind.clone()
    }
}

pub struct Lexer<'a>{
    chars: Peekable<Chars<'a>>,
}

impl <'a>Lexer<'a>{
    pub fn new(input: &'a str)-> Lexer{
        Lexer{
            chars: input.chars().peekable(),
        }
    }

}

impl<'a> Iterator for Lexer<'a>{
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        while self.chars.peek().is_none() || self.chars.peek().unwrap().is_whitespace(){
            if self.chars.peek().is_none(){
                return None;
            }
            self.chars.next();
        }
        let mut current_char=self.chars.peek().unwrap();
        match *current_char {
            '{' => {self.chars.next(); return Some(Token::new(TokenKind::OpenBrace))},
            '}' => {self.chars.next(); return Some(Token::new(TokenKind::CloseBrace))},
            '(' => {self.chars.next(); return Some(Token::new(TokenKind::OpenParenthesis))},
            ')' => {self.chars.next(); return Some(Token::new(TokenKind::CloseParenthesis))},
            ';' => {self.chars.next(); return Some(Token::new(TokenKind::Semicolon))},
            '+' => {self.chars.next(); return Some(Token::new(TokenKind::Plus))},
            '-' => {self.chars.next(); return Some(Token::new(TokenKind::Minus))},
            '~' => {self.chars.next(); return Some(Token::new(TokenKind::Complement))},
            '!' => {self.chars.next(); return Some(Token::new(TokenKind::LogicNegation))},
            _ => {
                let symbols=HashSet::from(['{','}','(',')', ';','+','-']);
                let keywords:HashMap<_,_> = vec![("int".to_string(),Keyword::Int),("return".to_string(), Keyword::Return)].iter().cloned().collect();
                if current_char.is_numeric(){
                    let mut bad= false;
                    let mut num:usize =0;
                    while !current_char.is_whitespace() && !symbols.contains(&current_char) {
                        if current_char.is_numeric(){
                            num*=10;
                            num+=current_char.to_digit(10).unwrap() as usize;
                        }
                        else{
                            bad=true;
                        }
                        self.chars.next();
                        if self.chars.peek().is_none(){
                            break;
                        }
                        current_char=self.chars.peek().unwrap();
                    }
                    if bad{
                        return Some(Token::new(TokenKind::Bad));
                    }
                    return Some(Token::new(TokenKind::Number(num)));
                }
                else{
                    let mut bad=false;
                    let mut s=String::new();
                    while !current_char.is_whitespace() && !symbols.contains(&current_char) {
                        if current_char.is_ascii_alphanumeric(){
                            s.push(*current_char);
                        }
                        else {
                            bad=true;
                        }
                        self.chars.next();
                        if self.chars.peek().is_none(){
                            break;
                        }
                        current_char=self.chars.peek().unwrap();
                    }
                    if bad{
                        return Some(Token::new(TokenKind::Bad));
                    }
                    if keywords.contains_key(&s){
                        return  Some(Token::new(TokenKind::Keyword(keywords[&s])));
                    }
                    return Some(Token::new(TokenKind::Identifier(s)));
                }
            }
        };
    }

}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer1() {
        let input="}(     ;".to_string();
        let mut lex=Lexer::new(&input);
        assert_eq!(lex.next(),Some(Token::new(TokenKind::CloseBrace)));
        assert_eq!(lex.next(),Some(Token::new(TokenKind::OpenParenthesis)));
        assert_eq!(lex.next(),Some(Token::new(TokenKind::Semicolon)));
        assert_eq!(lex.next(),None);
    }

    #[test]
    fn lexer2() {
        let input="}return(     ;".to_string();
        let mut lex=Lexer::new(&input);
        assert_eq!(lex.next(),Some(Token::new(TokenKind::CloseBrace)));
        assert_eq!(lex.next(),Some(Token::new(TokenKind::Keyword(Keyword::Return))));
        assert_eq!(lex.next(),Some(Token::new(TokenKind::OpenParenthesis)));
        assert_eq!(lex.next(),Some(Token::new(TokenKind::Semicolon)));
        assert_eq!(lex.next(),None);
    }

    #[test]
    fn lexer3() {
        let input="
           
           }  returnint(int   }{}      ;".to_string();
        let lex=Lexer::new(&input);
        let v:Vec<Token>=lex.collect();
        assert_eq!(v,[Token::new(TokenKind::CloseBrace),Token::new(TokenKind::Identifier("returnint".to_string())),Token::new(TokenKind::OpenParenthesis),
            Token::new(TokenKind::Keyword(Keyword::Int)), Token::new(TokenKind::CloseBrace), Token::new(TokenKind::OpenBrace),
            Token::new(TokenKind::CloseBrace),Token::new(TokenKind::Semicolon)]);
    }

    #[test]
    fn lexer4() {
        let input="-!~!~~5;".to_string();
        let lex=Lexer::new(&input);
        let v:Vec<Token>=lex.collect();
        assert_eq!(v,[Token::new(TokenKind::Minus),Token::new(TokenKind::LogicNegation),Token::new(TokenKind::Complement),Token::new(TokenKind::LogicNegation),
            Token::new(TokenKind::Complement),Token::new(TokenKind::Complement),Token::new(TokenKind::Number(5)),Token::new(TokenKind::Semicolon)]);
    }
}