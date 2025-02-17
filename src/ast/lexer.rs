/// Implements a lexer.
/// The struct `Lexer` gets a string representing a .c file, and transforms it into a stream of `Token` for use by the `Parser`
/// (see ast/parse.rs for details).

use std::{iter::Peekable, str::Chars};
use std::collections::HashSet;
use std::collections::HashMap;

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum Keyword{
    Int,
    Return,
    If,
    Else,
    For,
    Do,
    While,
    Break,
    Continue,
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
    Minus,//
    Complement,//
    LogicNegation,//
    Plus,//
    Star,//
    Division,//
    LogicAnd,//
    LogicOr,//
    Eq,//
    Neq,//
    Lesser,//
    Leq,//
    Greater,//
    Geq,//
    Assignment,//
    BitAnd,//
    BitOr,//
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    Modulo,
    Colon,
    QuestionMark,
    Comma,
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

    pub fn get_identifier(&self)->Option<String>{
        match &self.kind {
            TokenKind::Identifier(s) => Some(s.clone()),
            _ => None,
        }
    }

    pub fn get_kind(&self)->TokenKind{
        self.kind.clone()
    }
}

pub struct Lexer<'a>{
    chars: Peekable<Chars<'a>>,
    symbols: HashSet<char>,
    keywords: HashMap<String,Keyword>,
}

impl <'a>Lexer<'a>{
    pub fn new(input: &'a str)-> Lexer{
        Lexer{
            chars: input.chars().peekable(),
            symbols: HashSet::from(['{','}','(',')', ';','-','~','!','+','*','/','&','|','=','<','>','^','%','?',':', ',']),
            keywords: vec![
                ("int".to_string(),Keyword::Int),("return".to_string(), Keyword::Return), ("if".to_string(), Keyword::If),
                ("else".to_string(), Keyword::Else), ("for".to_string(), Keyword::For),("do".to_string(), Keyword::Do),
                ("while".to_string(), Keyword::While),("break".to_string(), Keyword::Break),("continue".to_string(), Keyword::Continue)
                ].iter().cloned().collect(),
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
            '-' => {self.chars.next(); return Some(Token::new(TokenKind::Minus))},
            '~' => {self.chars.next(); return Some(Token::new(TokenKind::Complement))},
            '!' => {
                self.chars.next();
                if !self.chars.peek().is_none() && self.chars.peek().unwrap()==&'='{
                    self.chars.next();
                    return Some(Token::new(TokenKind::Neq));
                }
                return Some(Token::new(TokenKind::LogicNegation));
            }
            '+' => {self.chars.next(); return Some(Token::new(TokenKind::Plus))},
            '*' => {self.chars.next(); return Some(Token::new(TokenKind::Star))},
            '/' => {self.chars.next(); return Some(Token::new(TokenKind::Division))},
            '&' => {
                self.chars.next();
                if !self.chars.peek().is_none() && self.chars.peek().unwrap()==&'&'{
                    self.chars.next();
                    return Some(Token::new(TokenKind::LogicAnd));
                }
                return Some(Token::new(TokenKind::BitAnd));
            }
            '|' => {
                self.chars.next();
                if !self.chars.peek().is_none() && self.chars.peek().unwrap()==&'|'{
                    self.chars.next();
                    return Some(Token::new(TokenKind::LogicOr));
                }
                return Some(Token::new(TokenKind::BitOr));
            }
            '=' => {
                self.chars.next();
                if !self.chars.peek().is_none() && self.chars.peek().unwrap()==&'='{
                    self.chars.next();
                    return Some(Token::new(TokenKind::Eq));
                }
                return Some(Token::new(TokenKind::Assignment));
            }
            '<' => {
                self.chars.next();
                if !self.chars.peek().is_none() && self.chars.peek().unwrap()==&'='{
                    self.chars.next();
                    return Some(Token::new(TokenKind::Leq));
                }
                if !self.chars.peek().is_none() && self.chars.peek().unwrap()==&'<'{
                    self.chars.next();
                    return Some(Token::new(TokenKind::BitShiftLeft));
                }
                return Some(Token::new(TokenKind::Lesser));
            }
            '>' => {
                self.chars.next();
                if !self.chars.peek().is_none() && self.chars.peek().unwrap()==&'='{
                    self.chars.next();
                    return Some(Token::new(TokenKind::Geq));
                }
                if !self.chars.peek().is_none() && self.chars.peek().unwrap()==&'>'{
                    self.chars.next();
                    return Some(Token::new(TokenKind::BitShiftRight));
                }
                return Some(Token::new(TokenKind::Greater));
            }
            '^' => {self.chars.next(); return Some(Token::new(TokenKind::BitXor))},
            '%' => {self.chars.next(); return Some(Token::new(TokenKind::Modulo))},
            ':' => {self.chars.next(); return Some(Token::new(TokenKind::Colon))},
            '?' => {self.chars.next(); return Some(Token::new(TokenKind::QuestionMark))},
            ',' => {self.chars.next(); return Some(Token::new(TokenKind::Comma))},
            _ => {
                if current_char.is_numeric(){
                    let mut bad= false;
                    let mut num:usize =0;
                    while !current_char.is_whitespace() && !self.symbols.contains(&current_char) {
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
                    while !current_char.is_whitespace() && !self.symbols.contains(&current_char) {
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
                    if self.keywords.contains_key(&s){
                        return  Some(Token::new(TokenKind::Keyword(self.keywords[&s])));
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

    #[test]
    fn lexer5() {
        let input="1*7".to_string();
        let lex=Lexer::new(&input);
        let v:Vec<Token>=lex.collect();
        assert_eq!(v,[Token::new(TokenKind::Number(1)),Token::new(TokenKind::Star),Token::new(TokenKind::Number(7))]);
    }

    #[test]
    fn lexer6() {
        let input="===<".to_string();
        let lex=Lexer::new(&input);
        let v:Vec<Token>=lex.collect();
        assert_eq!(v,[Token::new(TokenKind::Eq),Token::new(TokenKind::Assignment),Token::new(TokenKind::Lesser)]);
    }

    #[test]
    fn lexer7() {
        let input="<==>".to_string();
        let lex=Lexer::new(&input);
        let v:Vec<Token>=lex.collect();
        assert_eq!(v,[Token::new(TokenKind::Leq),Token::new(TokenKind::Assignment),Token::new(TokenKind::Greater)]);
    }

    #[test]
    fn lexer8() {
        let input="4^8".to_string();
        let lex=Lexer::new(&input);
        let v:Vec<Token>=lex.collect();
        assert_eq!(v,[Token::new(TokenKind::Number(4)),Token::new(TokenKind::BitXor),Token::new(TokenKind::Number(8))]);
    }

    #[test]
    fn lexer9() {
        let input="hello!=!!5".to_string();
        let lex=Lexer::new(&input);
        let v:Vec<Token>=lex.collect();
        assert_eq!(v,[Token::new(TokenKind::Identifier("hello".to_string())),Token::new(TokenKind::Neq),Token::new(TokenKind::LogicNegation), 
            Token::new(TokenKind::LogicNegation),Token::new(TokenKind::Number(5))]);
    }

    #[test]
    fn lexer10() {
        let input="14||&|&&&|||".to_string();
        let lex=Lexer::new(&input);
        let v:Vec<Token>=lex.collect();
        assert_eq!(v,[Token::new(TokenKind::Number(14)), Token::new(TokenKind::LogicOr),Token::new(TokenKind::BitAnd), Token::new(TokenKind::BitOr),
            Token::new(TokenKind::LogicAnd), Token::new(TokenKind::BitAnd), Token::new(TokenKind::LogicOr),Token::new(TokenKind::BitOr)]);
    }


    #[test]
    fn lexer11() {
        let input="if 14 ;;:??23:?elsehello else hello".to_string();
        let lex=Lexer::new(&input);
        let v:Vec<Token>=lex.collect();
        assert_eq!(v,[Token::new(TokenKind::Keyword(Keyword::If)),Token::new(TokenKind::Number(14)), Token::new(TokenKind::Semicolon),Token::new(TokenKind::Semicolon),
        Token::new(TokenKind::Colon), Token::new(TokenKind::QuestionMark),Token::new(TokenKind::QuestionMark),
        Token::new(TokenKind::Number(23)), Token::new(TokenKind::Colon), Token::new(TokenKind::QuestionMark), Token::new(TokenKind::Identifier("elsehello".to_string())),
        Token::new(TokenKind::Keyword(Keyword::Else)), Token::new(TokenKind::Identifier("hello".to_string())),
        ]);
    }

    #[test]
    fn lexer12() {
        let input="do { break; } while(1);".to_string();
        let lex=Lexer::new(&input);
        let v:Vec<Token>=lex.collect();
        assert_eq!(v,[Token::new(TokenKind::Keyword(Keyword::Do)),Token::new(TokenKind::OpenBrace), Token::new(TokenKind::Keyword(Keyword::Break)),
        Token::new(TokenKind::Semicolon), Token::new(TokenKind::CloseBrace), Token::new(TokenKind::Keyword(Keyword::While)), Token::new(TokenKind::OpenParenthesis),
        Token::new(TokenKind::Number(1)), Token::new(TokenKind::CloseParenthesis), Token::new(TokenKind::Semicolon),
        ]);
    }
}