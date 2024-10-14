use crate::ast::{Ast, AstExpression, AstFunction, AstProgram, AstStatement, AstUnaryOp};


pub struct Generator{

}

impl Generator{
    pub fn new()->Self{
        Self {  }
    }

    pub fn generate_assembly(&self, ast: Ast)->String{
        self.generate_program(ast.get_program())
    }

    fn generate_program(&self, program: &AstProgram)->String{
        self.generate_function(program.get_function())+"\n"
    }

    fn generate_function(&self, function: &AstFunction)->String{
        "\t.global ".to_string()+&function.get_id()+"\n"+&function.get_id()+":\n\t"+&self.generate_statement(function.get_statement())
    }

    fn generate_statement(&self, statement: &AstStatement)->String{
        self.generate_expression(statement.get_expression())+"\tret\n"
    }

    fn generate_expression(&self, expression: &AstExpression)->String{
        match expression {
            AstExpression::UnaryOpExpression(ast_expression_unary_op_expression) => {
                let s1=self.generate_unary_op(&ast_expression_unary_op_expression.get_op());
                let s2=self.generate_expression(&ast_expression_unary_op_expression.get_expression());
                return s2+&s1;
            },
            AstExpression::Int(ast_expression_int) => {
                return "movl\t$".to_string()+&ast_expression_int.get_val().to_string()+&", %eax\n".to_string()
            },
        }
    }

    fn generate_unary_op(&self, op: &AstUnaryOp)->String{
        match op.get_type() {
            crate::ast::UnaryOpType::Minus => return "\tneg\t%eax\n".to_string(),
            crate::ast::UnaryOpType::Complement => return "\tnot\t%eax\n".to_string(),
            crate::ast::UnaryOpType::LogicNegation => return "\tcmpl\t$0, %eax\nmovl\t$0, %eax\nsete\t%al\n".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn generator1(){
        let input="int main(){return 7;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast().unwrap();
        let gen=Generator::new();
        println!("{}",gen.generate_assembly(ast));
    }

    #[test]
    fn generator2(){
        let input="int main(){return -7;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast().unwrap();
        let gen=Generator::new();
        println!("{}",gen.generate_assembly(ast));
    }
}

