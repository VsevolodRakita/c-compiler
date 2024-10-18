use crate::ast::{Ast, AstExpression, AstExpressionAux, AstFactor, AstFunction, AstProgram, AstStatement, AstTerm, AstTermAux, AstUnaryOp};


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
        "\t.global ".to_string()+&function.get_id()+"\n"+&function.get_id()+":\n"+&self.generate_statement(function.get_statement())
    }

    fn generate_statement(&self, statement: &AstStatement)->String{
        self.generate_expression(statement.get_expression())+"\tret\n"
    }

    fn generate_expression(&self, expression: &AstExpression)->String{
        match expression {
            AstExpression::Term(term) => return self.generate_term(term),
            AstExpression::TermExpAux(term, expression_aux) => 
                return self.generate_term(term)+&self.generate_expression_aux(&expression_aux),
        }
    }

    fn generate_expression_aux(&self, expression_aux: &AstExpressionAux)->String{
        match expression_aux {
            AstExpressionAux::PlusTerm(term) => 
                return "\tpush\t%rax\n".to_string()+&self.generate_term(&term)+&"\tpop\t%rcx\n\tadd\t%rcx, %rax\n".to_string(),
            AstExpressionAux::MinusTerm(term) => 
                return "\tpush\t%rax\n".to_string()+&self.generate_term(&term)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tsub\t%rcx, %rax\n".to_string(),
            AstExpressionAux::PlusTermAux(term, expression_aux2) => 
                return "\tpush\t%rax\n".to_string()+&self.generate_term(&term)+&"\tpop\t%rcx\n\tadd\t%rcx, %rax\n".to_string()+&self.generate_expression_aux(expression_aux2),
            AstExpressionAux::MinusTermAux(term, expression_aux2) => 
                return "\tpush\t%rax\n".to_string()+&self.generate_term(&term)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tsub\t%rcx, %rax\n".to_string()
                +&self.generate_expression_aux(expression_aux2),
        }
    }

    fn generate_term(&self, term: &AstTerm)->String{
        match term{
            AstTerm::Factor(factor) => return self.generate_factor(factor),
            AstTerm::FactorAux(factor, aux) => return self.generate_factor(factor)+&self.generate_term_aux(&aux),
        }
    }

    fn generate_term_aux(&self, term_aux: &AstTermAux)->String{
        match term_aux {
            AstTermAux::TimesFactor(factor) => 
                return "\tpush\t%rax\n".to_string()+&self.generate_factor(factor)+&"\tpop\t%rcx\n\timul\t%rcx, %rax\n".to_string(),
            AstTermAux::DivideFactor(factor) => 
                return "\tpush\t%rax\n".to_string()+&self.generate_factor(factor)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tcdq\n\tidiv\t%rcx\n".to_string(),
            AstTermAux::TimesFactorAux(factor, term_aux2) => 
                return "\tpush\t%rax\n".to_string()+&self.generate_factor(factor)+&"\tpop\t%rcx\n\timul\t%rcx, %rax\n".to_string()+&self.generate_term_aux(term_aux2),
            AstTermAux::DivideFactorAux(factor, term_aux2) => 
                return "\tpush\t%rax\n".to_string()+&self.generate_factor(factor)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tcdq\n\tidiv\t%rcx\n".to_string()
                +&self.generate_term_aux(term_aux2),
        }
    }
    fn generate_factor(&self, factor: &AstFactor)->String{
        match factor {
            AstFactor::Expression(expression) => return self.generate_expression(expression),
            AstFactor::UnaryOpFactor(op, factor2) => return self.generate_factor(&factor2) +  &self.generate_unary_op(op),
            AstFactor::Int(x) => return "\tmov\t$".to_string()+&x.to_string()+&", %rax\n".to_string(),
        }
    }


    fn generate_unary_op(&self, op: &AstUnaryOp)->String{
        match op {
            &crate::ast::AstUnaryOp::Minus => return "\tneg\t%rax\n".to_string(),
            &crate::ast::AstUnaryOp::Complement => return "\tnot\t%rax\n".to_string(),
            &crate::ast::AstUnaryOp::LogicNegation => return "\tcmp\t$0, %rax\nmov\t$0, %rax\nsete\t%al\n".to_string(),
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

