use crate::ast::*;


pub struct Generator{
    label_counter: usize,
}

impl Generator{
    pub fn new()->Self{
        Self {label_counter:0,}
    }

    pub fn generate_assembly(&mut self, ast: Ast)->String{
        self.generate_program(ast.get_program())
    }

    fn generate_program(&mut self, program: &AstProgram)->String{
        self.generate_function(program.get_function())+"\n"
    }

    fn generate_function(&mut self, function: &AstFunction)->String{
        "\t.global ".to_string()+&function.get_id()+"\n"+&function.get_id()+":\n"+&self.generate_statement(function.get_statement())
    }

    fn generate_statement(&mut self, statement: &AstStatement)->String{
        self.generate_expression(statement.get_expression())+"\tret\n"
    }

    fn generate_expression(&mut self, expression: &AstExpression)->String{
        match expression {
            AstExpression::LogicAndExp(ast_logic_and_exp) => self.generate_logical_and_expression(ast_logic_and_exp),
            AstExpression::LogicAndExpAux(ast_logic_and_exp, ast_expression_aux) => 
            self.generate_logical_and_expression(ast_logic_and_exp)+&self.generate_expression_aux(ast_expression_aux),
        }
    }

    fn generate_expression_aux(&mut self, ast_expression_aux: &AstExpressionAux)->String{
        match ast_expression_aux {
            AstExpressionAux::LogicOrLogicAndExp(ast_logic_and_exp) => {
                let label="_label".to_string()+&self.label_counter.to_string();
                let end="_end".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                return "\tcmp\t$0, %rax\n\tje\t".to_string()+&label+&"\n\tmov\t$1, %rax\n\tjmp\t".to_string()+&end+&"\n"+
                &label+&":\n".to_string()+&self.generate_logical_and_expression(ast_logic_and_exp)+&"\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsetne\t%al\n"+
                &end+&":\n".to_string();
            },
            AstExpressionAux::LogicOrLogicAndExpAux(ast_logic_and_exp, ast_expression_aux) => {
                let label="_label".to_string()+&self.label_counter.to_string();
                let end="_end".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                return "\tcmp\t$0, %rax\n\tje\t".to_string()+&label+&"\n\tmov\t$1, %rax\n\tjmp\t".to_string()+&end+&"\n"+
                &label+&":\n".to_string()+&self.generate_logical_and_expression(ast_logic_and_exp)+&"\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsetne\t%al\n"+
                &end+&":\n".to_string()+&self.generate_expression_aux(ast_expression_aux);
            },
        }
    }

    fn generate_logical_and_expression(&mut self, ast_logic_and_exp: &AstLogicAndExp)->String{
        match ast_logic_and_exp {
            AstLogicAndExp::BitOrExp(ast_bit_or_exp) => self.generate_bit_or_expression(ast_bit_or_exp),
            AstLogicAndExp::BitOrExpAux(ast_bit_or_exp, ast_logic_and_exp_aux) => 
            self.generate_bit_or_expression(ast_bit_or_exp)+&self.generate_logical_and_expression_aux(ast_logic_and_exp_aux),
        }
    }

    fn generate_logical_and_expression_aux(&mut self, ast_logic_and_exp_aux: &AstLogicAndExpAux)->String{
        match ast_logic_and_exp_aux {
            AstLogicAndExpAux::LogicAndBitOrExp(ast_bit_or_exp) => {
                let label="_label".to_string()+&self.label_counter.to_string();
                let end="_end".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                return "\tcmp\t$0, %rax\n\tjne\t".to_string()+&label+&"\n\tjmp\t".to_string()+&end+&"\n"+
                    &label+&":\n".to_string()+&self.generate_bit_or_expression(ast_bit_or_exp)+&"\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsetne\t%al\n"+
                    &end+&":\n".to_string();
            },
            AstLogicAndExpAux::LogicAndBitOrExpAux(ast_bit_or_exp, ast_logic_and_exp_aux) => {
                let label="_label".to_string()+&self.label_counter.to_string();
                let end="_end".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                return "\tcmp\t$0, %rax\n\tjne\t".to_string()+&label+&"\n\tjmp\t".to_string()+&end+&"\n"+
                    &label+&":\n".to_string()+&self.generate_bit_or_expression(ast_bit_or_exp)+&"\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsetne\t%al\n"+
                    &end+&":\n".to_string()+&self.generate_logical_and_expression_aux(ast_logic_and_exp_aux);
            },
        }
    }

    fn generate_bit_or_expression(&mut self, ast_bit_or_exp: &AstBitOrExp)->String{
        match ast_bit_or_exp {
            AstBitOrExp::BitXorExp(ast_bit_xor_exp) => self.generate_bit_xor_expression(ast_bit_xor_exp),
            AstBitOrExp::BitXorExpAux(ast_bit_xor_exp, ast_bit_or_exp_aux) => 
            self.generate_bit_xor_expression(ast_bit_xor_exp)+&self.generate_bit_or_expression_aux(ast_bit_or_exp_aux),
        }
    }

    fn generate_bit_or_expression_aux(&mut self, ast_bit_or_exp_aux: &AstBitOrExpAux)->String{
        match ast_bit_or_exp_aux {
            AstBitOrExpAux::BitOrBitXorExp(ast_bit_xor_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_xor_expression(ast_bit_xor_exp)+&"\tpop\t%rcx\n\tor\t%rcx, %rax\n".to_string(),
            AstBitOrExpAux::BitOrBitXorExpAux(ast_bit_xor_exp, ast_bit_or_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_xor_expression(ast_bit_xor_exp)+&"\tpop\t%rcx\n\tor\t%rcx, %rax\n".to_string()+
                &self.generate_bit_or_expression_aux(ast_bit_or_exp_aux),
        }
    }


    fn generate_bit_xor_expression(&mut self, ast_bit_xor_exp: &AstBitXorExp)->String{
        match ast_bit_xor_exp {
            AstBitXorExp::BitAndExp(ast_bit_and_exp) => self.generate_bit_and_expression(ast_bit_and_exp),
            AstBitXorExp::BitAndExpAux(ast_bit_and_exp, ast_bit_xor_exp_aux) => 
            self.generate_bit_and_expression(ast_bit_and_exp)+&self.generate_bit_xor_expression_aux(ast_bit_xor_exp_aux),
        }
    }

    fn generate_bit_xor_expression_aux(&mut self, ast_bit_xor_exp_aux: &AstBitXorExpAux)->String{
        match ast_bit_xor_exp_aux{
            AstBitXorExpAux::BitXorBitAndExp(ast_bit_and_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_and_expression(ast_bit_and_exp)+&"\tpop\t%rcx\n\txor\t%rcx, %rax\n".to_string(),
            AstBitXorExpAux::BitXorBitAndExpAux(ast_bit_and_exp, ast_bit_xor_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_and_expression(ast_bit_and_exp)+
                &"\tpop\t%rcx\n\txor\t%rcx, %rax\n".to_string()+&self.generate_bit_xor_expression_aux(ast_bit_xor_exp_aux),
        }
    }

    fn generate_bit_and_expression(&mut self, ast_bit_and_exp: &AstBitAndExp)->String{
        match ast_bit_and_exp {
            AstBitAndExp::EqualityExp(ast_equality_exp) => self.generate_equality_expression(ast_equality_exp),
            AstBitAndExp::EqualityExpAux(ast_equality_exp, ast_bit_and_exp_aux) => 
            self.generate_equality_expression(ast_equality_exp)+&self.generate_bit_and_expression_aux(ast_bit_and_exp_aux),
        }
    }

    fn generate_bit_and_expression_aux(&mut self, ast_bit_and_exp_aux: &AstBitAndExpAux)->String{
        match ast_bit_and_exp_aux {
            AstBitAndExpAux::BitAndEqualityExp(ast_equality_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_equality_expression(ast_equality_exp)+
                &"\tpop\t%rcx\n\tand\t%rcx, %rax\n".to_string(),
            AstBitAndExpAux::BitAndEqualityExpAux(ast_equality_exp, ast_bit_and_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_equality_expression(ast_equality_exp)+
                &"\tpop\t%rcx\n\tand\t%rcx, %rax\n".to_string()+&self.generate_bit_and_expression_aux(ast_bit_and_exp_aux),
        }
    }

    fn generate_equality_expression(&mut self, ast_equality_exp: &AstEqualityExp)->String{
        match ast_equality_exp {
            AstEqualityExp::RelationalExp(ast_relational_exp) => self.generate_relational_expression(ast_relational_exp),
            AstEqualityExp::RelationalExpAux(ast_relational_exp, ast_equality_exp_aux) => 
            self.generate_relational_expression(ast_relational_exp)+&self.generate_equality_expression_aux(ast_equality_exp_aux),
        }
    }

    fn generate_equality_expression_aux(&mut self, ast_equality_exp_aux: &AstEqualityExpAux)->String{
        match ast_equality_exp_aux {
            AstEqualityExpAux::NeqRelationalExp(ast_relational_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_relational_expression(ast_relational_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetne\t%al\n".to_string(),
            AstEqualityExpAux::NeqRelationalExpAux(ast_relational_exp, ast_equality_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_relational_expression(ast_relational_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetne\t%al\n".to_string()+&self.generate_equality_expression_aux(ast_equality_exp_aux),
            AstEqualityExpAux::EqRelationalExp(ast_relational_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_relational_expression(ast_relational_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string(),
            AstEqualityExpAux::EqRelationalExpAux(ast_relational_exp, ast_equality_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_relational_expression(ast_relational_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string()+&self.generate_equality_expression_aux(ast_equality_exp_aux),
        }
    }

    fn generate_relational_expression(&mut self, ast_relational_exp: &AstRelationalExp)->String{
        match ast_relational_exp {
            AstRelationalExp::BitShiftExp(ast_bit_shift_exp) => self.generate_bit_shift_expression(ast_bit_shift_exp),
            AstRelationalExp::BitShiftExpAux(ast_bit_shift_exp, ast_relational_exp_aux) => 
            self.generate_bit_shift_expression(ast_bit_shift_exp)+&self.generate_relational_expression_aux(ast_relational_exp_aux),
        }
    }

    fn generate_relational_expression_aux(&mut self, ast_relational_exp_aux: &AstRelationalExpAux)->String{
        match ast_relational_exp_aux {
            AstRelationalExpAux::LessBitShiftExp(ast_bit_shift_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetl\t%al\n".to_string(),
            AstRelationalExpAux::LessBitShiftExpAux(ast_bit_shift_exp, ast_relational_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetl\t%al\n".to_string()+&self.generate_relational_expression_aux(ast_relational_exp_aux),
            AstRelationalExpAux::GreaterBitShiftExp(ast_bit_shift_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rcx, %rax\n\tmov\t$0, %rax\n\tsetl\t%al\n".to_string(),
            AstRelationalExpAux::GreaterBitShiftExpAux(ast_bit_shift_exp, ast_relational_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rcx, %rax\n\tmov\t$0, %rax\n\tsetl\t%al\n".to_string()+&self.generate_relational_expression_aux(ast_relational_exp_aux),
            AstRelationalExpAux::LeqBitShiftExp(ast_bit_shift_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rcx, %rax\n\tmov\t$0, %rax\n\tsetl\t%al\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string(),
            AstRelationalExpAux::LeqBitShiftExpAux(ast_bit_shift_exp, ast_relational_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rcx, %rax\n\tmov\t$0, %rax\n\tsetl\t%al\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string()+
                &self.generate_relational_expression_aux(ast_relational_exp_aux),
            AstRelationalExpAux::GeqBitShiftExp(ast_bit_shift_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetl\t%al\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string(),
            AstRelationalExpAux::GeqBitShiftExpAux(ast_bit_shift_exp, ast_relational_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetl\t%al\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string()+
                &self.generate_relational_expression_aux(ast_relational_exp_aux),
        }
    }

    fn generate_bit_shift_expression(&mut self, ast_bit_shift_exp: &AstBitShiftExp)->String{
        match ast_bit_shift_exp {
            AstBitShiftExp::AdditiveExp(ast_additive_exp) => self.generate_additive_expression(ast_additive_exp),
            AstBitShiftExp::AdditiveExpAux(ast_additive_exp, ast_bit_shift_exp_aux) => 
                self.generate_additive_expression(ast_additive_exp)+&self.generate_bit_shift_expression_aux(ast_bit_shift_exp_aux),
        }
    }

    fn generate_bit_shift_expression_aux(&mut self, ast_bit_shift_exp_aux: &AstBitShiftExpAux)->String{
        match ast_bit_shift_exp_aux {
            AstBitShiftExpAux::BitShiftLeftAdditiveExp(ast_additive_exp) => "\tpush\t%rax\n".to_string()+
                &self.generate_additive_expression(ast_additive_exp)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tshl\t%cl,%rax\n".to_string(),
            AstBitShiftExpAux::BitShiftLeftAdditiveExpAux(ast_additive_exp, ast_bit_shift_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_additive_expression(ast_additive_exp)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tshl\t%cl,%rax\n".to_string()+
                &self.generate_bit_shift_expression_aux(ast_bit_shift_exp_aux),
            AstBitShiftExpAux::BitShiftRightAdditiveExp(ast_additive_exp) => "\tpush\t%rax\n".to_string()+
                &self.generate_additive_expression(ast_additive_exp)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tshr\t%cl,%rax\n".to_string(),
            AstBitShiftExpAux::BitShiftRighttAdditiveExpAux(ast_additive_exp, ast_bit_shift_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_additive_expression(ast_additive_exp)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tshr\t%cl,%rax\n".to_string()+
                &self.generate_bit_shift_expression_aux(ast_bit_shift_exp_aux),
        }
    }

    fn generate_additive_expression(&mut self, ast_additive_exp: &AstAdditiveExp)->String{
        match ast_additive_exp {
            AstAdditiveExp::Term(ast_term) => self.generate_term(ast_term),
            AstAdditiveExp::TermAux(ast_term, ast_additive_exp_aux) =>
                self.generate_term(ast_term)+&self.generate_additive_expression_aux(ast_additive_exp_aux),
        }
    }

    fn generate_additive_expression_aux(&mut self, ast_additive_exp_aux: &AstAdditiveExpAux)->String{
        match ast_additive_exp_aux {
            AstAdditiveExpAux::PlusTerm(ast_term) => 
                "\tpush\t%rax\n".to_string()+&self.generate_term(&ast_term)+&"\tpop\t%rcx\n\tadd\t%rcx, %rax\n".to_string(),
            AstAdditiveExpAux::PlusTermAux(ast_term, ast_additive_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_term(&ast_term)+&"\tpop\t%rcx\n\tadd\t%rcx, %rax\n".to_string()+
                &self.generate_additive_expression_aux(ast_additive_exp_aux),
            AstAdditiveExpAux::MinusTerm(ast_term) => 
                "\tpush\t%rax\n".to_string()+&self.generate_term(&ast_term)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tsub\t%rcx, %rax\n".to_string(),
            AstAdditiveExpAux::MinusTermAux(ast_term, ast_additive_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_term(&ast_term)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tsub\t%rcx, %rax\n".to_string()
                +&self.generate_additive_expression_aux(ast_additive_exp_aux),
        }
    }

    fn generate_term(&mut self, ast_term: &AstTerm)->String{
        match ast_term {
            AstTerm::Factor(ast_factor) => self.generate_factor(ast_factor),
            AstTerm::FactorAux(ast_factor, ast_term_aux) => self.generate_factor(ast_factor)+&self.generate_term_aux(ast_term_aux),
        }
    }

    fn generate_term_aux(&mut self, ast_term_aux: &AstTermAux)->String{
        match ast_term_aux {
            AstTermAux::StarFactor(ast_factor) => 
                "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor)+&"\tpop\t%rcx\n\timul\t%rcx, %rax\n".to_string(),
            AstTermAux::StarFactorAux(ast_factor, ast_term_aux2) => 
                "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor)+&"\tpop\t%rcx\n\timul\t%rcx, %rax\n".to_string()+&self.generate_term_aux(ast_term_aux2),
            AstTermAux::DivideFactor(ast_factor) => 
                "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tcdq\n\tidiv\t%rcx\n".to_string(),
            AstTermAux::DivideFactorAux(ast_factor, ast_term_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tcdq\n\tidiv\t%rcx\n".to_string()
                +&self.generate_term_aux(ast_term_aux),
            AstTermAux::ModFactor(ast_factor) => 
                "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tcdq\n\tidiv\t%rcx\n\tmov\t%rdx, %rax\n".to_string(),
            AstTermAux::ModFactorAux(ast_factor, ast_term_aux) => 
            "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tcdq\n\tidiv\t%rcx\n\tmov\t%rdx, %rax\n".to_string()
                +&self.generate_term_aux(ast_term_aux),
        }
    }

    fn generate_factor(&mut self, factor: &AstFactor)->String{
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
            &crate::ast::AstUnaryOp::LogicNegation => return "\tcmp\t$0, %rax\nmov\t$0, %rax\n\tsete\t%al\n".to_string(),
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
        let mut gen=Generator::new();
        println!("{}",gen.generate_assembly(ast));
    }

    #[test]
    fn generator2(){
        let input="int main(){return -7;}";
        let lex=Lexer::new(&input);
        let mut pars=Parser::new(lex);
        let ast=pars.get_ast().unwrap();
        let mut gen=Generator::new();
        println!("{}",gen.generate_assembly(ast));
    }
}

