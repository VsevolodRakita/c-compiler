use std::collections::HashMap;

use crate::ast::*;

const WORDSIZE:usize = 8;

pub struct Generator{
    label_counter: usize,
    variables: HashMap<String, usize>,
    end_label: String,
}

impl Generator{
    pub fn new()->Self{
        Self {label_counter:1, 
            variables: HashMap::new(),
            end_label: "_label0".to_string(),
        }
    }

    pub fn generate_assembly(&mut self, ast: Ast)->String{
        self.generate_program(ast.get_program())
    }


    fn generate_program(&mut self, program: &AstProgram)->String{
        self.generate_function(program.get_function())+"\n"
    }

    fn generate_function(&mut self, function: &AstFunction)->String{
        self.variables=HashMap::new();
        let AstFunction::IdFunctionAux(name, ast_function_aux, _, variable_set)=function;
        let mut ctr: usize=1;
        for var in variable_set{
            self.variables.insert(var.clone(), ctr);
            ctr+=1;
        }
        let prologue = "\t.global ".to_string()+name+"\n"+name+":\n\tpush\t%rbp\n\tmov\t%rsp, %rbp\n";
        let variable_allocation = "\tadd\t$-".to_string()+&(self.variables.len()*WORDSIZE).to_string()+", %rsp\n";
        let default_return_value = "\tmov\t$0, %rax\n".to_string()+&self.end_label+&":\n".to_string();
        let varable_deallocation = "\tadd\t$".to_string()+&(self.variables.len()*WORDSIZE).to_string()+", %rsp\n";
        let epilogue = "\tpop\t%rbp\n\tret\n".to_string();
        prologue+&variable_allocation+&self.generate_function_aux(ast_function_aux,None, None)
            +&default_return_value+&varable_deallocation+&epilogue
    }

    fn generate_function_aux(&mut self, ast_function_aux: &AstFunctionAux, break_label: Option<&String>, continue_label: Option<&String>)->String{
        match ast_function_aux {
            AstFunctionAux::BlockItem(ast_block_item) => self.generate_block_item(ast_block_item, break_label, continue_label),
            AstFunctionAux::BlockItemAux(ast_block_item, ast_function_aux) => 
            self.generate_block_item(ast_block_item, break_label.clone(), continue_label.clone())+&self.generate_function_aux(ast_function_aux, break_label, continue_label),
        }
    }

    fn generate_block_item(&mut self, ast_block_item: &AstBlockItem, break_label: Option<&String>, continue_label: Option<&String>)->String{
        match ast_block_item {
            AstBlockItem::Statement(ast_statement) => self.generate_statement(ast_statement, break_label, continue_label),
            AstBlockItem::Declaration(ast_declaration) => self.generate_declaration(ast_declaration),
        }
    }

    fn generate_declaration(&mut self, ast_declaration: &AstDeclaration)->String{
        match ast_declaration {
            AstDeclaration::Id(_) => "".to_string(),
            AstDeclaration::IdAssignment(s, ast_expression) => self.generate_expression(ast_expression)+&"\tmov\t%rax, -".to_string()+
            &(self.variables[s]*WORDSIZE).to_string()+&"(%rbp)\n",
        }
    }

    fn generate_statement(&mut self, ast_statement: &AstStatement, break_label: Option<&String>, continue_label: Option<&String>)->String{
        match ast_statement {
            AstStatement::ReturnExpression(ast_expression) => 
                self.generate_expression(ast_expression)+&"\tjmp\t".to_string()+&self.end_label+&"\n".to_string(),
            AstStatement::ExpOptionSemicolon(ast_expression_option_semicolon) => 
                self.generate_exp_option_semicolon(ast_expression_option_semicolon),
            AstStatement::IfExpressionStatement(ast_expression, ast_statement) => {
                let end="_post_conditional".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let e1=self.generate_expression(ast_expression);
                let e2 = self.generate_statement(ast_statement, break_label, continue_label);
                e1+"\tcmp\t$0, %rax\n\tje\t"+&end+"\n"+&e2+&end+":\n"
            },
            AstStatement::IfExpressionStatementElseStatement(ast_expression, ast_statement, ast_statement1) => {
                let label = "_label".to_string()+&self.label_counter.to_string();
                let end="_post_conditional".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let e1=self.generate_expression(ast_expression);
                let e2 = self.generate_statement(ast_statement, break_label.clone(), continue_label.clone());
                let e3 = self.generate_statement(ast_statement1, break_label, continue_label);
                e1+"\tcmp\t$0, %rax\n\tjne\t"+&label+"\n"+&e3+"\tjmp\t"+&end+"\n"+&label+":\n"+&e2+&end+":\n"
            },
            AstStatement::Block(ast_block) => self.generate_block(ast_block,break_label, continue_label),
            AstStatement::For(ast_for) => self.generate_for(ast_for),
            AstStatement::DoWhile(ast_do_while) => self.generate_do_while(ast_do_while),
            AstStatement::While(ast_while) => self.generate_while(ast_while),
            AstStatement::Break => {
                match break_label {
                    Some(label) => "jmp\t".to_string()+label+"\n",
                    None => {println!("Compilation Failed! Encountered break outside control sequence.");"".to_string()},
                }
            },
            AstStatement::Continue => {
                match continue_label {
                    Some(label) => "jmp\t".to_string()+label+"\n",
                    None => {println!("Compilation Failed! Encountered continue outside control sequence.");"".to_string()},
                }
            },
        }
    }

    fn generate_block(&mut self, ast_block: &AstBlock, break_label: Option<&String>, continue_label: Option<&String>)->String{
        match ast_block {
            AstBlock::EmptyBlock => "".to_string(),
            AstBlock::FunctionAux(ast_function_aux, _) => self.generate_function_aux(ast_function_aux,break_label,continue_label),
        }
    }

    fn generate_for(&mut self, ast_for: &AstFor)->String{
        let condition_label = "_condition_label".to_string()+&self.label_counter.to_string();
        let post_exp_label = "_post_exp".to_string()+&self.label_counter.to_string();
        let end_of_loop="_end_of_loop".to_string()+&self.label_counter.to_string();
        self.label_counter+=1;
        let initial_clause= match *ast_for.initial_clause.clone() {
            AstInitialClause::Declaration(ast_declaration) => self.generate_declaration(&ast_declaration),
            AstInitialClause::NoDeclaration(ast_exp_option_semicolon) => 
                self.generate_exp_option_semicolon(&ast_exp_option_semicolon),
        };
        let controlling_exp = match *ast_for.controlling_expression.clone() {
            AstExpOptionSemicolon::ExpressionSemicolon(ast_expression) => 
                self.generate_expression(&*ast_expression),
            AstExpOptionSemicolon::EmptySemicolon => "mov\t$1,%rax\n".to_string(),
        };
        let post_exp = self.generate_exp_option_close_paren(&ast_for.post_expression);
        let body = self.generate_statement(&ast_for.body,Some(&end_of_loop),Some(&post_exp_label));
        initial_clause+&condition_label+":\n"+&controlling_exp+"\tcmp\t$0, %rax\n\tje\t"+&end_of_loop+"\n"+&body+&post_exp_label+
                    ":\n"+&post_exp+"jmp\t"+&condition_label+"\n"+&end_of_loop+":\n"
    }

    fn generate_do_while(&mut self, ast_do_while: &AstDoWhile)->String{
        match ast_do_while {
            AstDoWhile::StatementExpression(ast_statement, ast_expression) => {
                let beginning = "_pre_conditional".to_string()+&self.label_counter.to_string();
                let beginning2 = beginning.clone();
                let end="_post_conditional".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let e1 = self.generate_expression(ast_expression);
                let e2 = self.generate_statement(ast_statement, Some(&end), Some(&beginning));
                beginning+":\n"+&e2+&e1+"\tcmp\t$0, %rax\n\tje\t"+&end+"\n"+"jmp\t"+&beginning2+"\n"+&end+":\n"
            },
        }
    }

    fn generate_while(&mut self, ast_while: &AstWhile)->String{
        match ast_while {
            AstWhile::ExpressionStatement(ast_expression, ast_statement) => {
                let beginning = "_pre_conditional".to_string()+&self.label_counter.to_string();
                let beginning2 = beginning.clone();
                let end="_post_conditional".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let e1 = self.generate_expression(ast_expression);
                let e2 = self.generate_statement(ast_statement,Some(&end), Some(&beginning));
                beginning+":\n"+&e1+"\tcmp\t$0, %rax\n\tje\t"+&end+"\n"+&e2+"\tjmp\t"+&beginning2+"\n"+&end+":\n"
            },
        }
    }

    fn generate_exp_option_semicolon(&mut self, ast_exp_option_semicolon: &AstExpOptionSemicolon)->String{
        match ast_exp_option_semicolon{
            AstExpOptionSemicolon::ExpressionSemicolon(ast_expression) => self.generate_expression(ast_expression),
            AstExpOptionSemicolon::EmptySemicolon => "".to_string(),
        } 
    }

    fn generate_exp_option_close_paren(&mut self, ast_exp_option_close_paren: &AstExpOptionCloseParen)->String{
        match ast_exp_option_close_paren {
            AstExpOptionCloseParen::ExpressionCloseParen(ast_expression) => self.generate_expression(ast_expression),
            AstExpOptionCloseParen::EmptyCloseParen => "".to_string(),
        }
    }

    fn generate_expression(&mut self, ast_expression: &AstExpression)->String{
        match ast_expression {
            AstExpression::IdExpression(s, ast_expression) => self.generate_expression(ast_expression)+&"\tmov\t%rax, -".to_string()+
                &(self.variables[s]*WORDSIZE).to_string()+&"(%rbp)\n",
            AstExpression::ConditionalExp(ast_conditional_exp) => self.generate_conditional_exp(ast_conditional_exp),
        }
    }

    fn generate_conditional_exp(&mut self, ast_conditional_exp: &AstConditionalExp)->String{
        match ast_conditional_exp {
            AstConditionalExp::LogicOrExp(ast_logic_or_exp) => self.generate_logical_or_expression(ast_logic_or_exp),
            AstConditionalExp::LogicOrExpExpConditionalExp(ast_logic_or_exp, ast_expression, ast_conditional_exp) => {
                let label="_label".to_string()+&self.label_counter.to_string();
                let end="_post_conditional".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let e1=self.generate_logical_or_expression(ast_logic_or_exp);
                let e2 = self.generate_expression(ast_expression);
                let e3 = self.generate_conditional_exp(ast_conditional_exp);
                e1+&"\tcmp\t$0, %rax\n\tje\t".to_string()+&label+&"\n".to_string()+&e2+&"\tjmp\t".to_string()+&end+&"\n".to_string()+&label+&":\n".to_string()+
                    &e3+&end+&":\n".to_string()
            },
        }
    }

    fn generate_logical_or_expression(&mut self, ast_logic_or_exp: &AstLogicOrExp)->String{
        match ast_logic_or_exp {
            AstLogicOrExp::LogicAndExp(ast_logic_and_exp) => self.generate_logical_and_expression(ast_logic_and_exp),
            AstLogicOrExp::LogicAndExpAux(ast_logic_and_exp, ast_logic_or_exp_aux) => 
            self.generate_logical_and_expression(ast_logic_and_exp)+&self.generate_logical_or_expression_aux(ast_logic_or_exp_aux),
        }
    }

    fn generate_logical_or_expression_aux(&mut self, ast_logic_or_exp_aux: &AstLogicOrExpAux)->String{
        match ast_logic_or_exp_aux {
            AstLogicOrExpAux::LogicOrLogicAndExp(ast_logic_and_exp) => {
                let label="_label".to_string()+&self.label_counter.to_string();
                let end="_end".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                return "\tcmp\t$0, %rax\n\tje\t".to_string()+&label+&"\n\tmov\t$1, %rax\n\tjmp\t".to_string()+&end+&"\n"+
                &label+&":\n".to_string()+&self.generate_logical_and_expression(ast_logic_and_exp)+&"\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsetne\t%al\n"+
                &end+&":\n".to_string();
            },
            AstLogicOrExpAux::LogicOrLogicAndExpAux(ast_logic_and_exp, ast_expression_aux) => {
                let label="_label".to_string()+&self.label_counter.to_string();
                let end="_end".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                return "\tcmp\t$0, %rax\n\tje\t".to_string()+&label+&"\n\tmov\t$1, %rax\n\tjmp\t".to_string()+&end+&"\n"+
                &label+&":\n".to_string()+&self.generate_logical_and_expression(ast_logic_and_exp)+&"\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsetne\t%al\n"+
                &end+&":\n".to_string()+&self.generate_logical_or_expression_aux(ast_expression_aux);
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
            AstFactor::Expression(expression) => self.generate_expression(expression),
            AstFactor::UnaryOpFactor(op, factor2) => self.generate_factor(&factor2) +  &self.generate_unary_op(op),
            AstFactor::Int(x) => "\tmov\t$".to_string()+&x.to_string()+&", %rax\n".to_string(),
            AstFactor::Id(s) => {
                let stack_index = self.variables.get(s).unwrap()*WORDSIZE;
                "\tmov\t-".to_string()+&stack_index.to_string()+&"(%rbp), %rax\n".to_string()
            }
        }
    }


    fn generate_unary_op(&self, op: &AstUnaryOp)->String{
        match op {
            &crate::ast::AstUnaryOp::Minus => "\tneg\t%rax\n".to_string(),
            &crate::ast::AstUnaryOp::Complement => "\tnot\t%rax\n".to_string(),
            &crate::ast::AstUnaryOp::LogicNegation => "\tcmp\t$0, %rax\nmov\t$0, %rax\n\tsete\t%al\n".to_string(),
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
