use std::collections::HashMap;

use crate::ast::*;

const WORDSIZE:i32 = 8;

pub struct Generator{
    label_counter: usize,
}

impl Generator{
    pub fn new()->Self{
        Self {
            label_counter:1, 
        }
    }

    pub fn generate_assembly(&mut self, ast: Ast)->String{
        self.generate_program(ast.get_program())
    }

    fn generate_program(&mut self, program: &AstProgram)->String{
        let mut program_string=String::new();
        for (_, status) in program.get_map(){
            match status {
                FunctionStatus::Declared(_) => continue,
                FunctionStatus::Implemented(ast_function) => {
                    program_string+=&self.generate_function(ast_function);
                    program_string+="\n";
                },
            }
        }
        program_string
    }

    fn generate_function(&mut self, ast_function:&AstFunction)->String{
        let mut variables=HashMap::new();
        let mut ctr:i32=-2;
        if ast_function.argument_list.len()<5{
            ctr-=4-ast_function.argument_list.len() as i32;
        }
        for var in ast_function.argument_list.iter(){
            variables.insert(var.clone(), ctr);
            ctr-=1;
        }
        ctr=1;
        for var in ast_function.variable_set.iter(){
            if !variables.contains_key(var){
                variables.insert(var.clone(), ctr);
                ctr+=1;
            }
        }
        let function_label = &ast_function.identifier.clone();
        let end_label = "end_label".to_string()+&self.label_counter.to_string();
        self.label_counter+=1;
        let prologue = "\t.global ".to_string()+&function_label+"\n"+&function_label+":\n\tpush\t%rbp\n\tmov\t%rsp, %rbp\n";
        let variable_allocation = "\tadd\t$-".to_string()+&((ctr-1)*WORDSIZE).to_string()+", %rsp\n";
        let default_return_value = "\tmov\t$0, %rax\n".to_string()+&end_label+&":\n".to_string();
        let variable_deallocation = "\tadd\t$".to_string()+&((ctr-1)*WORDSIZE).to_string()+", %rsp\n";
        let epilogue = "\tpop\t%rbp\n\tret\n".to_string();
        prologue+&variable_allocation+&self.generate_function_aux(&ast_function.body, &variables, None, None, &end_label)
            +&default_return_value+&variable_deallocation+&epilogue
    }

    fn generate_function_aux(&mut self, ast_function_aux: &AstFunctionAux, variables: &HashMap<String,i32>, break_label: Option<&String>, 
        continue_label: Option<&String>, function_end_label: &String)->String{
        match ast_function_aux {
            AstFunctionAux::BlockItem(ast_block_item) => self.generate_block_item(ast_block_item, variables, break_label, continue_label, function_end_label),
            AstFunctionAux::BlockItemAux(ast_block_item, ast_function_aux) => 
            self.generate_block_item(ast_block_item, variables, break_label.clone(), continue_label.clone(), function_end_label)+
            &self.generate_function_aux(ast_function_aux, variables, break_label, continue_label, function_end_label),
        }
    }

    fn generate_block_item(&mut self, ast_block_item: &AstBlockItem, variables: &HashMap<String,i32>, break_label: Option<&String>, continue_label: Option<&String>, 
        function_end_label: &String)->String{
        match ast_block_item {
            AstBlockItem::Statement(ast_statement) => self.generate_statement(ast_statement, variables, break_label, continue_label, function_end_label),
            AstBlockItem::Declaration(ast_declaration) => self.generate_declaration(ast_declaration, variables),
        }
    }

    fn generate_declaration(&mut self, ast_declaration: &AstDeclaration, variables: &HashMap<String,i32>,)->String{
        match ast_declaration {
            AstDeclaration::Id(_) => "".to_string(),
            AstDeclaration::IdAssignment(s, ast_expression) => self.generate_expression(ast_expression, variables)+&"\tmov\t%rax, -".to_string()+
            &(variables[s]*WORDSIZE).to_string()+&"(%rbp)\n",
        }
    }

    fn generate_statement(&mut self, ast_statement: &AstStatement, variables: &HashMap<String,i32>, break_label: Option<&String>, 
        continue_label: Option<&String>, function_end_label: &String)->String{
        match ast_statement {
            AstStatement::ReturnExpression(ast_expression) => 
                self.generate_expression(ast_expression, variables)+&"\tjmp\t".to_string()+function_end_label+&"\n".to_string(),
            AstStatement::ExpOptionSemicolon(ast_expression_option_semicolon) => 
                self.generate_exp_option_semicolon(ast_expression_option_semicolon, variables),
            AstStatement::IfExpressionStatement(ast_expression, ast_statement) => {
                let end="post_conditional".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let e1=self.generate_expression(ast_expression, variables);
                let e2 = self.generate_statement(ast_statement, variables, break_label, continue_label, function_end_label);
                e1+"\tcmp\t$0, %rax\n\tje\t"+&end+"\n"+&e2+&end+":\n"
            },
            AstStatement::IfExpressionStatementElseStatement(ast_expression, ast_statement, ast_statement1) => {
                let label = "label".to_string()+&self.label_counter.to_string();
                let end="post_conditional".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let e1=self.generate_expression(ast_expression, variables);
                let e2 = self.generate_statement(ast_statement, variables, break_label.clone(), continue_label.clone(), function_end_label);
                let e3 = self.generate_statement(ast_statement1, variables, break_label, continue_label, function_end_label);
                e1+"\tcmp\t$0, %rax\n\tjne\t"+&label+"\n"+&e3+"\tjmp\t"+&end+"\n"+&label+":\n"+&e2+&end+":\n"
            },
            AstStatement::Block(ast_block) => self.generate_block(ast_block, variables, break_label, continue_label, function_end_label),
            AstStatement::For(ast_for) => self.generate_for(ast_for, variables, function_end_label),
            AstStatement::DoWhile(ast_do_while) => self.generate_do_while(ast_do_while, variables, function_end_label),
            AstStatement::While(ast_while) => self.generate_while(ast_while, variables, function_end_label),
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

    fn generate_block(&mut self, ast_block: &AstBlock, variables: &HashMap<String,i32>, break_label: Option<&String>, 
        continue_label: Option<&String>, function_end_label: &String)->String{
        match ast_block {
            AstBlock::EmptyBlock => "".to_string(),
            AstBlock::FunctionAux(ast_function_aux, _) => 
                self.generate_function_aux(ast_function_aux, variables,break_label,continue_label, function_end_label),
        }
    }

    fn generate_for(&mut self, ast_for: &AstFor, variables: &HashMap<String,i32>, function_end_label: &String)->String{
        let condition_label = "condition_label".to_string()+&self.label_counter.to_string();
        let post_exp_label = "post_exp".to_string()+&self.label_counter.to_string();
        let end_of_loop="end_of_loop".to_string()+&self.label_counter.to_string();
        self.label_counter+=1;
        let initial_clause= match *ast_for.initial_clause.clone() {
            AstInitialClause::Declaration(ast_declaration) => self.generate_declaration(&ast_declaration, variables),
            AstInitialClause::NoDeclaration(ast_exp_option_semicolon) => 
                self.generate_exp_option_semicolon(&ast_exp_option_semicolon, variables),
        };
        let controlling_exp = match *ast_for.controlling_expression.clone() {
            AstExpOptionSemicolon::ExpressionSemicolon(ast_expression) => 
                self.generate_expression(&*ast_expression, variables),
            AstExpOptionSemicolon::EmptySemicolon => "mov\t$1,%rax\n".to_string(),
        };
        let post_exp = self.generate_exp_option_close_paren(&ast_for.post_expression, variables);
        let body = self.generate_statement(&ast_for.body, variables,Some(&end_of_loop),Some(&post_exp_label), function_end_label);
        initial_clause+&condition_label+":\n"+&controlling_exp+"\tcmp\t$0, %rax\n\tje\t"+&end_of_loop+"\n"+&body+&post_exp_label+
                    ":\n"+&post_exp+"jmp\t"+&condition_label+"\n"+&end_of_loop+":\n"
    }

    fn generate_do_while(&mut self, ast_do_while: &AstDoWhile, variables: &HashMap<String,i32>, function_end_label: &String)->String{
        match ast_do_while {
            AstDoWhile::StatementExpression(ast_statement, ast_expression) => {
                let beginning = "pre_conditional".to_string()+&self.label_counter.to_string();
                let beginning2 = beginning.clone();
                let end="post_conditional".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let e1 = self.generate_expression(ast_expression, variables);
                let e2 = self.generate_statement(ast_statement, variables, Some(&end), Some(&beginning), function_end_label);
                beginning+":\n"+&e2+&e1+"\tcmp\t$0, %rax\n\tje\t"+&end+"\n"+"jmp\t"+&beginning2+"\n"+&end+":\n"
            },
        }
    }

    fn generate_while(&mut self, ast_while: &AstWhile, variables: &HashMap<String,i32>, function_end_label: &String)->String{
        match ast_while {
            AstWhile::ExpressionStatement(ast_expression, ast_statement) => {
                let beginning = "pre_conditional".to_string()+&self.label_counter.to_string();
                let beginning2 = beginning.clone();
                let end="post_conditional".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let e1 = self.generate_expression(ast_expression, variables);
                let e2 = self.generate_statement(ast_statement, variables,Some(&end), Some(&beginning), function_end_label);
                beginning+":\n"+&e1+"\tcmp\t$0, %rax\n\tje\t"+&end+"\n"+&e2+"\tjmp\t"+&beginning2+"\n"+&end+":\n"
            },
        }
    }

    fn generate_exp_option_semicolon(&mut self, ast_exp_option_semicolon: &AstExpOptionSemicolon, variables: &HashMap<String,i32>)->String{
        match ast_exp_option_semicolon{
            AstExpOptionSemicolon::ExpressionSemicolon(ast_expression) => self.generate_expression(ast_expression, variables),
            AstExpOptionSemicolon::EmptySemicolon => "".to_string(),
        } 
    }

    fn generate_exp_option_close_paren(&mut self, ast_exp_option_close_paren: &AstExpOptionCloseParen, variables: &HashMap<String,i32>)->String{
        match ast_exp_option_close_paren {
            AstExpOptionCloseParen::ExpressionCloseParen(ast_expression) => self.generate_expression(ast_expression, variables),
            AstExpOptionCloseParen::EmptyCloseParen => "".to_string(),
        }
    }

    fn generate_expression(&mut self, ast_expression: &AstExpression, variables: &HashMap<String,i32>)->String{
        match ast_expression {
            AstExpression::IdExpression(s, ast_expression) => self.generate_expression(ast_expression, variables)+
            &"\tmov\t%rax, -".to_string()+&(variables[s]*WORDSIZE).to_string()+&"(%rbp)\n",
            AstExpression::ConditionalExp(ast_conditional_exp) => self.generate_conditional_exp(ast_conditional_exp, variables),
        }
    }

    fn generate_conditional_exp(&mut self, ast_conditional_exp: &AstConditionalExp, variables: &HashMap<String,i32>)->String{
        match ast_conditional_exp {
            AstConditionalExp::LogicOrExp(ast_logic_or_exp) => self.generate_logical_or_expression(ast_logic_or_exp, variables),
            AstConditionalExp::LogicOrExpExpConditionalExp(ast_logic_or_exp, ast_expression, ast_conditional_exp) => {
                let label="label".to_string()+&self.label_counter.to_string();
                let end="post_conditional".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let e1=self.generate_logical_or_expression(ast_logic_or_exp, variables);
                let e2 = self.generate_expression(ast_expression, variables);
                let e3 = self.generate_conditional_exp(ast_conditional_exp, variables);
                e1+&"\tcmp\t$0, %rax\n\tje\t".to_string()+&label+&"\n".to_string()+&e2+&"\tjmp\t".to_string()+&end+&"\n".to_string()+&label+&":\n".to_string()+
                    &e3+&end+&":\n".to_string()
            },
        }
    }

    fn generate_logical_or_expression(&mut self, ast_logic_or_exp: &AstLogicOrExp, variables: &HashMap<String,i32>)->String{
        match ast_logic_or_exp {
            AstLogicOrExp::LogicAndExp(ast_logic_and_exp) => self.generate_logical_and_expression(ast_logic_and_exp, variables),
            AstLogicOrExp::LogicAndExpAux(ast_logic_and_exp, ast_logic_or_exp_aux) => 
            self.generate_logical_and_expression(ast_logic_and_exp, variables)+&self.generate_logical_or_expression_aux(ast_logic_or_exp_aux, variables),
        }
    }

    fn generate_logical_or_expression_aux(&mut self, ast_logic_or_exp_aux: &AstLogicOrExpAux, variables: &HashMap<String,i32>)->String{
        match ast_logic_or_exp_aux {
            AstLogicOrExpAux::LogicOrLogicAndExp(ast_logic_and_exp) => {
                let label="label".to_string()+&self.label_counter.to_string();
                let end="end".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                return "\tcmp\t$0, %rax\n\tje\t".to_string()+&label+&"\n\tmov\t$1, %rax\n\tjmp\t".to_string()+&end+&"\n"+
                &label+&":\n".to_string()+&self.generate_logical_and_expression(ast_logic_and_exp, variables)+&"\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsetne\t%al\n"+
                &end+&":\n".to_string();
            },
            AstLogicOrExpAux::LogicOrLogicAndExpAux(ast_logic_and_exp, ast_expression_aux) => {
                let label="label".to_string()+&self.label_counter.to_string();
                let end="end".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                return "\tcmp\t$0, %rax\n\tje\t".to_string()+&label+&"\n\tmov\t$1, %rax\n\tjmp\t".to_string()+&end+&"\n"+
                &label+&":\n".to_string()+&self.generate_logical_and_expression(ast_logic_and_exp, variables)+&"\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsetne\t%al\n"+
                &end+&":\n".to_string()+&self.generate_logical_or_expression_aux(ast_expression_aux, variables);
            },
        }
    }

    fn generate_logical_and_expression(&mut self, ast_logic_and_exp: &AstLogicAndExp, variables: &HashMap<String,i32>)->String{
        match ast_logic_and_exp {
            AstLogicAndExp::BitOrExp(ast_bit_or_exp) => self.generate_bit_or_expression(ast_bit_or_exp, variables),
            AstLogicAndExp::BitOrExpAux(ast_bit_or_exp, ast_logic_and_exp_aux) => 
            self.generate_bit_or_expression(ast_bit_or_exp, variables)+&self.generate_logical_and_expression_aux(ast_logic_and_exp_aux, variables),
        }
    }

    fn generate_logical_and_expression_aux(&mut self, ast_logic_and_exp_aux: &AstLogicAndExpAux, variables: &HashMap<String,i32>)->String{
        match ast_logic_and_exp_aux {
            AstLogicAndExpAux::LogicAndBitOrExp(ast_bit_or_exp) => {
                let label="label".to_string()+&self.label_counter.to_string();
                let end="end".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                return "\tcmp\t$0, %rax\n\tjne\t".to_string()+&label+&"\n\tjmp\t".to_string()+&end+&"\n"+
                    &label+&":\n".to_string()+&self.generate_bit_or_expression(ast_bit_or_exp, variables)+&"\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsetne\t%al\n"+
                    &end+&":\n".to_string();
            },
            AstLogicAndExpAux::LogicAndBitOrExpAux(ast_bit_or_exp, ast_logic_and_exp_aux) => {
                let label="label".to_string()+&self.label_counter.to_string();
                let end="end".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                return "\tcmp\t$0, %rax\n\tjne\t".to_string()+&label+&"\n\tjmp\t".to_string()+&end+&"\n"+
                    &label+&":\n".to_string()+&self.generate_bit_or_expression(ast_bit_or_exp, variables)+&"\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsetne\t%al\n"+
                    &end+&":\n".to_string()+&self.generate_logical_and_expression_aux(ast_logic_and_exp_aux, variables);
            },
        }
    }

    fn generate_bit_or_expression(&mut self, ast_bit_or_exp: &AstBitOrExp, variables: &HashMap<String,i32>)->String{
        match ast_bit_or_exp {
            AstBitOrExp::BitXorExp(ast_bit_xor_exp) => self.generate_bit_xor_expression(ast_bit_xor_exp, variables),
            AstBitOrExp::BitXorExpAux(ast_bit_xor_exp, ast_bit_or_exp_aux) => 
            self.generate_bit_xor_expression(ast_bit_xor_exp, variables)+&self.generate_bit_or_expression_aux(ast_bit_or_exp_aux, variables),
        }
    }

    fn generate_bit_or_expression_aux(&mut self, ast_bit_or_exp_aux: &AstBitOrExpAux, variables: &HashMap<String,i32>)->String{
        match ast_bit_or_exp_aux {
            AstBitOrExpAux::BitOrBitXorExp(ast_bit_xor_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_xor_expression(ast_bit_xor_exp, variables)+&"\tpop\t%rcx\n\tor\t%rcx, %rax\n".to_string(),
            AstBitOrExpAux::BitOrBitXorExpAux(ast_bit_xor_exp, ast_bit_or_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_xor_expression(ast_bit_xor_exp, variables)+&"\tpop\t%rcx\n\tor\t%rcx, %rax\n".to_string()+
                &self.generate_bit_or_expression_aux(ast_bit_or_exp_aux, variables),
        }
    }


    fn generate_bit_xor_expression(&mut self, ast_bit_xor_exp: &AstBitXorExp, variables: &HashMap<String,i32>)->String{
        match ast_bit_xor_exp {
            AstBitXorExp::BitAndExp(ast_bit_and_exp) => self.generate_bit_and_expression(ast_bit_and_exp, variables),
            AstBitXorExp::BitAndExpAux(ast_bit_and_exp, ast_bit_xor_exp_aux) => 
            self.generate_bit_and_expression(ast_bit_and_exp, variables)+&self.generate_bit_xor_expression_aux(ast_bit_xor_exp_aux, variables),
        }
    }

    fn generate_bit_xor_expression_aux(&mut self, ast_bit_xor_exp_aux: &AstBitXorExpAux, variables: &HashMap<String,i32>)->String{
        match ast_bit_xor_exp_aux{
            AstBitXorExpAux::BitXorBitAndExp(ast_bit_and_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_and_expression(ast_bit_and_exp, variables)+&"\tpop\t%rcx\n\txor\t%rcx, %rax\n".to_string(),
            AstBitXorExpAux::BitXorBitAndExpAux(ast_bit_and_exp, ast_bit_xor_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_and_expression(ast_bit_and_exp, variables)+
                &"\tpop\t%rcx\n\txor\t%rcx, %rax\n".to_string()+&self.generate_bit_xor_expression_aux(ast_bit_xor_exp_aux, variables),
        }
    }

    fn generate_bit_and_expression(&mut self, ast_bit_and_exp: &AstBitAndExp, variables: &HashMap<String,i32>)->String{
        match ast_bit_and_exp {
            AstBitAndExp::EqualityExp(ast_equality_exp) => self.generate_equality_expression(ast_equality_exp, variables),
            AstBitAndExp::EqualityExpAux(ast_equality_exp, ast_bit_and_exp_aux) => 
            self.generate_equality_expression(ast_equality_exp, variables)+&self.generate_bit_and_expression_aux(ast_bit_and_exp_aux, variables),
        }
    }

    fn generate_bit_and_expression_aux(&mut self, ast_bit_and_exp_aux: &AstBitAndExpAux, variables: &HashMap<String,i32>)->String{
        match ast_bit_and_exp_aux {
            AstBitAndExpAux::BitAndEqualityExp(ast_equality_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_equality_expression(ast_equality_exp, variables)+
                &"\tpop\t%rcx\n\tand\t%rcx, %rax\n".to_string(),
            AstBitAndExpAux::BitAndEqualityExpAux(ast_equality_exp, ast_bit_and_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_equality_expression(ast_equality_exp, variables)+
                &"\tpop\t%rcx\n\tand\t%rcx, %rax\n".to_string()+&self.generate_bit_and_expression_aux(ast_bit_and_exp_aux, variables),
        }
    }

    fn generate_equality_expression(&mut self, ast_equality_exp: &AstEqualityExp, variables: &HashMap<String,i32>)->String{
        match ast_equality_exp {
            AstEqualityExp::RelationalExp(ast_relational_exp) => self.generate_relational_expression(ast_relational_exp, variables),
            AstEqualityExp::RelationalExpAux(ast_relational_exp, ast_equality_exp_aux) => 
            self.generate_relational_expression(ast_relational_exp, variables)+&self.generate_equality_expression_aux(ast_equality_exp_aux, variables),
        }
    }

    fn generate_equality_expression_aux(&mut self, ast_equality_exp_aux: &AstEqualityExpAux, variables: &HashMap<String,i32>)->String{
        match ast_equality_exp_aux {
            AstEqualityExpAux::NeqRelationalExp(ast_relational_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_relational_expression(ast_relational_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetne\t%al\n".to_string(),
            AstEqualityExpAux::NeqRelationalExpAux(ast_relational_exp, ast_equality_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_relational_expression(ast_relational_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetne\t%al\n".to_string()+&self.generate_equality_expression_aux(ast_equality_exp_aux, variables),
            AstEqualityExpAux::EqRelationalExp(ast_relational_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_relational_expression(ast_relational_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string(),
            AstEqualityExpAux::EqRelationalExpAux(ast_relational_exp, ast_equality_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_relational_expression(ast_relational_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string()+&self.generate_equality_expression_aux(ast_equality_exp_aux, variables),
        }
    }

    fn generate_relational_expression(&mut self, ast_relational_exp: &AstRelationalExp, variables: &HashMap<String,i32>)->String{
        match ast_relational_exp {
            AstRelationalExp::BitShiftExp(ast_bit_shift_exp) => self.generate_bit_shift_expression(ast_bit_shift_exp, variables),
            AstRelationalExp::BitShiftExpAux(ast_bit_shift_exp, ast_relational_exp_aux) => 
            self.generate_bit_shift_expression(ast_bit_shift_exp, variables)+&self.generate_relational_expression_aux(ast_relational_exp_aux, variables),
        }
    }

    fn generate_relational_expression_aux(&mut self, ast_relational_exp_aux: &AstRelationalExpAux, variables: &HashMap<String,i32>)->String{
        match ast_relational_exp_aux {
            AstRelationalExpAux::LessBitShiftExp(ast_bit_shift_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetl\t%al\n".to_string(),
            AstRelationalExpAux::LessBitShiftExpAux(ast_bit_shift_exp, ast_relational_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetl\t%al\n".to_string()+&self.generate_relational_expression_aux(ast_relational_exp_aux, variables),
            AstRelationalExpAux::GreaterBitShiftExp(ast_bit_shift_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rcx, %rax\n\tmov\t$0, %rax\n\tsetl\t%al\n".to_string(),
            AstRelationalExpAux::GreaterBitShiftExpAux(ast_bit_shift_exp, ast_relational_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rcx, %rax\n\tmov\t$0, %rax\n\tsetl\t%al\n".to_string()+
                &self.generate_relational_expression_aux(ast_relational_exp_aux, variables),
            AstRelationalExpAux::LeqBitShiftExp(ast_bit_shift_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rcx, %rax\n\tmov\t$0, %rax\n\tsetl\t%al\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string(),
            AstRelationalExpAux::LeqBitShiftExpAux(ast_bit_shift_exp, ast_relational_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rcx, %rax\n\tmov\t$0, %rax\n\tsetl\t%al\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string()+
                &self.generate_relational_expression_aux(ast_relational_exp_aux, variables),
            AstRelationalExpAux::GeqBitShiftExp(ast_bit_shift_exp) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetl\t%al\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string(),
            AstRelationalExpAux::GeqBitShiftExpAux(ast_bit_shift_exp, ast_relational_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_bit_shift_expression(ast_bit_shift_exp, variables)+
                &"\tpop\t%rcx\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetl\t%al\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n".to_string()+
                &self.generate_relational_expression_aux(ast_relational_exp_aux, variables),
        }
    }

    fn generate_bit_shift_expression(&mut self, ast_bit_shift_exp: &AstBitShiftExp, variables: &HashMap<String,i32>)->String{
        match ast_bit_shift_exp {
            AstBitShiftExp::AdditiveExp(ast_additive_exp) => self.generate_additive_expression(ast_additive_exp, variables),
            AstBitShiftExp::AdditiveExpAux(ast_additive_exp, ast_bit_shift_exp_aux) => 
                self.generate_additive_expression(ast_additive_exp, variables)+&self.generate_bit_shift_expression_aux(ast_bit_shift_exp_aux, variables),
        }
    }

    fn generate_bit_shift_expression_aux(&mut self, ast_bit_shift_exp_aux: &AstBitShiftExpAux, variables: &HashMap<String,i32>,)->String{
        match ast_bit_shift_exp_aux {
            AstBitShiftExpAux::BitShiftLeftAdditiveExp(ast_additive_exp) => "\tpush\t%rax\n".to_string()+
                &self.generate_additive_expression(ast_additive_exp, variables)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tshl\t%cl,%rax\n".to_string(),
            AstBitShiftExpAux::BitShiftLeftAdditiveExpAux(ast_additive_exp, ast_bit_shift_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_additive_expression(ast_additive_exp, variables)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tshl\t%cl,%rax\n".to_string()+
                &self.generate_bit_shift_expression_aux(ast_bit_shift_exp_aux, variables),
            AstBitShiftExpAux::BitShiftRightAdditiveExp(ast_additive_exp) => "\tpush\t%rax\n".to_string()+
                &self.generate_additive_expression(ast_additive_exp, variables)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tshr\t%cl,%rax\n".to_string(),
            AstBitShiftExpAux::BitShiftRighttAdditiveExpAux(ast_additive_exp, ast_bit_shift_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_additive_expression(ast_additive_exp, variables)+
                &"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tshr\t%cl,%rax\n".to_string()+&self.generate_bit_shift_expression_aux(ast_bit_shift_exp_aux, variables),
        }
    }

    fn generate_additive_expression(&mut self, ast_additive_exp: &AstAdditiveExp, variables: &HashMap<String,i32>,)->String{
        match ast_additive_exp {
            AstAdditiveExp::Term(ast_term) => self.generate_term(ast_term, variables),
            AstAdditiveExp::TermAux(ast_term, ast_additive_exp_aux) =>
                self.generate_term(ast_term, variables)+&self.generate_additive_expression_aux(ast_additive_exp_aux, variables),
        }
    }

    fn generate_additive_expression_aux(&mut self, ast_additive_exp_aux: &AstAdditiveExpAux, variables: &HashMap<String,i32>)->String{
        match ast_additive_exp_aux {
            AstAdditiveExpAux::PlusTerm(ast_term) => 
                "\tpush\t%rax\n".to_string()+&self.generate_term(&ast_term, variables)+&"\tpop\t%rcx\n\tadd\t%rcx, %rax\n".to_string(),
            AstAdditiveExpAux::PlusTermAux(ast_term, ast_additive_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_term(&ast_term, variables)+&"\tpop\t%rcx\n\tadd\t%rcx, %rax\n".to_string()+
                &self.generate_additive_expression_aux(ast_additive_exp_aux, variables),
            AstAdditiveExpAux::MinusTerm(ast_term) => 
                "\tpush\t%rax\n".to_string()+&self.generate_term(&ast_term, variables)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tsub\t%rcx, %rax\n".to_string(),
            AstAdditiveExpAux::MinusTermAux(ast_term, ast_additive_exp_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_term(&ast_term, variables)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tsub\t%rcx, %rax\n".to_string()
                +&self.generate_additive_expression_aux(ast_additive_exp_aux, variables),
        }
    }

    fn generate_term(&mut self, ast_term: &AstTerm, variables: &HashMap<String,i32>)->String{
        match ast_term {
            AstTerm::Factor(ast_factor) => self.generate_factor(ast_factor, variables),
            AstTerm::FactorAux(ast_factor, ast_term_aux) => self.generate_factor(ast_factor,variables)+
                &self.generate_term_aux(ast_term_aux, variables),
        }
    }

    fn generate_term_aux(&mut self, ast_term_aux: &AstTermAux, variables: &HashMap<String,i32>,)->String{
        match ast_term_aux {
            AstTermAux::StarFactor(ast_factor) => 
                "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor, variables)+&"\tpop\t%rcx\n\timul\t%rcx, %rax\n".to_string(),
            AstTermAux::StarFactorAux(ast_factor, ast_term_aux2) => 
                "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor, variables)+&"\tpop\t%rcx\n\timul\t%rcx, %rax\n".to_string()+
                &self.generate_term_aux(ast_term_aux2, variables),
            AstTermAux::DivideFactor(ast_factor) => 
                "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor, variables)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tcdq\n\tidiv\t%rcx\n".to_string(),
            AstTermAux::DivideFactorAux(ast_factor, ast_term_aux) => 
                "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor, variables)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tcdq\n\tidiv\t%rcx\n".to_string()
                +&self.generate_term_aux(ast_term_aux,variables),
            AstTermAux::ModFactor(ast_factor) => 
                "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor, variables)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tcdq\n\tidiv\t%rcx\n\tmov\t%rdx, %rax\n".to_string(),
            AstTermAux::ModFactorAux(ast_factor, ast_term_aux) => 
            "\tpush\t%rax\n".to_string()+&self.generate_factor(ast_factor, variables)+&"\tmov\t%rax, %rcx\n\tpop\t%rax\n\tcdq\n\tidiv\t%rcx\n\tmov\t%rdx, %rax\n".to_string()
                +&self.generate_term_aux(ast_term_aux, variables),
        }
    }

    fn generate_factor(&mut self, factor: &AstFactor, variables: &HashMap<String,i32>)->String{
        match factor {
            AstFactor::FunctionCall(ast_function_call) => {
                let mut s = String::new();
                let num_arguments = ast_function_call.arguments.len();
                let move_to_registers = vec!["\tmov\t%rax,%rcx\n", "\tmov\t%rax,%rdx\n", "\tmov\t%rax,%r8\n", "\tmov\t%rax,%r9\n"];
                //In windows x64 calling convention, the first 4 arguments are passed in the registers (rcx,rdx,r8,r9), and any more are passed on the stack.
                for i in 0..usize::min(num_arguments,move_to_registers.len()){
                    s+=&self.generate_expression(&ast_function_call.arguments[i], variables);
                    s+=move_to_registers[i];
                }
                for ast_expression in ast_function_call.arguments.iter().rev(){
                    s+=&self.generate_expression(ast_expression, variables);
                    s+="\tpush\t%rax\n";
                }
                if num_arguments<4{
                    s+="\tadd\t$-";                 //In windows x64 calling convention, the caller must allocate 32 bytes of space on
                    s+=&((4-num_arguments)*WORDSIZE as usize).to_string();  //the stack right before the function call.
                    s+=", %rsp\n";
                }
                s+"\tcall\t"+&ast_function_call.id+"\n\tadd\t$"+&(i32::max(num_arguments as i32, 4)*WORDSIZE).to_string()+", %rsp\n"
            },
            AstFactor::Expression(expression) => self.generate_expression(expression, variables),
            AstFactor::UnaryOpFactor(op, factor2) => self.generate_factor(&factor2, variables) +  &self.generate_unary_op(op),
            AstFactor::Int(x) => "\tmov\t$".to_string()+&x.to_string()+&", %rax\n".to_string(),
            AstFactor::Id(s) => {
                let stack_index = variables.get(s).unwrap()*WORDSIZE;
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
