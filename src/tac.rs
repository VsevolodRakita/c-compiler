/// Implements an intermediate step between an `Ast` (see ast.rs) and an asl generator (see generator.rs).
/// `TacGenerator` gets an `Ast` and returnes a vector of pairs, each representing an implemented function.
/// This vector is for use with the struct `generator::Generator` or `optimizer::Optimizer`.
/*
program = Program(function_definition*)
function_definition = Function(identifier, identifier* params, instruction* body)
instruction = Return(val) 
    | Unary(unary_operator, val src, val dst)
    | Binary(binary_operator, val src1, val src2, val dst)
    | Copy(val src, val dst)
    | Jump(identifier target)
    | JumpIfZero(val condition, identifier target)
    | JumpIfNotZero(val condition, identifier target)
    | Label(identifier)
    | FunCall(identifier fun_name, val* args, val dst)
val = Constant(int) | Var(identifier)
unary_operator = Complement | Negate | Not
binary_operator = Add | Subtract | Multiply | Divide | Mod | Equal | NotEqual
    | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual | LogicOr | LogicAnd
    | BitwiseOr | BitwiseAnd | BitwiseXor
*/

use std::collections::HashSet;

use crate::ast::*;

#[derive(Debug,PartialEq,Clone)]
pub enum TacValue{
    Constant(i64),
    Variable(String),
}

#[derive(Debug,PartialEq,Clone)]
pub enum TacBinaryCommand{
    BitOr,
    BitXor,
    BitAnd,
    Eq,
    Neq,
    Geq,
    Leq,
    Greater,
    Less,
    BitShiftLeft,
    BitShiftRight,
    Plus,
    Minus,
    Multiply,
    Divide,
    Mod,
}

#[derive(Debug,PartialEq,Clone)]
pub enum TacUnaryCommand{
    Minus,
    Not,
    Complement,
}

#[derive(Debug,PartialEq,Clone)]
pub enum TacCommand{
    PlaceHolder,
    Pass,
    Bad,
    FunctionStart(String,Vec<String>),//FunctionStart(id, arguments)
    FunctionEnd,
    Jump(String),
    JumpIfZero(TacValue, String),
    JumpIfNotZero(TacValue, String),
    Label(String),
    Return(TacValue),
    Copy(TacValue,TacValue), // Copy(from,dest)
    BinaryCommand(TacBinaryCommand, TacValue, TacValue, TacValue), //BinaryCommand(Command,LeftSide,RightSide,dest)
    UnaryCommand(TacUnaryCommand, TacValue, TacValue), //UnaryCommand(Command, argument, dest)
    FunctionCall(String, Vec<TacValue>, TacValue),
}

#[derive(Debug,PartialEq,Clone)]
pub struct TacGenerator{
    label_counter: usize,
    command_list: Vec<TacCommand>,
    failed: bool,
}

impl TacGenerator {
    pub fn new()->Self{
        Self{
            label_counter: 0,
            command_list: Vec::new(),
            failed: false,
        }
    }

    pub fn convert_ast_to_tac(&mut self, ast: Ast){
        let map = ast.get_program_and_consume().get_map_and_consume();
        for (_, func) in map{
            match func {
                FunctionStatus::Declared(_) => (),
                FunctionStatus::Implemented(ast_function) => self.generate_function(ast_function),
            }
        }
    }

    pub fn get_command_lists_and_consume(self)->Vec<(Vec<TacCommand>,HashSet<String>)> {
        if self.failed{
            return vec![];
        }
        let mut ans = vec![];
        let mut curr_function_variables = HashSet::new();
        let mut curr_function_commands = Vec::new();
        for command in self.command_list{
            match command.clone() {
                TacCommand::FunctionStart(_, vec) => {
                    for var in vec.iter(){
                        curr_function_variables.insert(var.clone());
                    }
                },
                TacCommand::FunctionEnd => {
                    curr_function_commands.push(command);
                    ans.push((curr_function_commands,curr_function_variables));
                    curr_function_variables = HashSet::new();
                    curr_function_commands = Vec::new();
                    continue;
                },
                TacCommand::Return(tac_value) => {
                    if let TacValue::Variable(s) = tac_value{
                        curr_function_variables.insert(s.clone());
                    }
                },
                TacCommand::Copy(_, tac_value1) => {
                    if let TacValue::Variable(s) = tac_value1{
                        curr_function_variables.insert(s.clone());
                    }
                    else {
                        panic!("Can't move to constant!")
                    }
                },
                TacCommand::BinaryCommand(_, _, _, tac_value2) => {
                    if let TacValue::Variable(s) = tac_value2{
                        curr_function_variables.insert(s.clone());
                    }
                    else {
                        panic!("Can't move to constant!")
                    }
                },
                TacCommand::UnaryCommand(_, _, tac_value1) => {
                    if let TacValue::Variable(s) = tac_value1{
                        curr_function_variables.insert(s.clone());
                    }
                    else {
                        panic!("Can't move to constant!")
                    }
                },
                TacCommand::FunctionCall(_, _, tac_value) => {
                    if let TacValue::Variable(s) = tac_value.clone(){
                        curr_function_variables.insert(s);
                    }
                    else {
                        panic!("Can't move to constant!")
                    }
                },
                _ => (),
            }
            curr_function_commands.push(command);
        }
        
        ans
    }
}

impl TacGenerator{
    fn generate_function(&mut self, ast_function: AstFunction){
        self.command_list.push(TacCommand::FunctionStart(ast_function.identifier, ast_function.argument_list));
        self.generate_function_aux(ast_function.body,  &None, &None);
        self.command_list.push(TacCommand::FunctionEnd);
    }

    fn generate_function_aux(&mut self, ast_function_aux: AstFunctionAux, break_label: &Option<String>, 
        continue_label: &Option<String>){
        match ast_function_aux {
            AstFunctionAux::BlockItem(ast_block_item) => self.generate_block_item(*ast_block_item,  break_label, 
                continue_label),
            AstFunctionAux::BlockItemAux(ast_block_item, ast_function_aux) => {
                self.generate_block_item(*ast_block_item, break_label, continue_label);
                self.generate_function_aux(*ast_function_aux, break_label, continue_label);
            },
        }
    }

    fn generate_block_item(&mut self, ast_block_item: AstBlockItem, break_label: &Option<String>, 
        continue_label: &Option<String>){
        match ast_block_item {
            AstBlockItem::Statement(ast_statement) => self.generate_statement(*ast_statement, break_label, 
                continue_label),
            AstBlockItem::Declaration(ast_declaration) => {
                match *ast_declaration {
                    AstDeclaration::Id(_) => (),
                    AstDeclaration::IdAssignment(id, ast_expression) => {
                        let temp = self.generate_expression(*ast_expression);
                        self.command_list.push(TacCommand::Copy(temp, TacValue::Variable(id)));
                    },
                }
            },
        }
    }

    fn generate_statement(&mut self, ast_statement: AstStatement, break_label: &Option<String>, 
        continue_label: &Option<String>){
        match ast_statement {
            AstStatement::ReturnExpression(ast_expression) => {
                let temp = self.generate_expression(*ast_expression);
                self.command_list.push(TacCommand::Return(temp));
            },
            AstStatement::ExpOptionSemicolon(ast_exp_option_semicolon) => {
                match *ast_exp_option_semicolon {
                    AstExpOptionSemicolon::ExpressionSemicolon(ast_expression) => 
                        {
                            self.generate_expression(*ast_expression);
                        },
                    AstExpOptionSemicolon::EmptySemicolon => (),
                }
            },
            AstStatement::IfExpressionStatement(ast_expression, ast_statement) => {
                let post_if_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let temp = self.generate_expression(*ast_expression);
                self.command_list.push(TacCommand::JumpIfZero(temp, post_if_label.clone()));
                self.generate_statement(*ast_statement, break_label, continue_label);
                self.command_list.push(TacCommand::Label(post_if_label));
            },
            AstStatement::IfExpressionStatementElseStatement(ast_expression, ast_statement, ast_statement1) => {
                let else_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let post_else_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let temp = self.generate_expression(*ast_expression);
                self.command_list.push(TacCommand::JumpIfZero(temp, else_label.clone()));
                self.generate_statement(*ast_statement,  break_label, continue_label);
                self.command_list.push(TacCommand::Jump(post_else_label.clone()));
                self.command_list.push(TacCommand::Label(else_label));
                self.generate_statement(*ast_statement1,  break_label, continue_label);
                self.command_list.push(TacCommand::Label(post_else_label));
            },
            AstStatement::Block(ast_block) => {
                match *ast_block {
                    AstBlock::EmptyBlock => (),
                    AstBlock::FunctionAux(ast_function_aux, _) => {
                        self.generate_function_aux(*ast_function_aux, break_label, continue_label);
                    },
                }
            },
            AstStatement::For(ast_for) => {
                let condition_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let post_expression_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let post_loop_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                match *ast_for.initial_clause {
                    AstInitialClause::NoDeclaration(ast_exp_option_semicolon) => {
                        match *ast_exp_option_semicolon {
                            AstExpOptionSemicolon::ExpressionSemicolon(ast_expression) => {
                                self.generate_expression(*ast_expression);
                            },
                            AstExpOptionSemicolon::EmptySemicolon => (),
                        }
                    },
                    AstInitialClause::Declaration(ast_declaration) => {
                        match *ast_declaration {
                            AstDeclaration::Id(_) => (),
                            AstDeclaration::IdAssignment(id, ast_expression) => {
                                let temp = self.generate_expression(*ast_expression);
                                self.command_list.push(TacCommand::Copy(temp, TacValue::Variable(id)));
                            },
                        }
                    },
                }
                self.command_list.push(TacCommand::Label(condition_label.clone()));
                match *ast_for.controlling_expression {
                    AstExpOptionSemicolon::ExpressionSemicolon(ast_expression) => {
                        let temp = self.generate_expression(*ast_expression);
                        self.command_list.push(TacCommand::JumpIfZero(temp, post_loop_label.clone()));
                    },
                    AstExpOptionSemicolon::EmptySemicolon => (),
                }
                self.generate_statement(*ast_for.body, 
                    &Some(post_loop_label.clone()), &Some(post_expression_label.clone()));
                self.command_list.push(TacCommand::Label(post_expression_label));
                match *ast_for.post_expression {
                    AstExpOptionCloseParen::ExpressionCloseParen(ast_expression) => 
                        {self.generate_expression(*ast_expression);},
                    AstExpOptionCloseParen::EmptyCloseParen => (),
                }
                self.command_list.push(TacCommand::Jump(condition_label));
                self.command_list.push(TacCommand::Label(post_loop_label));

            },
            AstStatement::DoWhile(ast_do_while) => {
                match *ast_do_while {
                    AstDoWhile::StatementExpression(ast_statement, ast_expression) => {
                        let do_label = "_tac_label".to_string()+&self.label_counter.to_string();
                        self.label_counter+=1;
                        let condition_label = "_tac_label".to_string()+&self.label_counter.to_string();
                        self.label_counter+=1;
                        let post_loop_label = "_tac_label".to_string()+&self.label_counter.to_string();
                        self.label_counter+=1;
                        self.command_list.push(TacCommand::Label(do_label.clone()));
                        self.generate_statement(*ast_statement, &Some(post_loop_label.clone()), 
                            &Some(condition_label.clone()));
                        self.command_list.push(TacCommand::Label(condition_label));
                        let temp = self.generate_expression(*ast_expression);
                        self.command_list.push(TacCommand::JumpIfNotZero(temp, do_label));
                        self.command_list.push(TacCommand::Label(post_loop_label));
                    },
                }
            },
            AstStatement::While(ast_while) => {
                match *ast_while {
                    AstWhile::ExpressionStatement(ast_expression, ast_statement) => {
                        let condition_label = "_tac_label".to_string()+&self.label_counter.to_string();
                        self.label_counter+=1;
                        let post_loop_label = "_tac_label".to_string()+&self.label_counter.to_string();
                        self.label_counter+=1;
                        self.command_list.push(TacCommand::Label(condition_label.clone()));
                        let temp = self.generate_expression(*ast_expression);
                        self.command_list.push(TacCommand::JumpIfZero(temp, post_loop_label.clone()));
                        self.generate_statement(*ast_statement, &Some(post_loop_label.clone()), 
                            &Some(condition_label.clone()));
                        self.command_list.push(TacCommand::Jump(condition_label));
                        self.command_list.push(TacCommand::Label(post_loop_label));
                    },
                }
            },
            AstStatement::Break => {
                match break_label {
                    Some(target) => {
                        self.command_list.push(TacCommand::Jump(target.clone()));
                    },
                    None => {
                        println!("Compilation Failed! Improperly placed break.");
                        self.failed=true;
                    }, 
                }
            },
            AstStatement::Continue => {
                match continue_label {
                    Some(target) => {
                        self.command_list.push(TacCommand::Jump(target.clone()));
                    },
                    None => {
                        println!("Compilation Failed! Improperly placed continue.");
                        self.failed=true;
                    }, 
                }
            },
        }
    }

    fn generate_expression(&mut self, ast_expression: AstExpression)->TacValue{
        match ast_expression {
            AstExpression::IdExpression(id, ast_expression) => {
                let src = self.generate_expression(*ast_expression);
                let dest= TacValue::Variable(id);
                self.command_list.push(TacCommand::Copy(src, dest.clone()));
                dest
            },
            AstExpression::ConditionalExp(ast_conditional_exp) => 
                self.generate_conditional_exp(*ast_conditional_exp),
        }
    }

    fn generate_conditional_exp(&mut self, ast_conditional_exp: AstConditionalExp)->TacValue{
        match ast_conditional_exp {
            AstConditionalExp::LogicOrExp(ast_logic_or_exp) => 
                self.generate_logic_or_expression(*ast_logic_or_exp),
            AstConditionalExp::LogicOrExpExpConditionalExp(ast_logic_or_exp, ast_expression, ast_conditional_exp) => {
                let false_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let post_conditional_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let temp_var = "_tac_var".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let temp_var = TacValue::Variable(temp_var);
                let result = self.generate_logic_or_expression(*ast_logic_or_exp);
                self.command_list.push(TacCommand::JumpIfZero(result, false_label.clone()));
                let true_result = self.generate_expression(*ast_expression);
                self.command_list.push(TacCommand::Copy(true_result, temp_var.clone()));
                self.command_list.push(TacCommand::Jump(post_conditional_label.clone()));
                self.command_list.push(TacCommand::Label(false_label));
                let false_result = self.generate_conditional_exp(*ast_conditional_exp);
                self.command_list.push(TacCommand::Copy(false_result, temp_var.clone()));
                self.command_list.push(TacCommand::Label(post_conditional_label));
                temp_var
            },
        }
    }

    fn generate_logic_or_expression(&mut self, ast_logic_or_exp: AstLogicOrExp)->TacValue{
        match ast_logic_or_exp {
            AstLogicOrExp::LogicAndExp(ast_logic_and_exp) => self.generate_logic_and_expression(*ast_logic_and_exp),
            AstLogicOrExp::LogicAndExpAux(ast_logic_and_exp, ast_logic_or_exp_aux) => {
                let dst = "_tac_var".to_string()+&self.label_counter.to_string();
                let dst = TacValue::Variable(dst.clone());
                self.label_counter+=1;
                let post_expression_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let true_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                self.command_list.push(TacCommand::Copy(TacValue::Constant(0), dst.clone()));
                let left = self.generate_logic_and_expression(*ast_logic_and_exp);
                self.command_list.push(TacCommand::JumpIfNotZero(left, true_label.clone()));
                self.generate_logic_or_expression_aux(*ast_logic_or_exp_aux, post_expression_label.clone(), true_label.clone());
                self.command_list.push(TacCommand::Label(true_label));
                self.command_list.push(TacCommand::Copy(TacValue::Constant(1), dst.clone()));
                self.command_list.push(TacCommand::Label(post_expression_label));
                dst
            },
        }
    }

    fn generate_logic_or_expression_aux(&mut self, ast_logic_or_exp_aux: AstLogicOrExpAux, post_expression_label: String, true_label: String){
        match ast_logic_or_exp_aux {
            AstLogicOrExpAux::LogicOrLogicAndExp(ast_logic_and_exp) => {
                let right = self.generate_logic_and_expression(*ast_logic_and_exp);
                self.command_list.push(TacCommand::JumpIfZero(right, post_expression_label));

            },
            AstLogicOrExpAux::LogicOrLogicAndExpAux(ast_logic_and_exp, ast_logic_or_exp_aux) => {
                let left = self.generate_logic_and_expression(*ast_logic_and_exp);
                self.command_list.push(TacCommand::JumpIfNotZero(left, true_label.clone()));
                self.generate_logic_or_expression_aux(*ast_logic_or_exp_aux, post_expression_label, true_label);
            },
        }
    }

    fn generate_logic_and_expression(&mut self, ast_logic_and_exp: AstLogicAndExp)->TacValue{
        match ast_logic_and_exp {
            AstLogicAndExp::BitOrExp(ast_bit_or_exp) => self.generate_bit_or_expression(*ast_bit_or_exp),
            AstLogicAndExp::BitOrExpAux(ast_bit_or_exp, ast_logic_and_exp_aux) => {
                let dst = "_tac_var".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let dst = TacValue::Variable(dst);
                let post_expression_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let false_label = "_tac_label".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                self.command_list.push(TacCommand::Copy(TacValue::Constant(1), dst.clone()));
                let left = self.generate_bit_or_expression(*ast_bit_or_exp);
                self.command_list.push(TacCommand::JumpIfZero(left, false_label.clone()));
                self.generate_logic_and_expression_aux(*ast_logic_and_exp_aux, post_expression_label.clone(), false_label.clone());
                self.command_list.push(TacCommand::Label(false_label));
                self.command_list.push(TacCommand::Copy(TacValue::Constant(0), dst.clone()));
                self.command_list.push(TacCommand::Label(post_expression_label));
                dst
            },
        }
    }

    fn generate_logic_and_expression_aux(&mut self, ast_logic_and_exp_aux: AstLogicAndExpAux, post_expression_label: String, false_label: String){
        match ast_logic_and_exp_aux {
            AstLogicAndExpAux::LogicAndBitOrExp(ast_bit_or_exp) => {
                let right = self.generate_bit_or_expression(*ast_bit_or_exp);
                self.command_list.push(TacCommand::JumpIfNotZero(right, post_expression_label));
            },
            AstLogicAndExpAux::LogicAndBitOrExpAux(ast_bit_or_exp, ast_logic_and_exp_aux) => {
                let left = self.generate_bit_or_expression(*ast_bit_or_exp);
                self.command_list.push(TacCommand::JumpIfZero(left, false_label.clone()));
                self.generate_logic_and_expression_aux(*ast_logic_and_exp_aux, post_expression_label, false_label);
            },
        }
    }

}


/// Macro for handling expressions with form similar to
/// <logical-or-exp> ::= <logical-and-exp> | <logical-and-exp> <logical-or-exp-aux>
/// <logical-or-exp-aux> ::= "||" <logical-and-exp> |  "||" <logical-and-exp><logical-or-exp-aux>
/// 
/// #Arguments
/// - `$method_name` - the name of the method to be created (in the rule above: `generate_logic_or_expression`).
/// - `$aux_method_name` - the name of the auxiliary method to be created (in the rule above: `generate_logic_or_expression_aux`).
/// - `$ast_type` - the type of ast node to be handled (in the rule above: `AstLogicOrExpression`).
/// - `$ast_type_aux` - the type of auxiliary ast node to be handled (in the rule above: `AstLogicOrExpressionAux`).
/// - `$variant1` - the name of the first variant of `$ast_type` enum (in the rule above: `AstLogicOrExp::LogicAndExp`)
/// - `$variant2` - the name of the second variant of `$ast_type` enum (in the rules above: `AstLogicOrExp::LogicAndExpAux`)
/// - `$child_method` - the method used to handle the inner node (in the rule above: `generate_logic_and_expression`).
/// - `$connecting_token` - the connecting token in this derivation (in the rules above: `TokenKind::Star`, `TokenKind::Divide`, `TokenKind::Mod`)
/// - `$variant_no_rec` - the name of a non recursive variant of `$ast_type_aux` (in the rules above: `AstLogicOrExpAux::LogicOrLogicAndExp`)
/// - `$variant_rec` - the name of the recursive variant of `$ast_type_aux` corresponding to `$variant_no_rec` 
///     (in the rules above: `AstLogicOrExpAux::LogicOrLogicAndExpAux`)
/// - `$operator` - the `TacBinaryCommand` type corresponding to the operator connecting the different clauses of the rule. 
///     (in the rules above: `TacBinaryCommand::LogicOr`)
macro_rules! generate_recursive_tac {
    ($method_name: ident, $aux_method_name: ident, $ast_type: ty, $ast_type_aux: ty, $variant1: path, $variant2: path, $child_method: ident, 
        $($variant_no_rec: path, $variant_rec: path, $operator: expr),*) => {
        fn $method_name(&mut self, x: $ast_type)->TacValue{
            match x{
                $variant1(y) => self.$child_method(*y),
                $variant2(a,b) => {
                    let left = self.$child_method(*a);
                    self.$aux_method_name(*b, left)
                },
            }
        }

        fn $aux_method_name(&mut self, x: $ast_type_aux, left: TacValue)->TacValue{
            match x{
                $(
                    $variant_no_rec(y) => {
                        let right = self.$child_method(*y);
                        let dest = "_tac_var".to_string()+&self.label_counter.to_string();
                        let dest = TacValue::Variable(dest);
                        self.label_counter+=1;
                        self.command_list.push(TacCommand::BinaryCommand($operator, left, right ,dest.clone()));
                        dest
                    },
                    $variant_rec(a,b) => {
                        let right = self.$child_method(*a);
                        let dest = "_tac_var".to_string()+&self.label_counter.to_string();
                        let dest = TacValue::Variable(dest);
                        self.label_counter+=1;
                        self.command_list.push(TacCommand::BinaryCommand($operator, left, right ,dest.clone()));
                        let left = dest;
                        self.$aux_method_name(*b,left)
                    }
                )*
            }
        }
    }
}

impl TacGenerator{
    generate_recursive_tac!(generate_bit_or_expression, generate_bit_or_expression_aux, AstBitOrExp, AstBitOrExpAux, 
        AstBitOrExp::BitXorExp, AstBitOrExp::BitXorExpAux, generate_bit_xor_expression, 
        AstBitOrExpAux::BitOrBitXorExp, AstBitOrExpAux::BitOrBitXorExpAux, TacBinaryCommand::BitOr);
    
    generate_recursive_tac!(generate_bit_xor_expression, generate_bit_xor_expression_aux, AstBitXorExp, AstBitXorExpAux, 
        AstBitXorExp::BitAndExp, AstBitXorExp::BitAndExpAux, generate_bit_and_expression, 
        AstBitXorExpAux::BitXorBitAndExp, AstBitXorExpAux::BitXorBitAndExpAux, TacBinaryCommand::BitXor);

    generate_recursive_tac!(generate_bit_and_expression, generate_bit_and_expression_aux, AstBitAndExp, AstBitAndExpAux, 
        AstBitAndExp::EqualityExp, AstBitAndExp::EqualityExpAux, generate_equality_expression, 
        AstBitAndExpAux::BitAndEqualityExp, AstBitAndExpAux::BitAndEqualityExpAux, TacBinaryCommand::BitAnd);

    generate_recursive_tac!(generate_equality_expression, generate_equality_expression_aux, AstEqualityExp, AstEqualityExpAux, 
        AstEqualityExp::RelationalExp, AstEqualityExp::RelationalExpAux, generate_relational_expression, 
        AstEqualityExpAux::EqRelationalExp, AstEqualityExpAux::EqRelationalExpAux, TacBinaryCommand::Eq,
        AstEqualityExpAux::NeqRelationalExp, AstEqualityExpAux::NeqRelationalExpAux, TacBinaryCommand::Neq);

    generate_recursive_tac!(generate_relational_expression, generate_relational_expression_aux, AstRelationalExp, AstRelationalExpAux, 
        AstRelationalExp::BitShiftExp, AstRelationalExp::BitShiftExpAux, generate_bit_shift_expression, 
        AstRelationalExpAux::GeqBitShiftExp, AstRelationalExpAux::GeqBitShiftExpAux, TacBinaryCommand::Geq,
        AstRelationalExpAux::LeqBitShiftExp, AstRelationalExpAux::LeqBitShiftExpAux, TacBinaryCommand::Leq,
        AstRelationalExpAux::GreaterBitShiftExp, AstRelationalExpAux::GreaterBitShiftExpAux, TacBinaryCommand::Greater,
        AstRelationalExpAux::LessBitShiftExp, AstRelationalExpAux::LessBitShiftExpAux, TacBinaryCommand::Less
    );

    generate_recursive_tac!(generate_bit_shift_expression, generate_bit_shift_expression_aux, AstBitShiftExp, AstBitShiftExpAux, 
        AstBitShiftExp::AdditiveExp, AstBitShiftExp::AdditiveExpAux, generate_additive_exp, 
        AstBitShiftExpAux::BitShiftLeftAdditiveExp, AstBitShiftExpAux::BitShiftLeftAdditiveExpAux, TacBinaryCommand::BitShiftLeft,
        AstBitShiftExpAux::BitShiftRightAdditiveExp, AstBitShiftExpAux::BitShiftRighttAdditiveExpAux, TacBinaryCommand::BitShiftRight);

    generate_recursive_tac!(generate_additive_exp, generate_additive_exp_aux, AstAdditiveExp, AstAdditiveExpAux, 
        AstAdditiveExp::Term, AstAdditiveExp::TermAux, generate_term, 
        AstAdditiveExpAux::PlusTerm, AstAdditiveExpAux::PlusTermAux, TacBinaryCommand::Plus,
        AstAdditiveExpAux::MinusTerm, AstAdditiveExpAux::MinusTermAux, TacBinaryCommand::Minus);

    generate_recursive_tac!(generate_term, generate_term_aux, AstTerm, AstTermAux, 
        AstTerm::Factor, AstTerm::FactorAux, generate_factor, 
        AstTermAux::DivideFactor, AstTermAux::DivideFactorAux, TacBinaryCommand::Divide,
        AstTermAux::StarFactor, AstTermAux::StarFactorAux, TacBinaryCommand::Multiply,
        AstTermAux::ModFactor, AstTermAux::ModFactorAux, TacBinaryCommand::Mod
    );

    fn generate_factor(&mut self, ast_factor: AstFactor)->TacValue{
        match ast_factor {
            AstFactor::FunctionCall(ast_function_call) => {
                let dest = "_tac_var".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let dest = TacValue::Variable(dest);
                let mut args = Vec::new();
                for exp in ast_function_call.arguments{
                    args.push(self.generate_expression(exp));
                }
                self.command_list.push(TacCommand::FunctionCall(ast_function_call.id, args, dest.clone()));
                dest
            },
            AstFactor::Expression(ast_expression) => self.generate_expression(*ast_expression),
            AstFactor::UnaryOpFactor(ast_unary_op, ast_factor) => {
                let dest = "_tac_var".to_string()+&self.label_counter.to_string();
                self.label_counter+=1;
                let temp = self.generate_factor(*ast_factor);
                match ast_unary_op {
                    AstUnaryOp::Minus => self.command_list.push(TacCommand::UnaryCommand(TacUnaryCommand::Minus, temp, TacValue::Variable(dest.clone()))),
                    AstUnaryOp::Complement => self.command_list.push(TacCommand::UnaryCommand(TacUnaryCommand::Complement, temp, TacValue::Variable(dest.clone()))),
                    AstUnaryOp::LogicNegation => self.command_list.push(TacCommand::UnaryCommand(TacUnaryCommand::Not, temp, TacValue::Variable(dest.clone()))),
                }
                TacValue::Variable(dest)
            },
            AstFactor::Int(x) => {
                TacValue::Constant(x as i64)
            },
            AstFactor::Id(id) => TacValue::Variable(id),
        }
    }  
}