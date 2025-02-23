/// Implements an asl generator `Generator`.
/// `Generator` gets the output of `TacGenerator` defined in tac.rs and outputs a `String`
/// containing x86 assembly code.

use std::collections::{HashMap, HashSet};

use crate::tac::*;

const WORDSIZE:i32 = 8;

pub struct Generator{

}

enum VariableLocation {
    Stack(i32),
    Register(String),
}

impl Generator{
    pub fn new()->Self{
        Self {

        }
    }

    pub fn generate_assembly(&self, functions: Vec<(Vec<TacCommand>,HashSet<String>)>)->String{
        let mut assembly = String::new();
        for (commands, variables) in functions{
            assembly+=&self.generate_function(commands,variables);
        }
        assembly
    }
}

impl Generator {
    fn generate_function(&self, commands: Vec<TacCommand>, variables: HashSet<String>)->String{
        let mut variable_location_map = HashMap::new();
        let mut function_label;
        let mut ctr=1;
        let mut end_label=String::new();
        let mut default_return_value=String::new();
        let mut variable_deallocation=String::new();
        let epilogue = "\tpop\t%rbp\n\tret\n".to_string();
        let mut ans =String::new();
        for command in commands{
            match command {
                TacCommand::FunctionStart(id, args ) => {
                    function_label = id.clone();
                    end_label = "_".to_string()+&function_label+"_end";
                    let prologue = "\t.global ".to_string()+&function_label+"\n"+&function_label+":\n\tpush\t%rbp\n\tmov\t%rsp, %rbp\n";
                    default_return_value = "\tmov\t$0, %rax\n".to_string()+&end_label+&":\n".to_string();
                    for arg in args.iter(){
                        let location=VariableLocation::Stack(-ctr-1);
                        ctr+=1;
                        variable_location_map.insert(arg.clone(), location);
                    }
                    ctr=1;
                    for var in variables.iter(){
                        if !variable_location_map.contains_key(var){
                            variable_location_map.insert(var.clone(), VariableLocation::Stack(ctr));
                            ctr+=1;
                        }
                    }
                    let variable_allocation = "\tadd\t$-".to_string()+&((ctr-1)*WORDSIZE).to_string()+", %rsp\n";
                    variable_deallocation = "\tadd\t$".to_string()+&((ctr-1)*WORDSIZE).to_string()+", %rsp\n";
                    ans = prologue+&variable_allocation;
                },
                _ => ans+=&self.generate_command(command, &variable_location_map, &end_label),
            }
        }
        ans+&default_return_value+&variable_deallocation+&epilogue
    }

    fn generate_command(&self, command: TacCommand, variable_location_map: &HashMap<String, VariableLocation>, function_end_label: &String)->String{
        match command {
            TacCommand::PlaceHolder => panic!("Shouldn't be placeholders here!"),
            TacCommand::Pass => "".to_string(),
            TacCommand::Bad => panic!("Compilation Failed! Invalid constant."),
            TacCommand::FunctionStart(_, _) => panic!("Function start is handled in generate_function."),
            TacCommand::FunctionEnd => "".to_string(),
            TacCommand::Jump(label) => "\tjmp\t".to_string()+&label+"\n",
            TacCommand::JumpIfZero(tac_value, label) => {
                match tac_value {
                    TacValue::Constant(x) => if x==0{
                        "\tjmp\t".to_string()+&label+"\n"
                    }
                    else{
                        "".to_string()
                    },
                    TacValue::Variable(id) => {
                        let src = variable_location_map.get(&id).unwrap();
                        match src {
                            VariableLocation::Stack(stack_index) => {
                                "\tmov\t-".to_string()+&(stack_index*WORDSIZE).to_string()+"(%rbp), %rax\n\tcmp\t$0,%rax\n\tje\t"+&label+"\n"
                            },
                            VariableLocation::Register(register) => {
                                "\tmov\t".to_string()+register+", %rax\n\tcmp\t$0,%rax\n\tje\t"+&label+"\n"
                            },
                        }
                    },
                }
            },
            TacCommand::JumpIfNotZero(tac_value, label) => {
                match tac_value {
                    TacValue::Constant(x) => if x!=0{
                        "\tjmp\t".to_string()+&label+"\n"
                    }
                    else{
                        "".to_string()
                    },
                    TacValue::Variable(id) => {
                        let src = variable_location_map.get(&id).unwrap();
                        match src {
                            VariableLocation::Stack(stack_index) => {
                                "\tmov\t-".to_string()+&(stack_index*WORDSIZE).to_string()+"(%rbp), %rax\n\tcmp\t$0,%rax\n\tjne\t"+&label+"\n"
                            },
                            VariableLocation::Register(register) => {
                                "\tmov\t%".to_string()+register+", %rax\n\tcmp\t$0,%rax\n\tjne\t"+&label+"\n"
                            },
                        }
                    },
                }
            },
            TacCommand::Label(label) => label+":\n",
            TacCommand::Return(tac_value) => {
                match tac_value {
                    TacValue::Constant(x) => "\tmov\t$".to_string()+&x.to_string()+", %rax\n\tjmp\t"+function_end_label+"\n",
                    TacValue::Variable(id) => {
                        let src = variable_location_map.get(&id).unwrap();
                        match src {
                            VariableLocation::Stack(stack_index) => {
                                "\tmov\t-".to_string()+&(stack_index*WORDSIZE).to_string()+"(%rbp), %rax\n\tjmp\t"+function_end_label+"\n"
                            },
                            VariableLocation::Register(register) => {
                                "\tmov\t%".to_string()+register+", %rax\n\tjmp\t"+function_end_label+"\n"
                            },
                        }
                    },
                }
            },
            TacCommand::Copy(tac_value, tac_value1) => {
                let temp;
                match tac_value {
                    TacValue::Constant(x) => {temp = "\tmov\t$".to_string()+&x.to_string()+", %rax\n";},
                    TacValue::Variable(id) => {
                        let src = variable_location_map.get(&id).unwrap();
                        match src {
                            VariableLocation::Stack(stack_index) => {temp="\tmov\t-".to_string()+&(stack_index*WORDSIZE).to_string()+"(%rbp), %rax\n";},
                            VariableLocation::Register(register) => {temp="\tmov\t%".to_string()+register+", %rax\n";},
                        }
                    },
                }
                match tac_value1 {
                    TacValue::Constant(_) => panic!("Can't copy to a constant!"),
                    TacValue::Variable(id) => {
                        let dst = variable_location_map.get(&id).unwrap();
                        match dst {
                            VariableLocation::Stack(stack_index) => temp+"\tmov\t%rax, -"+&(stack_index*WORDSIZE).to_string()+"(%rbp)\n",
                            VariableLocation::Register(register) => temp+"\tmov\t%rax, %"+register+"\n",
                        }
                    },
                }
            },
            TacCommand::BinaryCommand(command, left_side, right_side, dst) => 
                self.handle_binary_command(command, left_side, right_side, dst, variable_location_map),
            TacCommand::UnaryCommand(command, src, dst) => 
                self.handle_unary_command(command, src, dst, variable_location_map),
            TacCommand::FunctionCall(id, arguments, dst) => 
                self.handle_function_call(id, arguments, dst, variable_location_map),
        }
    }

    fn handle_binary_command(&self, command: TacBinaryCommand, left_side: TacValue, right_side: TacValue, dst: TacValue,
         variable_location_map: &HashMap<String, VariableLocation>)->String{
            let left_side_str;
            match left_side {
                TacValue::Constant(x) => {left_side_str = "$".to_string()+&x.to_string();},
                TacValue::Variable(id) => {
                    let var = variable_location_map.get(&id).unwrap();
                    match var {
                        VariableLocation::Stack(stack_index) => {left_side_str="-".to_string()+&(stack_index*WORDSIZE).to_string()+"(%rbp)"},
                        VariableLocation::Register(register) => {left_side_str="%".to_string()+register},
                    }
                },
            }
            let right_side_str;
            match right_side {
                TacValue::Constant(x) => {right_side_str = "$".to_string()+&x.to_string();},
                TacValue::Variable(id) => {
                    let var = variable_location_map.get(&id).unwrap();
                    match var {
                        VariableLocation::Stack(stack_index) => {right_side_str="-".to_string()+&(stack_index*WORDSIZE).to_string()+"(%rbp)"},
                        VariableLocation::Register(register) => {right_side_str="%".to_string()+register},
                    }
                },
            }
            let dst_str;
            match dst {
                TacValue::Constant(_) => {panic!("Can't move to a constant.");},
                TacValue::Variable(id) => {
                    let var = variable_location_map.get(&id).unwrap();
                    match var {
                        VariableLocation::Stack(stack_index) => {dst_str="-".to_string()+&(stack_index*WORDSIZE).to_string()+"(%rbp)"},
                        VariableLocation::Register(register) => {dst_str="%".to_string()+register},
                    }
                },
            }
            match command {
                TacBinaryCommand::BitOr => 
                    "\tmov\t".to_string()+&left_side_str+", %rcx\n\tmov\t"+&right_side_str+", %rax\n\tor\t%rcx, %rax\n\tmov\t%rax, "+&dst_str+"\n",
                TacBinaryCommand::BitXor => 
                    "\tmov\t".to_string()+&left_side_str+", %rcx\n\tmov\t"+&right_side_str+", %rax\n\txor\t%rcx, %rax\n\tmov\t%rax, "+&dst_str+"\n",
                TacBinaryCommand::BitAnd =>
                    "\tmov\t".to_string()+&left_side_str+", %rax\n\tmov\t"+&right_side_str+", %rcx\n\tand\t%rcx, %rax\n\tmov\t%rax, "+&dst_str+"\n",
                TacBinaryCommand::Eq => 
                    "\tmov\t".to_string()+&right_side_str+", %rcx\n\tmov\t"+&left_side_str+
                    ", %rax\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsete\t%al\n\tmov\t%rax, "+&dst_str+"\n",
                TacBinaryCommand::Neq => 
                    "\tmov\t".to_string()+&right_side_str+", %rcx\n\tmov\t"+&left_side_str+
                    ", %rax\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetne\t%al\n\tmov\t%rax, "+&dst_str+"\n",
                TacBinaryCommand::Geq => 
                    "\tmov\t".to_string()+&right_side_str+", %rcx\n\tmov\t"+&left_side_str+
                    ", %rax\n\tcmp\t%rcx, %rax\n\tmov\t$0, %rax\n\tsetl\t%al\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n\tmov\t%rax, "+
                    &dst_str+"\n",
                TacBinaryCommand::Leq => 
                    "\tmov\t".to_string()+&left_side_str+", %rcx\n\tmov\t"+&right_side_str+
                    ", %rax\n\tcmp\t%rcx, %rax\n\tmov\t$0, %rax\n\tsetl\t%al\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n\tmov\t%rax, "+
                    &dst_str+"\n",
                TacBinaryCommand::Greater => 
                    "\tmov\t".to_string()+&right_side_str+", %rcx\n\tmov\t"+&left_side_str+
                    ", %rax\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetl\t%al\n\tmov\t%rax, "+
                    &dst_str+"\n",
                TacBinaryCommand::Less => 
                    "\tmov\t".to_string()+&left_side_str+", %rcx\n\tmov\t"+&right_side_str+
                    ", %rax\n\tcmp\t%rax, %rcx\n\tmov\t$0, %rax\n\tsetl\t%al\n\tmov\t%rax, "+
                    &dst_str+"\n",
                TacBinaryCommand::BitShiftLeft =>
                    "\tmov\t".to_string()+&right_side_str+", %rcx\n\tmov\t"+&left_side_str+", %rax\n\tshl\t%cl,%rax\n\tmov\t%rax, "+&dst_str+"\n",
                TacBinaryCommand::BitShiftRight => 
                    "\tmov\t".to_string()+&right_side_str+", %rcx\n\tmov\t"+&left_side_str+", %rax\n\tshr\t%cl,%rax\n\tmov\t%rax, "+&dst_str+"\n",
                TacBinaryCommand::Plus => 
                    "\tmov\t".to_string() + &left_side_str+", %rax\n\tadd\t"+&right_side_str+", %rax\n\tmov\t%rax, "+&dst_str+"\n",
                TacBinaryCommand::Minus => 
                    "\tmov\t".to_string()+&left_side_str+", %rax\n\tsub\t"+&right_side_str+", %rax\n\tmov\t%rax,"+&dst_str+"\n",
                TacBinaryCommand::Multiply => 
                    "\tmov\t".to_string()+&left_side_str+", %rax\n\tmov\t"+&right_side_str+", %r10\n\tmul\t%r10\n\tmov\t%rax, "+&dst_str+"\n",
                TacBinaryCommand::Divide => 
                    "\tmov\t".to_string()+&right_side_str+", %rcx\n\tmov\t"+&left_side_str+", %rax\n\tcdq\n\tidiv\t%rcx\n\tmov\t%rax, " + &dst_str+"\n",
                TacBinaryCommand::Mod => 
                    "\tmov\t".to_string()+&right_side_str+", %rcx\n\tmov\t"+&left_side_str+", %rax\n\tcdq\n\tidiv\t%rcx\n\tmov\t%rdx, " + &dst_str+"\n",
            }
    }

    fn handle_unary_command(&self, command: TacUnaryCommand, src: TacValue, dst: TacValue,
         variable_location_map: &HashMap<String, VariableLocation>)->String{
            let src_str;
            match src {
                TacValue::Constant(x) => {src_str="$".to_string()+&x.to_string();},
                TacValue::Variable(id) => {
                    let var = variable_location_map.get(&id).unwrap();
                    match var {
                        VariableLocation::Stack(stack_index) => {src_str="-".to_string()+&(stack_index*WORDSIZE).to_string()+"(%rbp)"},
                        VariableLocation::Register(register) => {src_str="%".to_string()+register},
                    }
                },
            }
            let dst_str;
            match dst {
                TacValue::Constant(_) => {panic!("Can't move to a constant.");},
                TacValue::Variable(id) => {
                    let var = variable_location_map.get(&id).unwrap();
                    match var {
                        VariableLocation::Stack(stack_index) => {dst_str="-".to_string()+&(stack_index*WORDSIZE).to_string()+"(%rbp)"},
                        VariableLocation::Register(register) => {dst_str="%".to_string()+register},
                    }
                },
            }
            match command {
                TacUnaryCommand::Minus => 
                    "\tmov\t".to_string()+&src_str+", %rax\n\tneg\t%rax\n\tmov\t%rax, "+&dst_str+"\n",
                TacUnaryCommand::Not => 
                    "\tmov\t".to_string()+&src_str+", %rax\n\tcmp\t$0, %rax\n\tmov\t$0, %rax\n\tsete\t%al\n\tmov\t%rax,"+&dst_str+"\n",
                TacUnaryCommand::Complement => 
                    "\tmov\t".to_string()+&src_str+", %rax\n\tnot\t%rax\n\tmov\t%rax, "+&dst_str+"\n",
            }
    }

    fn handle_function_call(&self, id: String, arguments: Vec<TacValue>, dst: TacValue, variable_location_map: &HashMap<String, VariableLocation>)->String{
        let mut s = String::new();
        let num_arguments = arguments.len();
        let registers = vec!["%rcx", "%rdx", "%r8", "%r9"];
        //In windows x64 calling convention, the first 4 arguments are passed in the registers (rcx,rdx,r8,r9), and any more are passed on the stack.
        for i in 0..usize::min(num_arguments,registers.len()){
            match &arguments[i] {
                TacValue::Constant(x) => s=s+"\tmov\t$"+&x.clone().to_string()+", "+registers[i]+"\n",
                TacValue::Variable(var_id) => {
                    let var_loc = variable_location_map.get(var_id).unwrap();
                    match var_loc {
                        VariableLocation::Stack(y) => s=s+"\tmov\t-"+&(y*WORDSIZE).to_string()+"(%rbp), "+registers[i]+"\n",
                        VariableLocation::Register(reg) => s=s+"\tmov\t"+reg+", "+registers[i]+"\n",
                    }
                },
            }
        }
        if num_arguments<4{
            s+="\tadd\t$-";                 //In windows x64 calling convention, the caller must allocate 32 bytes of space on
            s+=&((4-num_arguments)*WORDSIZE as usize).to_string();  //the stack right before the function call.
            s+=", %rsp\n";
        }
        for arg in arguments.iter().rev(){
            match arg {
                TacValue::Constant(x) => s=s+"\tpush\t$"+&x.to_string()+"\n",
                TacValue::Variable(var_id) => {
                    let var_loc = variable_location_map.get(var_id).unwrap();
                    match var_loc {
                        VariableLocation::Stack(y) => s=s+"\tpush\t-"+&(y*WORDSIZE).to_string()+"(%rbp)\n",
                        VariableLocation::Register(reg) => s=s+"\tpush\t"+reg+"\n",
                    }
                },
            }
        }
        s=s+"\tcall\t"+&id+"\n\tadd\t$"+&(i32::max(num_arguments as i32, 4)*WORDSIZE).to_string()+", %rsp\n";
        match dst {
            TacValue::Constant(_) => panic!("Can't move to a constant!"),
            TacValue::Variable(var_id) => {
                let var_loc = variable_location_map.get(&var_id).unwrap();
                match var_loc {
                    VariableLocation::Stack(y) => s=s+"\tmov\t%rax, -"+&(y*WORDSIZE).to_string()+"(%rbp)\n",
                    VariableLocation::Register(reg) => s=s+"\tmov\t%rax, "+reg+"\n",
                }
            },
        }
        s
   }

   
}