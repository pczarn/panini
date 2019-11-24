use panini_logic::output::Instruction;
use output::instruction::LowerInstruction;

fn translate(ir: Ir, tables: Tables, instructions: Vec<Instruction>) -> Vec<LowerInstruction> {
    instruction::translate(ir, tables, instructions)
}
