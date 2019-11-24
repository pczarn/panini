use output::instruction::LowerInstruction;
use panini_logic::output::Instruction;

fn translate(ir: Ir, tables: Tables, instructions: Vec<Instruction>) -> Vec<LowerInstruction> {
    instruction::translate(ir, tables, instructions)
}
