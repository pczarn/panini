use panini_logic::output::Instruction;
use instruction;

fn translate(ir: Ir, instructions: Vec<Instruction>) -> Vec<LowerInstruction> {
    instruction::translate(ir, instructions)
}
