use rs;

#[derive(Default)]
struct InterpreterState {
    stack: Vec<rs::TokenStream>,
}

impl LowerInstructionList {
    pub fn generate(self) -> rs::TokenStream {
        let mut state = InterpreterState::default();
        for instruction in self.list {
            run_instruction(&mut state, instruction);
        }
        let result = state.stack.pop().expect("interpreter expected at least one result");
        assert!(state.stack.is_empty(), "interpreter expected exactly one result");
        result
    }
}

impl InterpreterState {
    fn pop(&mut self) -> rs::TokenStream {
        self.stack.pop().expect("stack is empty")
    }

    fn pop_n(&mut self, count: usize) -> Vec<rs::TokenStream> {
        let n = self.stack.len().checked_sub(count).expect("stack underflow??");
        self.stack.split_off(n)
    }

    fn peek(&mut self) -> rs::TokenStream {
        self.stack.last().expect("stack is empty").clone()
    }
}

fn run_instruction(state: &mut InterpreterState, instruction: LowerInstruction) {
    let val = eval_instruction(state, instruction);
    state.stack.push(val);
}
