// LATM handling.
struct Checkpoint<Leaf, V> where V: Copy {
    finished_node: Option<NodeRef<'g, 'g, Leaf, V>>,
    scanned: Vec<(Symbol, Leaf)>,
    num_scanned: Vec<usize>,
    current_num_scanned: usize,
}

impl Checkpoint<Leaf, V> {
    fn scan(token: Leaf, value: V) {
        self.scanned.push((token, value));
    }

    fn end_earleme(&mut self) {
      self.num_scanned.push(self.current_num_scanned);
      self.current_num_scanned = 0;
    }

    fn checkpoint(&mut self, node: NodeRef<'g, 'g, Leaf, V>) {
        self.finished_node = Some(node);
        self.scanned.clear();
        self.num_scanned.clear();
    }

    fn take_scanned(&mut self) -> Vec<usize> {
        mem::replace(&mut self.scanned, vec![])
    }

    fn is_present(&self) -> bool {
        self.finished_node.is_some()
    }

    fn is_empty(&self) -> bool {
        self.scanned.is_empty()
    }
}


