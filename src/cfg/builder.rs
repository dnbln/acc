use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Write as _,
};

use crate::{
    cfg::{def::*, sema::SemaResults},
    parser::{ast::VarId, span::Span},
};

pub struct CfgBuilder {
    next_bb_id: BBId,
    next_value_ref: usize,
    pub(super) blocks: Vec<BasicBlock>,
}

impl CfgBuilder {
    pub(super) fn new() -> (Self, BBId) {
        let mut builder = CfgBuilder {
            next_bb_id: BBId(0),
            next_value_ref: 0,
            blocks: Vec::new(),
        };
        let entry = builder.create_bb();
        (builder, entry)
    }

    pub(super) fn create_bb(&mut self) -> BBId {
        let bb_id = self.next_bb_id;
        self.next_bb_id.0 += 1;
        self.blocks.push(BasicBlock {
            id: bb_id,
            phi: Vec::new(),
            instructions: Vec::new(),
            tail: TailCfgInstruction::Undefined,
            successors: Vec::new(),
            predecessors: Vec::new(),
        });
        bb_id
    }

    pub(super) fn allocate_value(&mut self, span: Span, var_id: Option<VarId>) -> ValueRef {
        let val_ref = self.next_value_ref;
        self.next_value_ref += 1;
        ValueRef(val_ref, span, var_id, None)
    }

    pub(super) fn add_phi_instruction(
        &mut self,
        bb_id: BBId,
        dest: ValueRef,
        sources: BTreeMap<BBId, ValueRefOrConst>,
        var_id: Option<VarId>,
    ) {
        self.blocks[bb_id.0].phi.push(PhiCfgInstruction {
            dest,
            sources,
            var_id,
        });
    }

    pub(super) fn is_optimized_phi(instr: &CfgInstruction) -> Option<(ValueRef, BBId, ValueRef, VarId)> {
        // An optimized phi is an assignment where the destination and source spans are the same
        // (e.g. both point to the same variable reference, e.g. the declaration, VarId).
        // Assignments (new versions of the variable) will have their destination span pointing
        // to the RefId of the new assignment.
        // this happens in add_phi_source when we optimize phis for blocks with a single predecessor
        // block, we create an assignment from the predecessor's value.
        // They keep the source BBId in the ValueRef.3 field.
        if let CfgInstruction::Assign { dest, val } = instr
            && let RValue::Value(v) = val
            && dest.2 == v.2
            && let Some(var_id) = dest.2
            && dest.1 == v.1
            && let Some(source_bb) = dest.3
        {
            Some((*dest, source_bb, *v, var_id))
        } else {
            None
        }
    }

    pub(super) fn add_phi_source(
        &mut self,
        bb_id: BBId,
        var_id: VarId,
        sources: BTreeMap<BBId, ValueRefOrConst>,
        span: Span,
    ) -> (ValueRef, bool) {
        let bb = &mut self.blocks[bb_id.0];

        if bb.predecessors.len() == 1 {
            // optimization: if bb has only one predecessor, then that block dominates this one,
            // so we can just reuse the old value from the predecessor block
            assert_eq!(sources.len(), 1);

            let (b, v) = sources.first_key_value().unwrap();
            let mut dest = self.allocate_value(span, Some(var_id));
            dest.3 = Some(*b); // mark as optimized phi

            self.blocks[bb_id.0].instructions.insert(
                0,
                CfgInstruction::Assign {
                    dest,
                    val: RValue::from_value_ref_or_const(v),
                },
            );
            return (dest, true);
        }

        match bb.phi.iter_mut().find(|phi| phi.var_id == Some(var_id)) {
            Some(phi) => {
                let mut ch = false;
                for (pred_bb, val) in sources {
                    if !phi.sources.contains_key(&pred_bb) {
                        phi.sources.insert(pred_bb, val);
                        ch = true;
                    } else if phi.sources.get(&pred_bb) != Some(&val) {
                        phi.sources.insert(pred_bb, val);
                        ch = true;
                    }
                }
                (phi.dest, ch)
            }
            None => {
                let dest = self.allocate_value(span, Some(var_id));
                self.blocks[bb_id.0].phi.push(PhiCfgInstruction {
                    dest,
                    sources,
                    var_id: Some(var_id),
                });
                (dest, true)
            }
        }
    }

    pub(super) fn add_instruction(&mut self, bb_id: BBId, instr: CfgInstruction) {
        self.blocks[bb_id.0].instructions.push(instr);
    }

    pub(super) fn prepend_instruction(&mut self, bb_id: BBId, instr: CfgInstruction) {
        self.blocks[bb_id.0].instructions.insert(0, instr);
    }

    pub(super) fn bb_has_tail(&self, bb_id: BBId) -> bool {
        self.blocks[bb_id.0].tail != TailCfgInstruction::Undefined
    }

    pub(super) fn set_tail_instruction(&mut self, bb_id: BBId, tail: TailCfgInstruction) {
        if self.blocks[bb_id.0].tail != TailCfgInstruction::Undefined {
            panic!("Tail instruction for BB {:?} is already set", bb_id);
        }
        self.blocks[bb_id.0].tail = tail;
    }

    pub(super) fn link_successors_and_predecessors(
        &mut self,
        validate_errors: &mut Vec<ValidateError>,
    ) {
        self.blocks.iter_mut().for_each(|bb| {
            bb.successors.clear();
            bb.predecessors.clear();
        });

        let links = self
            .blocks
            .iter()
            .flat_map(|bb| match bb.tail {
                TailCfgInstruction::UncondBranch { target } => {
                    vec![(bb.id, target)]
                }
                TailCfgInstruction::CondBranch {
                    then_bb, else_bb, ..
                } => vec![(bb.id, then_bb), (bb.id, else_bb)],
                TailCfgInstruction::Return { .. } => vec![],
                TailCfgInstruction::Undefined => {
                    validate_errors.push(ValidateError::UndefinedTailInstruction(bb.id));
                    vec![]
                }
            })
            .collect::<Vec<_>>();

        for (from, to) in links {
            self.blocks[from.0].successors.push(to);
            self.blocks[to.0].predecessors.push(from);
        }
    }

    pub(super) fn build(self, entry: BBId) -> Result<ControlFlowGraph, Vec<ValidateError>> {
        if self.blocks.iter().all(|bb| bb.id != entry) {
            return Err(vec![ValidateError::InvalidEntryBB]);
        }

        Ok(ControlFlowGraph {
            basic_blocks: self.blocks,
            entry,
        })
    }

    pub(super) fn debug(&self) {
        let mut s = String::new();
        for bb in &self.blocks {
            writeln!(&mut s, "BB {:?}:", bb.id).unwrap();
            for phi in &bb.phi {
                writeln!(&mut s, "  Phi: {} = phi (", phi.dest).unwrap();

                for (pred_bb, val) in &phi.sources {
                    writeln!(&mut s, "    [{:?} => {}]", pred_bb, val).unwrap();
                }
                writeln!(&mut s, "  )").unwrap();
            }
            for instr in &bb.instructions {
                writeln!(&mut s, "  Instr: {}", instr).unwrap();
            }
            writeln!(&mut s, "  Tail: {}", bb.tail).unwrap();
            writeln!(&mut s, "  Successors: {:?}", bb.successors).unwrap();
            writeln!(&mut s, "  Predecessors: {:?}", bb.predecessors).unwrap();
        }
        println!("{}", s);
    }

    pub(super) fn debug_graphviz(&self, sema: &SemaResults) -> String {
        let mut output = String::new();
        output.push_str("digraph CFG {\n");
        for bb in &self.blocks {
            let s = {
                let mut s = String::new();
                writeln!(&mut s, "BB{}:", bb.id.0).unwrap();
                for phi in &bb.phi {
                    write!(&mut s, "  {} = Φ(", phi.dest).unwrap();
                    for (i, (pred_bb, val)) in phi.sources.iter().enumerate() {
                        if i > 0 {
                            write!(&mut s, ", ").unwrap();
                        }
                        write!(&mut s, "{val}@BB{}", pred_bb.0).unwrap();
                    }
                    write!(&mut s, ")").unwrap();
                    if let Some(var_id) = phi.var_id {
                        write!(&mut s, " ({})", sema.var_name(var_id)).unwrap();
                    }
                    writeln!(&mut s).unwrap();
                }
                for instr in &bb.instructions {
                    write!(&mut s, "  {}", instr).unwrap();
                    let var_id = match instr {
                        CfgInstruction::Assign { dest, .. } => dest.2,
                        CfgInstruction::_AssignVar { var_id, .. } => Some(*var_id),
                    };
                    if let Some(var_id) = var_id {
                        write!(&mut s, " ({})", sema.var_name(var_id)).unwrap();
                    }
                    writeln!(&mut s).unwrap();
                }
                match &bb.tail {
                    TailCfgInstruction::Undefined => {
                        s.push_str("undefined\n");
                    }
                    TailCfgInstruction::UncondBranch { target } => {
                        writeln!(&mut s, "  br BB{}", target.0).unwrap();
                    }
                    TailCfgInstruction::CondBranch {
                        cond,
                        then_bb,
                        else_bb,
                    } => {
                        writeln!(
                            &mut s,
                            "  br_cond {} ? BB{} : BB{}",
                            cond, then_bb.0, else_bb.0
                        )
                        .unwrap();
                    }
                    TailCfgInstruction::Return { value } => match value {
                        Some(v) => {
                            writeln!(&mut s, "  return {}", v).unwrap();
                        }
                        None => {
                            writeln!(&mut s, "  return").unwrap();
                        }
                    },
                }
                s
            };
            writeln!(output, "  BB{} [label={s:?}];", bb.id.0).unwrap();
            for succ in &bb.successors {
                writeln!(output, "  BB{} -> BB{};", bb.id.0, succ.0).unwrap();
            }
        }
        output.push_str("}\n");
        output
    }

    fn dead_blocks(&self) -> BTreeSet<BBId> {
        let mut reachable = vec![false; self.blocks.len()];
        let mut worklist = vec![self.blocks[0].id]; // entry is always BBId(0)
        reachable[0] = true;

        while let Some(bb_id) = worklist.pop() {
            let bb = &self.blocks[bb_id.0];
            for succ in &bb.successors {
                if !reachable[succ.0] {
                    reachable[succ.0] = true;
                    worklist.push(*succ);
                }
            }
        }

        self.blocks
            .iter()
            .filter_map(|bb| {
                if !reachable[bb.id.0] {
                    Some(bb.id)
                } else {
                    None
                }
            })
            .collect()
    }

    pub(super) fn trim_dead_blocks(&mut self) {
        let dead_blocks = self.dead_blocks();
        if dead_blocks.is_empty() {
            return;
        }

        self.blocks.retain(|bb| !dead_blocks.contains(&bb.id));

        // Rebuild BBId mapping
        let mut bb_id_mapping = BTreeMap::new();
        for (new_index, bb) in self.blocks.iter_mut().enumerate() {
            let old_id = bb.id;
            let new_id = BBId(new_index);
            bb.id = new_id;
            bb_id_mapping.insert(old_id, new_id);
        }

        // Update successors and predecessors
        for bb in &mut self.blocks {
            match bb.tail {
                TailCfgInstruction::UncondBranch { ref mut target } => {
                    if let Some(new_target) = bb_id_mapping.get(target) {
                        *target = *new_target;
                    }
                }
                TailCfgInstruction::CondBranch {
                    ref mut then_bb,
                    ref mut else_bb,
                    ..
                } => {
                    if let Some(new_then) = bb_id_mapping.get(then_bb) {
                        *then_bb = *new_then;
                    }
                    if let Some(new_else) = bb_id_mapping.get(else_bb) {
                        *else_bb = *new_else;
                    }
                }
                TailCfgInstruction::Return { .. } => {}
                TailCfgInstruction::Undefined => {}
            }

            bb.successors = bb
                .successors
                .iter()
                .filter_map(|succ| bb_id_mapping.get(succ).cloned())
                .collect();
            bb.predecessors = bb
                .predecessors
                .iter()
                .filter_map(|pred| bb_id_mapping.get(pred).cloned())
                .collect();
        }
    }
}

#[derive(Debug)]
pub enum ValidateError {
    InvalidEntryBB,
    UnreachableBB(BBId),
    UndefinedTailInstruction(BBId),
}
