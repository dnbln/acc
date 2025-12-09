use std::collections::BTreeMap;

use crate::{cfg::def::*, parser::{ast::VarId, span::Span}};

pub struct CfgBuilder {
    next_bb_id: BBId,
    next_value_ref: usize,
    pub(super) blocks: Vec<BasicBlock>,
}

impl CfgBuilder {
    pub(super) fn new() -> Self {
        CfgBuilder {
            next_bb_id: BBId(0),
            next_value_ref: 0,
            blocks: Vec::new(),
        }
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
        ValueRef(val_ref, span, var_id)
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

    pub(super) fn add_phi_source(
        &mut self,
        bb_id: BBId,
        var_id: VarId,
        sources: BTreeMap<BBId, ValueRefOrConst>,
        span: Span,
    ) -> (ValueRef, bool) {
        let bb = &mut self.blocks[bb_id.0];
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
            },
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

    pub(super) fn bb_has_tail(&self, bb_id: BBId) -> bool {
        self.blocks[bb_id.0].tail != TailCfgInstruction::Undefined
    }

    pub(super) fn set_tail_instruction(&mut self, bb_id: BBId, tail: TailCfgInstruction) {
        if self.blocks[bb_id.0].tail != TailCfgInstruction::Undefined {
            panic!("Tail instruction for BB {:?} is already set", bb_id);
        }
        self.blocks[bb_id.0].tail = tail;
    }

    pub(super) fn link_successors_and_predecessors(&mut self, validate_errors: &mut Vec<ValidateError>) {
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
}

#[derive(Debug)]
pub enum ValidateError {
    InvalidEntryBB,
    UnreachableBB(BBId),
    UndefinedTailInstruction(BBId),
}
