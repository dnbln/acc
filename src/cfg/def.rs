//! Definition of the control flow graph (CFG) structures.

use std::{
    collections::{BTreeMap, BTreeSet},
    hash::Hash,
};

use crate::parser::{ast::VarId, span::Span};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BBId(pub(crate) usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicBlock {
    pub(crate) id: BBId,
    /// Phi instructions at the start of the basic block
    pub(crate) phi: Vec<PhiCfgInstruction>,
    /// Actual instructions in the basic block
    pub(crate) instructions: Vec<CfgInstruction>,
    /// Tail instruction (branch/return)
    pub(crate) tail: TailCfgInstruction,
    /// Successor basic blocks (determined from tail instruction)
    pub(super) successors: Vec<BBId>,
    /// Predecessor basic blocks (determined from successors of all blocks)
    pub(super) predecessors: Vec<BBId>,
}

#[derive(Clone, Copy, Eq, Ord, Debug)]
pub struct ValueRef(
    pub(crate) usize,
    pub(super) Span,
    pub(crate) Option<VarId>,
    pub(super) Option<BBId>,
);

impl PartialEq for ValueRef {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialOrd for ValueRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

impl Hash for ValueRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhiCfgInstruction {
    pub dest: ValueRef,
    pub sources: BTreeMap<BBId, ValueRefOrConst>,
    pub var_id: Option<VarId>,
    pub phi_type: PhiType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PhiType {
    Bool,
    Int,
    Infer,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueRefOrConst {
    Value(ValueRef),
    Const(i64),
    ConstBool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CfgInstruction {
    Assign {
        dest: ValueRef,
        val: RValue,
    },
    /// Variable assignment, temporary representation before / during SSA conversion
    _AssignVar {
        var_id: VarId,
        val: RValue,
    },
}

impl CfgInstruction {
    pub(super) fn is_assignment_to_live(&self, live_values: &BTreeSet<ValueRef>) -> bool {
        match self {
            CfgInstruction::Assign { dest, .. } => live_values.contains(dest),
            CfgInstruction::_AssignVar { .. } => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RValue {
    Const(i64),
    ConstBool(bool),
    Param {
        param_index: usize,
    },
    Function {
        var_id: VarId,
    },
    Value(ValueRef),
    /// Direct variable reference, temporary representation before / during SSA conversion
    _VarId(VarId),
    Add {
        left: ValueRef,
        right: ValueRef,
    },
    Sub {
        left: ValueRef,
        right: ValueRef,
    },
    Mul {
        left: ValueRef,
        right: ValueRef,
    },
    Div {
        left: ValueRef,
        right: ValueRef,
    },
    Modulus {
        left: ValueRef,
        right: ValueRef,
    },
    EqCheck {
        left: ValueRef,
        right: ValueRef,
    },
    NEqCheck {
        left: ValueRef,
        right: ValueRef,
    },
    LtCheck {
        left: ValueRef,
        right: ValueRef,
    },
    GtCheck {
        left: ValueRef,
        right: ValueRef,
    },
    LEqCheck {
        left: ValueRef,
        right: ValueRef,
    },
    GEqCheck {
        left: ValueRef,
        right: ValueRef,
    },
    BitwiseAnd {
        left: ValueRef,
        right: ValueRef,
    },
    BitwiseOr {
        left: ValueRef,
        right: ValueRef,
    },
    BitwiseXor {
        left: ValueRef,
        right: ValueRef,
    },
    BitShiftLeft {
        left: ValueRef,
        right: ValueRef,
    },
    BitShiftRight {
        left: ValueRef,
        right: ValueRef,
    },
    BitwiseNot {
        expr: ValueRef,
    },
    Call {
        func: ValueRef,
        args: Vec<ValueRef>,
    },
    Select {
        cond: ValueRef,
        then_val: ValueRef,
        else_val: ValueRef,
    },
}

impl RValue {
    pub fn from_value_ref_or_const(v: &ValueRefOrConst) -> Self {
        match v {
            ValueRefOrConst::Value(val_ref) => RValue::Value(*val_ref),
            ValueRefOrConst::Const(c) => RValue::Const(*c),
            ValueRefOrConst::ConstBool(b) => RValue::ConstBool(*b),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TailCfgInstruction {
    Undefined,
    UncondBranch {
        target: BBId,
    },
    CondBranch {
        cond: ValueRef,
        then_bb: BBId,
        else_bb: BBId,
    },
    Return {
        value: Option<ValueRef>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ControlFlowGraph {
    pub basic_blocks: Vec<BasicBlock>,
    pub entry: BBId,
}
