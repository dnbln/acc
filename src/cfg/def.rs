//! Definition of the control flow graph (CFG) structures.

use std::collections::BTreeMap;

use crate::parser::{ast::VarId, span::Span};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct BBId(pub(super) usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicBlock {
    pub(super) id: BBId,
    pub(super) phi: Vec<PhiCfgInstruction>,
    pub(super) instructions: Vec<CfgInstruction>,
    pub(super) tail: TailCfgInstruction,
    pub(super) successors: Vec<BBId>,
    pub(super) predecessors: Vec<BBId>,
}

#[derive(Clone, Copy, Eq, Ord, Hash, Debug)]
pub struct ValueRef(pub(super) usize, pub(super) Span, pub(super) Option<VarId>);

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhiCfgInstruction {
    pub dest: ValueRef,
    pub sources: BTreeMap<BBId, ValueRefOrConst>,
    pub var_id: Option<VarId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueRefOrConst {
    Value(ValueRef),
    Const(i64),
    /// Temporary unknown value, used during SSA construction
    _TempUnknown,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RValue {
    Const(i64),
    Param {
        param_index: usize,
    },
    Function {
        name: String,
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

