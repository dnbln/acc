use std::fmt::{self, Display, Write as _};

use crate::cfg::def::{
    CfgInstruction, ControlFlowGraph, RValue, TailCfgInstruction, ValueRef, ValueRefOrConst,
};

impl Display for ControlFlowGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for bb in &self.basic_blocks {
            writeln!(f, "{:?}: % preds = {:?}", bb.id, bb.predecessors)?;
            for phi in &bb.phi {
                write!(f, "  {} = phi(", phi.dest)?;
                for (i, (pred_bb, val)) in phi.sources.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "[ from {:?}: {} ]", pred_bb, val)?;
                }
                writeln!(f, ")")?;
            }
            for instr in &bb.instructions {
                writeln!(f, "  {}", instr)?;
            }
            writeln!(f, "  {}", bb.tail)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Display for CfgInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CfgInstruction::Assign { dest, val } => {
                write!(f, "{} = {}", dest, val)
            }
            // CfgInstruction::_Kill { var_id } => {
            //     write!(f, "kill var {:?}", var_id)
            // }
            CfgInstruction::_AssignVar { var_id, val } => {
                write!(f, "var {:?} = {}", var_id, val)
            }
        }
    }
}

impl Display for TailCfgInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TailCfgInstruction::Return { value: None } => {
                write!(f, "return")
            }
            TailCfgInstruction::Return { value: Some(value) } => {
                write!(f, "return {}", value)
            }
            TailCfgInstruction::UncondBranch { target } => {
                write!(f, "br {:?}", target)
            }
            TailCfgInstruction::CondBranch {
                cond,
                then_bb,
                else_bb,
            } => {
                write!(f, "br_cond {} ? {:?} : {:?}", cond, then_bb, else_bb)
            }
            TailCfgInstruction::Undefined => {
                write!(f, "undefined")
            }
        }
    }
}

impl Display for ValueRefOrConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueRefOrConst::Value(v) => {
                write!(f, "{}", v)
            }
            ValueRefOrConst::Const(c) => {
                write!(f, "{}", c)
            }
        }
    }
}

impl Display for RValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RValue::Const(c) => {
                write!(f, "const {}", c)
            }
            RValue::Param { param_index } => {
                write!(f, "param[{}]", param_index)
            }
            RValue::Function { name } => {
                write!(f, "function {}", name)
            }
            RValue::Value(value_ref) => {
                write!(f, "{}", value_ref)
            }
            RValue::_VarId(var_id) => {
                write!(f, "var {:?}", var_id)
            }
            RValue::Add { left, right } => {
                write!(f, "add {}, {}", left, right)
            }
            RValue::Sub { left, right } => {
                write!(f, "sub {}, {}", left, right)
            }
            RValue::Mul { left, right } => {
                write!(f, "mul {}, {}", left, right)
            }
            RValue::Div { left, right } => {
                write!(f, "div {}, {}", left, right)
            }
            RValue::Modulus { left, right } => {
                write!(f, "mod {}, {}", left, right)
            }
            RValue::EqCheck { left, right } => {
                write!(f, "eq {}, {}", left, right)
            }
            RValue::NEqCheck { left, right } => {
                write!(f, "neq {}, {}", left, right)
            }
            RValue::LtCheck { left, right } => {
                write!(f, "lt {}, {}", left, right)
            }
            RValue::GtCheck { left, right } => {
                write!(f, "gt {}, {}", left, right)
            }
            RValue::LEqCheck { left, right } => {
                write!(f, "leq {}, {}", left, right)
            }
            RValue::GEqCheck { left, right } => {
                write!(f, "geq {}, {}", left, right)
            }
            RValue::BitwiseAnd { left, right } => {
                write!(f, "and {}, {}", left, right)
            }
            RValue::BitwiseOr { left, right } => {
                write!(f, "or {}, {}", left, right)
            }
            RValue::BitwiseXor { left, right } => {
                write!(f, "xor {}, {}", left, right)
            }
            RValue::BitShiftLeft { left, right } => {
                write!(f, "shl {}, {}", left, right)
            }
            RValue::BitShiftRight { left, right } => {
                write!(f, "shr {}, {}", left, right)
            }
            RValue::BitwiseNot { expr } => {
                write!(f, "not {}", expr)
            }
            RValue::Call { func, args } => {
                write!(f, "call {}(", func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write!(f, "%{}({:?}", self.0, self.1)?;
        // if let Some(var_id) = self.2 {
        //     write!(f, ", var {:?}", var_id.get_id())?;
        // }
        // write!(f, ")")
        write!(f, "%{}", self.0)
    }
}

pub fn graphviz(cfg: &ControlFlowGraph) -> String {
    let mut output = String::new();
    output.push_str("digraph CFG {\n");
    for bb in &cfg.basic_blocks {
        let s = {
            let mut s = String::new();
            writeln!(&mut s, "BB{}:", bb.id.0).unwrap();
            for phi in &bb.phi {
                write!(&mut s, "  {} = φ(", phi.dest).unwrap();
                for (i, (pred_bb, val)) in phi.sources.iter().enumerate() {
                    if i > 0 {
                        write!(&mut s, ", ").unwrap();
                    }
                    write!(&mut s, "{val}@BB{}", pred_bb.0).unwrap();
                }
                writeln!(&mut s, ")").unwrap();
            }
            for instr in &bb.instructions {
                writeln!(&mut s, "  {}", instr).unwrap();
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
                TailCfgInstruction::Return { value } => {
                    match value {
                        Some(v) => {
                            writeln!(&mut s, "  return {}", v).unwrap();
                        }
                        None => {
                            writeln!(&mut s, "  return").unwrap();
                        }
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
