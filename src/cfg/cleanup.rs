use std::collections::{BTreeMap, BTreeSet};

use crate::cfg::{builder::CfgBuilder, def::{CfgInstruction, RValue, TailCfgInstruction, ValueRef, ValueRefOrConst}};

fn phi_simplification(builder: &mut CfgBuilder) -> bool {
    // remove any phi instructions that have all sources the same
    let mut to_add = Vec::new();
    for bb in &mut builder.blocks {
        let simplified_phis = bb
            .phi
            .extract_if(0..bb.phi.len(), |phi| {
                let first_source = phi.sources.values().next().unwrap();
                phi.sources.values().all(|v| v == first_source)
            })
            .collect::<Vec<_>>();
        for phi in simplified_phis {
            let first_source = phi.sources.values().next().unwrap();
            let instr = CfgInstruction::Assign {
                dest: phi.dest,
                val: match first_source {
                    ValueRefOrConst::Value(v) => RValue::Value(*v),
                    ValueRefOrConst::Const(c) => RValue::Const(*c),
                },
            };
            to_add.push((bb.id, instr));
        }
    }

    let changed = !to_add.is_empty();

    for (bb_id, instr) in to_add {
        builder.prepend_instruction(bb_id, instr);
    }

    changed
}

fn val_inliner(builder: &mut CfgBuilder) -> bool {
    // inline values that are assigned only once and used only once
    let mut equalities = BTreeMap::<ValueRef, ValueRef>::new();
    for bb in &builder.blocks {
        for instr in &bb.instructions {
            if let CfgInstruction::Assign { dest, val } = instr
                && let RValue::Value(v) = val
            {
                equalities.insert(*dest, *v);
            }
        }
    }

    let mut changed = false;

    for bb in &mut builder.blocks {
        for phi in &mut bb.phi {
            for source in phi.sources.values_mut() {
                if let ValueRefOrConst::Value(v) = source {
                    let mut current = *v;
                    while let Some(next) = equalities.get(&current) {
                        current = *next;
                    }
                    if v != &current {
                        *v = current;
                        changed = true;
                    }
                }
            }
        }
        for instr in &mut bb.instructions {
            if let CfgInstruction::Assign { dest: _, val } = instr {
                match val {
                    RValue::Value(v) => {
                        let mut current = *v;
                        while let Some(next) = equalities.get(&current) {
                            current = *next;
                        }
                        if v != &current {
                            *v = current;
                            changed = true;
                        }
                    }
                    RValue::Add { left, right }
                    | RValue::Sub { left, right }
                    | RValue::Mul { left, right }
                    | RValue::Div { left, right }
                    | RValue::Modulus { left, right }
                    | RValue::EqCheck { left, right }
                    | RValue::NEqCheck { left, right }
                    | RValue::LtCheck { left, right }
                    | RValue::GtCheck { left, right }
                    | RValue::LEqCheck { left, right }
                    | RValue::GEqCheck { left, right }
                    | RValue::BitwiseAnd { left, right }
                    | RValue::BitwiseOr { left, right }
                    | RValue::BitwiseXor { left, right }
                    | RValue::BitShiftLeft { left, right }
                    | RValue::BitShiftRight { left, right } => {
                        let mut current_left = *left;
                        while let Some(next) = equalities.get(&current_left) {
                            current_left = *next;
                        }
                        if left != &current_left {
                            *left = current_left;
                            changed = true;
                        }
                        let mut current_right = *right;
                        while let Some(next) = equalities.get(&current_right) {
                            current_right = *next;
                        }
                        if right != &current_right {
                            *right = current_right;
                            changed = true;
                        }
                    }
                    RValue::Call { func, args } => {
                        let mut current_func = *func;
                        while let Some(next) = equalities.get(&current_func) {
                            current_func = *next;
                        }
                        if func != &current_func {
                            *func = current_func;
                            changed = true;
                        }
                        for arg in args {
                            let mut current_arg = *arg;
                            while let Some(next) = equalities.get(&current_arg) {
                                current_arg = *next;
                            }
                            if arg != &current_arg {
                                *arg = current_arg;
                                changed = true;
                            }
                        }
                    }
                    RValue::Const(_) => {}
                    RValue::Param { .. } => {}
                    RValue::Function { .. } => {}
                    RValue::_VarId(_) => {}
                    RValue::BitwiseNot { expr } => {
                        let mut current_expr = *expr;
                        while let Some(next) = equalities.get(&current_expr) {
                            current_expr = *next;
                        }
                        if expr != &current_expr {
                            *expr = current_expr;
                            changed = true;
                        }
                    }
                }
            }
        }

        match &mut bb.tail {
            TailCfgInstruction::CondBranch {
                cond,
                then_bb: _,
                else_bb: _,
            } => {
                let mut current = *cond;
                while let Some(next) = equalities.get(&current) {
                    current = *next;
                }
                if cond != &current {
                    *cond = current;
                    changed = true;
                }
            }
            TailCfgInstruction::UncondBranch { target: _ } => {}
            TailCfgInstruction::Return { value } => {
                if let Some(v) = value {
                    let mut current = *v;
                    while let Some(next) = equalities.get(&current) {
                        current = *next;
                    }
                    if v != &current {
                        *v = current;
                        changed = true;
                    }
                }
            }
            TailCfgInstruction::Undefined => {
                unreachable!()
            }
        }
    }

    changed
}

fn live_values_analysis(builder: &mut CfgBuilder) -> BTreeSet<ValueRef> {
    let mut live_values = BTreeSet::new();

    loop {
        let mut changed = false;

        for bb in &builder.blocks {
            match &bb.tail {
                TailCfgInstruction::CondBranch {
                    cond,
                    then_bb: _,
                    else_bb: _,
                } => {
                    if !live_values.contains(cond) {
                        live_values.insert(*cond);
                        changed = true;
                    }
                }
                TailCfgInstruction::UncondBranch { target: _ } => {}
                TailCfgInstruction::Return { value } => {
                    if let Some(v) = value {
                        if !live_values.contains(v) {
                            live_values.insert(*v);
                            changed = true;
                        }
                    }
                }
                TailCfgInstruction::Undefined => {
                    unreachable!()
                }
            }

            for instr in bb.instructions.iter().rev() {
                match instr {
                    CfgInstruction::Assign { dest, val } => {
                        if matches!(val, RValue::Call { .. }) {
                            // function calls are assumed to have side effects, so their dest is always live
                            if !live_values.contains(dest) {
                                live_values.insert(*dest);
                                changed = true;
                            }
                        }

                        if !live_values.contains(dest) {
                            continue;
                        }

                        match val {
                            RValue::Value(v) => {
                                if !live_values.contains(v) {
                                    live_values.insert(*v);
                                    changed = true;
                                }
                            }
                            RValue::Const(_) => {}
                            RValue::Add { left, right }
                            | RValue::Sub { left, right }
                            | RValue::Mul { left, right }
                            | RValue::Div { left, right }
                            | RValue::Modulus { left, right }
                            | RValue::EqCheck { left, right }
                            | RValue::NEqCheck { left, right }
                            | RValue::LtCheck { left, right }
                            | RValue::GtCheck { left, right }
                            | RValue::LEqCheck { left, right }
                            | RValue::GEqCheck { left, right }
                            | RValue::BitwiseAnd { left, right }
                            | RValue::BitwiseOr { left, right }
                            | RValue::BitwiseXor { left, right }
                            | RValue::BitShiftLeft { left, right }
                            | RValue::BitShiftRight { left, right } => {
                                if !live_values.contains(left) {
                                    live_values.insert(*left);
                                    changed = true;
                                }
                                if !live_values.contains(right) {
                                    live_values.insert(*right);
                                    changed = true;
                                }
                            }
                            RValue::BitwiseNot { expr } => {
                                if !live_values.contains(expr) {
                                    live_values.insert(*expr);
                                    changed = true;
                                }
                            }
                            RValue::Call { func, args } => {
                                if !live_values.contains(func) {
                                    live_values.insert(*func);
                                    changed = true;
                                }
                                for arg in args {
                                    if !live_values.contains(arg) {
                                        live_values.insert(*arg);
                                        changed = true;
                                    }
                                }
                            }
                            RValue::_VarId(..) => {
                                // unreachable!()
                            }
                            RValue::Param { .. } => {}
                            RValue::Function { .. } => {}
                        }
                    }
                    CfgInstruction::_AssignVar { .. } => {
                        unreachable!()
                    }
                }
            }

            for phi in &bb.phi {
                for source in phi.sources.values() {
                    match source {
                        ValueRefOrConst::Value(v) => {
                            if !live_values.contains(v)
                                // avoid %y = phi(%x@B1, %y@B2) for variables in loops, if %y is not live
                                && phi.dest != *v
                            {
                                live_values.insert(*v);
                                changed = true;
                            }
                        }
                        ValueRefOrConst::Const(_) => {}
                    }
                }
            }
        }

        if !changed {
            break;
        }
    }

    live_values
}

fn dead_value_elimination(builder: &mut CfgBuilder) {
    let live_values = live_values_analysis(builder);

    for bb in &mut builder.blocks {
        bb.phi.retain(|phi| live_values.contains(&phi.dest));
        bb.instructions.retain(|instr| {
            if let CfgInstruction::Assign { dest, val: _ } = instr
                && !live_values.contains(dest)
            {
                false
            } else {
                true
            }
        });
    }
}

fn constant_propagation(builder: &mut CfgBuilder) {
    let mut constants = BTreeMap::<ValueRef, i64>::new();

    loop {
        let mut changed = false;

        for bb in &builder.blocks {
            for phi in &bb.phi {
                let first_source = phi.sources.values().next().unwrap();
                match first_source {
                    ValueRefOrConst::Value(v) => {
                        if let Some(c) = constants.get(v) {
                            if phi.sources.values().all(|v| match v {
                                ValueRefOrConst::Value(v2) => {
                                    if let Some(c2) = constants.get(v2) {
                                        c == c2
                                    } else {
                                        false
                                    }
                                }
                                ValueRefOrConst::Const(c2) => c == c2,
                            }) {
                                if !constants.contains_key(&phi.dest) || constants[&phi.dest] != *c
                                {
                                    constants.insert(phi.dest, *c);
                                    changed = true;
                                }
                            }
                        }
                    }
                    ValueRefOrConst::Const(c) => {
                        if phi.sources.values().all(|v| match v {
                            ValueRefOrConst::Const(c2) => c == c2,
                            ValueRefOrConst::Value(v2) => match constants.get(v2) {
                                Some(c2) => c == c2,
                                None => false,
                            },
                        }) {
                            if !constants.contains_key(&phi.dest) || constants[&phi.dest] != *c {
                                constants.insert(phi.dest, *c);
                                changed = true;
                            }
                        }
                    }
                }
            }

            for instr in &bb.instructions {
                if let CfgInstruction::Assign { dest, val } = instr {
                    match val {
                        RValue::Const(c) => {
                            if !constants.contains_key(dest) || constants[dest] != *c {
                                constants.insert(*dest, *c);
                                changed = true;
                            }
                        }
                        RValue::Add { left, right } => {
                            if let (Some(lc), Some(rc)) =
                                (constants.get(left), constants.get(right))
                            {
                                let result = lc + rc;
                                if !constants.contains_key(dest) || constants[dest] != result {
                                    constants.insert(*dest, result);
                                    changed = true;
                                }
                            }
                        }
                        RValue::Sub { left, right } => {
                            if let (Some(lc), Some(rc)) =
                                (constants.get(left), constants.get(right))
                            {
                                let result = lc - rc;
                                if !constants.contains_key(dest) || constants[dest] != result {
                                    constants.insert(*dest, result);
                                    changed = true;
                                }
                            }
                        }
                        RValue::Mul { left, right } => {
                            if let (Some(lc), Some(rc)) =
                                (constants.get(left), constants.get(right))
                            {
                                let result = lc * rc;
                                if !constants.contains_key(dest) || constants[dest] != result {
                                    constants.insert(*dest, result);
                                    changed = true;
                                }
                            }
                        }
                        RValue::Div { left, right } => {
                            if let (Some(lc), Some(rc)) =
                                (constants.get(left), constants.get(right))
                            {
                                if *rc != 0 {
                                    let result = lc / rc;
                                    if !constants.contains_key(dest) || constants[dest] != result {
                                        constants.insert(*dest, result);
                                        changed = true;
                                    }
                                }
                            }
                        }
                        RValue::Modulus { left, right } => {
                            if let (Some(lc), Some(rc)) =
                                (constants.get(left), constants.get(right))
                            {
                                if *rc != 0 {
                                    let result = lc % rc;
                                    if !constants.contains_key(dest) || constants[dest] != result {
                                        constants.insert(*dest, result);
                                        changed = true;
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        if !changed {
            break;
        }
    }

    for bb in &mut builder.blocks {
        for phi in &mut bb.phi {
            for s in phi.sources.values_mut() {
                if let ValueRefOrConst::Value(v) = s {
                    if let Some(c) = constants.get(v) {
                        *s = ValueRefOrConst::Const(*c);
                    }
                }
            }
        }
        for instr in &mut bb.instructions {
            if let CfgInstruction::Assign { dest, val } = instr {
                if let Some(c) = constants.get(dest) {
                    *val = RValue::Const(*c);
                }
            }
        }
    }
}

pub(super) fn cleanup_passes(builder: &mut CfgBuilder) {
    loop {
        let mut changed = false;
        changed |= phi_simplification(builder);
        changed |= val_inliner(builder);
        if !changed {
            break;
        }
    }

    constant_propagation(builder);
    dead_value_elimination(builder);
}
