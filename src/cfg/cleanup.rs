use std::collections::{BTreeMap, BTreeSet};

use crate::cfg::{
    builder::CfgBuilder,
    def::{BBId, CfgInstruction, RValue, TailCfgInstruction, ValueRef, ValueRefOrConst},
};

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
                val: RValue::from_value_ref_or_const(first_source),
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
                    RValue::Const(_) | RValue::ConstBool(_) => {}
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

fn live_values_analysis(builder: &mut CfgBuilder, mark_calls: bool) -> BTreeSet<ValueRef> {
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
                        if matches!(val, RValue::Call { .. }) && mark_calls {
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
                            RValue::Const(_) | RValue::ConstBool(_) => {}
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
                        ValueRefOrConst::ConstBool(_) => {}
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

fn dead_value_elimination(builder: &mut CfgBuilder, mark_calls: bool) {
    let live_values = live_values_analysis(builder, mark_calls);

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
                                ValueRefOrConst::ConstBool(_) => false,
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
                            ValueRefOrConst::ConstBool(_) => false,
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
                    ValueRefOrConst::ConstBool(c) => {
                        // skip boolean constants for now
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
                        RValue::ConstBool(b) => {
                            // skip boolean constants for now
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

fn block_inliner(builder: &mut CfgBuilder) {
    loop {
        // inline blocks that lead directly to a block that has only one predecessor
        let mut to_inline = None;
        for bb in &builder.blocks {
            if let TailCfgInstruction::UncondBranch { target } = &bb.tail {
                let target_bb = &builder.blocks[target.0];
                if target_bb.predecessors.len() == 1 && target_bb.predecessors[0] == bb.id {
                    to_inline = Some((bb.id, *target));
                    break;
                }
            }
        }

        if let Some((from_bb_id, to_bb_id)) = to_inline {
            builder.inline_block_edge(from_bb_id, to_bb_id);
        } else {
            break;
        }
    }
}

fn compute_dominated(builder: &CfgBuilder, start_bb: BBId, dominated: &mut BTreeSet<BBId>) {
    dominated.insert(start_bb);

    for succ in &builder.blocks[start_bb.0].successors {
        if !dominated.contains(&succ) {
            compute_dominated(builder, *succ, dominated);
        }
    }
}

fn compute_ascendants(builder: &CfgBuilder, ascendents: &mut BTreeSet<BBId>) -> Option<BBId> {
    // Last inserted = most upstream block
    let mut last_inserted = None;

    loop {
        let mut to_insert = None;
        for bb in &*ascendents {
            for pred in &builder.blocks[bb.0].predecessors {
                if !ascendents.contains(pred) {
                    to_insert = Some(*pred);
                    break;
                }
            }
        }
        if let Some(bb_id) = to_insert {
            ascendents.insert(bb_id);
            last_inserted = Some(bb_id);
        } else {
            break;
        }
    }

    last_inserted
}

fn dominated_sets(builder: &CfgBuilder) -> BTreeMap<BBId, BTreeSet<BBId>> {
    let mut doms_map = BTreeMap::<BBId, BTreeSet<BBId>>::new();

    for bb in &builder.blocks {
        let mut dominated = BTreeSet::<BBId>::new();
        compute_dominated(builder, bb.id, &mut dominated);
        doms_map.insert(bb.id, dominated);
    }

    doms_map
}

// fn dominators(builder: &CfgBuilder) -> BTreeMap<BBId, BTreeSet<BBId>> {
//     let doms_map = dominated_sets(builder);
//     let mut dominators_map = BTreeMap::<BBId, BTreeSet<BBId>>::new();

//     for bb in &builder.blocks {
//         let mut dominators = BTreeSet::<BBId>::new();
//         for (other_bb_id, dominated_set) in &doms_map {
//             if dominated_set.contains(&bb.id) && other_bb_id != &bb.id {
//                 dominators.insert(*other_bb_id);
//             }
//         }
//         dominators_map.insert(bb.id, dominators);
//     }

//     dominators_map
// }

fn hoist_pass(builder: &mut CfgBuilder) {
    // attempt to hoist reused (sub-)expressions out of ifs
    // It works similar to LLVM's GVN pass, but only for identical expressions
    // and only hoisting out of branches

    let mut doms = dominated_sets(builder);

    loop {
        let mut changed = false;
        let mut exprs = BTreeMap::<RValue, Vec<(BBId, ValueRef)>>::new();

        for bb in &builder.blocks {
            for instr in &bb.instructions {
                if let CfgInstruction::Assign { dest, val } = instr
                    // only hoist pure RValues, calls may have side effects
                    && !matches!(val, RValue::Call { .. })
                {
                    exprs.entry(val.clone()).or_default().push((bb.id, *dest));
                }
            }
        }

        let mut removed_vals = Vec::new();

        for (rvalue, uses) in &exprs {
            if uses.len() < 2 {
                continue;
            }

            let mut init_set = BTreeSet::<BBId>::new();
            let mut asc_map = BTreeSet::<BBId>::new();
            for (bb_id, _) in uses {
                init_set.insert(*bb_id);
                asc_map.insert(*bb_id);
            }
            compute_ascendants(builder, &mut asc_map);
            let mut candidates = asc_map
                .iter()
                .filter(|bb_id| {
                    let dominated = &doms[bb_id];
                    let mut count = 0;
                    for (use_bb_id, _) in uses {
                        if dominated.contains(use_bb_id) {
                            count += 1;
                        }
                    }
                    count == uses.len()
                })
                .copied()
                .collect::<Vec<BBId>>();
            if candidates.is_empty() {
                continue;
            }
            // pick the most downstream candidate
            let mut best_candidate = *candidates.first().unwrap();
            for candidate in &candidates[1..] {
                if doms[&best_candidate].contains(candidate) {
                    best_candidate = *candidate;
                }
            }

            if let Some(v) = uses.iter().find(|it| it.0 == best_candidate) {
                // already computed in the best candidate block, reuse in all other blocks
                for (_, use_dest) in uses {
                    if use_dest != &v.1 {
                        builder.replace_value(*use_dest, v.1);
                        changed = true;
                        removed_vals.push(*use_dest);
                    }
                }
                continue;
            }

            // println!(
            //     "Hoisting expression {:?} from {:?} to {:?}",
            //     rvalue, uses, best_candidate
            // );

            // create a new value in the best_candidate block
            let first_use = &uses[0];
            let new_dest = builder.allocate_value(first_use.1.1, first_use.1.2);
            builder.add_instruction(
                best_candidate,
                CfgInstruction::Assign {
                    dest: new_dest,
                    val: rvalue.clone(),
                },
            );

            // replace uses in all use blocks
            for (_, use_dest) in uses {
                builder.replace_value(*use_dest, new_dest);
                removed_vals.push(*use_dest);
            }
            changed = true;
        }

        let mut to_hoist = Vec::new();
        for bb in &builder.blocks {
            if let TailCfgInstruction::CondBranch {
                cond: _,
                then_bb,
                else_bb,
            } = &bb.tail
            {
                let then_block = &builder.blocks[then_bb.0];
                let else_block = &builder.blocks[else_bb.0];

                let mut then_exprs = BTreeMap::<RValue, ValueRef>::new();
                let mut first_call = true;
                for instr in &then_block.instructions {
                    if let CfgInstruction::Assign { dest, val } = instr {
                        if matches!(val, RValue::Call { .. }) {
                            // we can only hoist one function call at a time to avoid function call ordering issues
                            // e.g., if both branches call the same function *first*, we can hoist it
                            if first_call {
                                first_call = false;
                            } else {
                                continue;
                            }
                        }
                        then_exprs.insert(val.clone(), *dest);
                    }
                }

                let mut first_call = true;
                for instr in &else_block.instructions {
                    if let CfgInstruction::Assign { dest, val } = instr {
                        if matches!(val, RValue::Call { .. }) {
                            // we can only hoist one function call at a time to avoid function call ordering issues
                            // e.g., if both branches call the same function *first*, we can hoist it
                            if first_call {
                                first_call = false;
                            } else {
                                continue;
                            }
                        }
                        if let Some(then_dest) = then_exprs.get(val) {
                            to_hoist.push((
                                val.clone(),
                                *then_dest,
                                *dest,
                                bb.id,
                                *then_bb,
                                *else_bb,
                            ));
                        }
                    }
                }
            }
        }

        for (expr, then_dest, else_dest, bb, then_bb, else_bb) in to_hoist {
            // println!(
            //     "Hoisting expression {:?} from BBId({}) and BBId({}) to BBId({})",
            //     expr, then_bb.0, else_bb.0, bb.0
            // );
            // create a new value in the parent block
            let new_dest = builder.allocate_value(then_dest.1, then_dest.2);
            builder.add_instruction(
                bb,
                CfgInstruction::Assign {
                    dest: new_dest,
                    val: expr,
                },
            );

            // replace uses in then and else blocks
            builder.replace_value(then_dest, new_dest);
            builder.replace_value(else_dest, new_dest);

            removed_vals.push(then_dest);
            removed_vals.push(else_dest);

            changed = true;
        }

        if changed {
            builder.remove_dead_values(&removed_vals);
        } else {
            break;
        }
    }
}

fn block_dedup(builder: &mut CfgBuilder) {
    let mut block_map = BTreeMap::<BBId, Vec<BBId>>::new();

    // never reuse entry block
    for i in 1..builder.blocks.len() {
        if block_map.contains_key(&builder.blocks[i].id) {
            continue;
        }
        let bb_i = &builder.blocks[i];
        if !bb_i.instructions.is_empty() || bb_i.predecessors.len() > 1 {
            continue;
        }
        for j in (i + 1)..builder.blocks.len() {
            let bb_j = &builder.blocks[j];
            if bb_i.tail == bb_j.tail {
                block_map.entry(bb_i.id).or_default().push(bb_j.id);
            }
        }
    }

    for (from, to) in block_map.clone() {
        for to in to {
            for bb in &builder.blocks {
                if bb.predecessors.contains(&from)
                    && bb.predecessors.contains(&to)
                    && bb.phi.len() > 0
                {
                    block_map.get_mut(&from).unwrap().retain(|&x| x != to);
                }
            }
        }
    }

    builder.dedup_blocks(&block_map);
}

fn tail_unification(builder: &mut CfgBuilder) {
    for bb in &mut builder.blocks {
        if let TailCfgInstruction::CondBranch {
            cond,
            then_bb,
            else_bb,
        } = &bb.tail
            && then_bb == else_bb
        {
            bb.tail = TailCfgInstruction::UncondBranch { target: *then_bb };
        }
    }

    builder.link_successors_and_predecessors(&mut vec![]);
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
    dead_value_elimination(builder, true);
    block_inliner(builder);

    hoist_pass(builder);
    loop {
        let mut changed = false;
        changed |= phi_simplification(builder);
        changed |= val_inliner(builder);
        if !changed {
            break;
        }
    }

    block_dedup(builder);
    tail_unification(builder);
    dead_value_elimination(builder, true);
    builder.trim_dead_blocks();
    block_inliner(builder);
}
