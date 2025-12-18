use std::collections::{BTreeMap, BTreeSet};

use crate::cfg::{
    builder::CfgBuilder,
    def::{
        BBId, CfgInstruction, PhiCfgInstruction, RValue, TailCfgInstruction, ValueRef,
        ValueRefOrConst,
    },
};

fn phi_simplification(builder: &mut CfgBuilder) -> bool {
    // Phi simplification pass
    // ==========================
    //
    // Remove any phi instructions that have all sources the same
    // and replace them with a simple assignment instruction.
    // For example, if we have:
    //   %x = phi(%a@B1, %a@B2, %a@B3)
    // We can replace it with:
    //   %x = %a
    // This helps reduce unnecessary phi instructions in the CFG.
    let mut to_add = Vec::new();

    fn compute_first_source(phi: &PhiCfgInstruction) -> Option<&ValueRefOrConst> {
        phi.sources
            .values()
            .filter(|it| match it {
                ValueRefOrConst::Const(_) | ValueRefOrConst::ConstBool(_) => true,
                ValueRefOrConst::Value(v) => v != &phi.dest,
            })
            .next()
    }

    for bb in &mut builder.blocks {
        let simplified_phis = bb
            .phi
            .extract_if(0..bb.phi.len(), |phi| {
                let first_source = compute_first_source(phi).unwrap();
                phi.sources.values().all(|v| v == first_source)
                    || phi.sources.iter().all(|it| match it.1 {
                        ValueRefOrConst::Value(v) => {
                            v == &phi.dest || ValueRefOrConst::Value(*v) == *first_source
                        }
                        _ => false,
                    })
            })
            .collect::<Vec<_>>();
        for phi in simplified_phis {
            let first_source = compute_first_source(&phi).unwrap();
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
    // Value inlining pass
    // ==========================
    //
    // If we have %a = %b, then we can replace all uses of %a with %b
    //
    // If we have %b = %c and %a = %b, we need to first build the equality map, then
    // replace all uses of %a with %c. This happens by walking the equality map until we reach
    // a value that is not mapped to another value.
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

    fn process(v: &mut ValueRef, equalities: &BTreeMap<ValueRef, ValueRef>, changed: &mut bool) {
        let mut current = *v;
        while let Some(next) = equalities.get(&current) {
            current = *next;
        }
        if v != &current {
            *v = current;
            *changed = true;
        }
    }

    for bb in &mut builder.blocks {
        for phi in &mut bb.phi {
            for source in phi.sources.values_mut() {
                if let ValueRefOrConst::Value(v) = source {
                    process(v, &equalities, &mut changed);
                }
            }
        }
        for instr in &mut bb.instructions {
            if let CfgInstruction::Assign { dest: _, val } = instr {
                match val {
                    RValue::Value(v) => process(v, &equalities, &mut changed),
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
                        process(left, &equalities, &mut changed);
                        process(right, &equalities, &mut changed);
                    }
                    RValue::Call { func, args } => {
                        process(func, &equalities, &mut changed);
                        for arg in args {
                            process(arg, &equalities, &mut changed);
                        }
                    }
                    RValue::Const(_) | RValue::ConstBool(_) => {}
                    RValue::Param { .. } => {}
                    RValue::Function { .. } => {}
                    RValue::_VarId(_) => {}
                    RValue::BitwiseNot { expr } => {
                        process(expr, &equalities, &mut changed);
                    }
                    RValue::Select {
                        cond,
                        then_val,
                        else_val,
                    } => {
                        process(cond, &equalities, &mut changed);
                        process(then_val, &equalities, &mut changed);
                        process(else_val, &equalities, &mut changed);
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
                process(cond, &equalities, &mut changed);
            }
            TailCfgInstruction::UncondBranch { target: _ } => {}
            TailCfgInstruction::Return { value } => {
                if let Some(v) = value {
                    process(v, &equalities, &mut changed);
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
    // Live values analysis pass
    // ==========================
    //
    // Compute live values using a fixed-point algorithm
    //
    // We start from the return values and branch conditions, marking them as live.
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
                            RValue::Select {
                                cond,
                                then_val,
                                else_val,
                            } => {
                                if !live_values.contains(cond) {
                                    live_values.insert(*cond);
                                    changed = true;
                                }
                                if !live_values.contains(then_val) {
                                    live_values.insert(*then_val);
                                    changed = true;
                                }
                                if !live_values.contains(else_val) {
                                    live_values.insert(*else_val);
                                    changed = true;
                                }
                            }
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
    // Dead value elimination pass
    // ============================
    //
    // Remove any instructions that assign to values that are not live.
    // We use the live values analysis to determine which values are live.
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
    // Constant propagation pass
    // ==========================
    //
    // Propagate constant values through the CFG.
    //
    // For example, if we have %a = 5, %b = 3, and %c = %a + %b we can replace %c with 8.
    // This enables further optimizations like dead value elimination.
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
    // repeatedly inline blocks that lead directly to blocks with only one predecessor
    //
    // For example, if we have:
    //
    //            A
    //            |
    //            B
    //            |
    //            C
    //
    // We can inline B into A, resulting in:
    //
    //            A
    //            |
    //            C
    //
    // Then we can inline C into A as well.
    //
    //            A
    //
    // This helps reduce unnecessary jumps and simplifies the CFG.
    loop {
        let mut to_inline = None;
        for bb in &builder.blocks {
            match &bb.tail {
                TailCfgInstruction::UncondBranch { target } => {
                    let target_bb = &builder.blocks[target.0];
                    if target_bb.predecessors.len() == 1 && target_bb.predecessors[0] == bb.id {
                        to_inline = Some((bb.id, *target));
                        break;
                    }
                }
                TailCfgInstruction::CondBranch {
                    cond,
                    then_bb,
                    else_bb,
                } => {
                    // Inline in the following case:
                    //
                    //            A
                    //           / \
                    //          B   |
                    //           \ /
                    //            C
                    let then_bb_ref = &builder.blocks[then_bb.0];
                    let else_bb_ref = &builder.blocks[else_bb.0];
                    if then_bb_ref.predecessors == [bb.id]
                        && else_bb_ref.predecessors.len() == 2
                        && else_bb_ref.predecessors.eq_unordered(&[bb.id, *then_bb])
                        && then_bb_ref.instructions.is_empty()
                        && (then_bb_ref.tail
                            == TailCfgInstruction::UncondBranch { target: *else_bb })
                        && else_bb_ref.phi.is_empty()
                    {
                        to_inline = Some((bb.id, *then_bb));
                        break;
                    }
                }
                _ => (),
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
    // Compute dominated blocks using a fixed-point algorithm
    //
    // First find all blocks reachable from start_bb, then iteratively remove blocks that have predecessors
    // not in the dominated set.
    fn _compute_dominated_internal(
        builder: &CfgBuilder,
        start_bb: BBId,
        dominated: &mut BTreeSet<BBId>,
    ) {
        dominated.insert(start_bb);
        for succ in &builder.blocks[start_bb.0].successors {
            if !dominated.contains(&succ) {
                _compute_dominated_internal(builder, *succ, dominated);
            }
        }
    }

    _compute_dominated_internal(builder, start_bb, dominated);

    loop {
        let old_dominated = dominated.clone();
        dominated.retain(|bb| {
            bb == &start_bb
                || builder.blocks[bb.0]
                    .predecessors
                    .iter()
                    .all(|pred| old_dominated.contains(pred))
        });
        if dominated.len() == old_dominated.len() {
            break;
        }
    }
}

fn compute_ascendants(builder: &CfgBuilder, ascendents: &mut BTreeSet<BBId>) {
    loop {
        let mut to_insert = None;
        for bb in &*ascendents {
            for pred in &builder.blocks[bb.0].predecessors {
                if !ascendents.contains(pred)
                    && builder.blocks[pred.0]
                        .successors
                        .iter()
                        .all(|s| ascendents.contains(s))
                {
                    to_insert = Some(*pred);
                    break;
                }
            }
        }
        if let Some(bb_id) = to_insert {
            ascendents.insert(bb_id);
        } else {
            break;
        }
    }
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

fn compute_top_level(
    builder: &CfgBuilder,
    ascendents: &BTreeSet<BBId>,
    init_set: &BTreeSet<BBId>,
    top_level: &mut BTreeMap<BBId, BBId>,
    expr_dominated: &BTreeSet<BBId>,
) {
    for bb in init_set {
        let mut p = *bb;
        loop {
            if builder.blocks[p.0].predecessors.is_empty() {
                break;
            }

            let parent = builder.blocks[p.0].predecessors[0];

            if builder.blocks[parent.0]
                .successors
                .iter()
                .any(|pr| !ascendents.contains(&pr) && !expr_dominated.contains(&pr))
            {
                break;
            }

            p = parent;
        }

        top_level.entry(*bb).insert_entry(p);
    }
}

fn hoist_pass(builder: &mut CfgBuilder) {
    // Hoist Pass: Expression Hoisting for Available/Very Busy Analysis
    // ================================================================
    // Hoists reused, pure expressions to common dominators using ascendants.
    //
    // Algorithm:
    // 1. Identify multi-use RValues (pure expressions).
    // 2. Compute ascendants (blocks where expr *must* be evaluated).
    // 3. Find "top-level" blocks: dominated by ascendants, no escaping paths.
    // 4. Insert expr at top-level, replace uses with new value.
    //
    // It works similar to LLVM's GVN pass, but only for identical (sub-)expressions
    //
    //
    // ======================
    // === Inner workings ===
    // ======================
    //
    // The way it works is by looking for expressions (RValues) that are used in multiple places,
    // then for each of those places, it attempts to find the "top-level" block where the expression can be computed
    // such that all uses are dominated by that block, and there are no "escaping" paths from that block (e.g. that lead
    // to paths where the expression is not used). The logic for this is implemented in the compute_ascendants and
    // compute_top_level functions above.
    //
    // Once the top-level blocks are found, the expression is inserted in those blocks, and the uses are replaced with
    // the new value we just inserted in the top-level block.
    //
    // For example, consider the following control flow graph:
    //
    //              A
    //             / \
    //            B   C
    //           / \   \
    //          D   E   F
    //          \   /   |
    //            G     H
    //
    //
    // In a graph like this, where we have the same expression used in blocks G and H, the top-level block where
    // we can insert the expression is block A, since all paths from A lead to either G or H.
    //
    // However, if the graph also had a block I that was a successor of F, but it did not use the expression:
    //
    //              A
    //             / \
    //            B   C
    //           / \   \
    //          D   E   F
    //          \   /   | \
    //            G     H  I
    //
    // Then we could not hoist the expression to block A, since there is an escaping path from A to I that does not lead
    // to our expression being evaluated.
    //
    // Due to how we construct the SSA form CFG, we can guarantee that if an expression is used in multiple blocks,
    // then those blocks are always dominated by a common ancestor block, which contains the last operation required
    // to compute the expression. This is because any value that is used in multiple blocks must have been defined in
    // a block that dominates all those blocks.
    //
    // So if the RValues are piece-wise equal, we can hoist them to that common ancestor block, or one of the blocks
    // dominated by it, but which is still a common ancestor to all the blocks where our expression was used.
    //
    // Let's consider another example, where the expression is used in blocks G and H:
    //
    //                A
    //              /   \
    //             B      C
    //           /   \      \
    //          D     E       F
    //           \   / \        \
    //             G    H        I
    //
    // In this case, the top-level block where we can hoist the expression is block B, since all paths from B lead to either G or H.
    //
    // We cannot hoist it to block A, since there is a path from A to I that does not lead to our expression being evaluated.
    //
    // This pass handles both the "very busy expressions" and the "available expressions" optimizations, since it can reuse
    // expressions that are already computed in the parent blocks. For example, consider the same graph as above:
    //
    //                A
    //              /   \
    //             B      C
    //           /   \      \
    //          D     E       F
    //           \   / \        \
    //             G    H        I
    //
    // If blocks B and G both compute the same expression, then this pass will flag, via the dominance of B over G,
    // that the expression can be reused in G, and will replace the computation in G with a use of the value computed in B.
    //
    // This way, we avoid redundant computations, and improve the efficiency of the generated code, which is precisely
    // the goal of the "available expressions" optimization.
    //
    // This is a fixpoint algorithm, although in practice it should converge very quickly, since we are hoisiting expressions
    // directly to where they should be computed in one iteration.
    //
    // An important limitation of the steps outlined above is that we only consider pure RValues, that is, expressions
    // that do not have side effects. This means that we do not attempt to hoist function calls, since they may have side effects,
    // and hoisting them could change the semantics of the program, if we end up calling them in a different order.
    //
    // As such, the current implementation handles the following Call expressions, in a CFG like this:
    //
    //              A
    //             / \
    //            B   C
    //
    // Where both B and C call the same function (say foo) with the same arguments. The expression can be hoisted to A,
    // if there are no other function calls in B or C before the call to foo, which we are trying to hoist.
    // Only then can we guarantee that hoisting the call to foo to A does not change the semantics of the program.
    //
    // This part is handled separately from the "top-level" hoisting algorithm outlined above, since it only works on one
    // level of conditional branches at a time.

    let mut doms = dominated_sets(builder);

    loop {
        let mut changed = false;
        let mut exprs = BTreeMap::<RValue, Vec<(BBId, ValueRef)>>::new();

        for bb in &builder.blocks {
            for instr in &bb.instructions {
                if let CfgInstruction::Assign { dest, val } = instr
                    // only hoist pure RValues, calls may have side effects, and hoisitng function references is not useful
                    && !matches!(val, RValue::Call { .. } | RValue::Function { .. })
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
            let mut top_level = BTreeMap::new();
            let mut expr_dominated = BTreeSet::<BBId>::new();
            for bb_id in &asc_map {
                let dominated = &doms[bb_id];
                expr_dominated.extend(dominated);
            }
            compute_top_level(
                builder,
                &asc_map,
                &init_set,
                &mut top_level,
                &expr_dominated,
            );

            // println!(
            //     "Hoisting expression {:?} from {:?} to {:?}",
            //     rvalue,
            //     uses.iter().map(|u| u.0).collect::<Vec<BBId>>(),
            //     top_level.values().copied().collect::<BTreeSet<BBId>>()
            // );

            let mut tl_values = BTreeMap::<BBId, ValueRef>::new();

            for (bb_id, use_dest) in uses {
                if tl_values.contains_key(&top_level[bb_id]) || top_level[bb_id] != *bb_id {
                    continue;
                }
                tl_values.insert(top_level[bb_id], use_dest.clone());
            }

            for (bb_id, use_dest) in uses {
                if let Some(tl_bb) = top_level.get(bb_id) {
                    if bb_id == tl_bb {
                        // already computed in this block
                        if let Some(tl_value) = tl_values.get(tl_bb)
                            && tl_value != use_dest
                        {
                            // but used another value, replace
                            builder.replace_value(*use_dest, *tl_value);
                            changed = true;
                            removed_vals.push(*use_dest);
                        }
                        continue;
                    }
                    if let Some(tl_value) = tl_values.get(tl_bb)
                        && tl_value != use_dest
                    {
                        // already computed in the top-level block, reuse
                        builder.replace_value(*use_dest, *tl_value);
                        changed = true;
                        removed_vals.push(*use_dest);
                        continue;
                    }
                    let new_dest = builder.allocate_value(use_dest.1, use_dest.2);
                    // replace use in bb_id with value from tl_bb
                    builder.add_instruction(
                        *tl_bb,
                        CfgInstruction::Assign {
                            dest: new_dest,
                            val: rvalue.clone(),
                        },
                    );
                    builder.replace_value(*use_dest, new_dest);
                    changed = true;
                    removed_vals.push(*use_dest);
                    tl_values.insert(*tl_bb, new_dest);
                }
            }
            //     let mut candidates = asc_map
            //         .iter()
            //         .filter(|bb_id| {
            //             let dominated = &doms[bb_id];
            //             let mut count = 0;
            //             for (use_bb_id, _) in uses {
            //                 if dominated.contains(use_bb_id) {
            //                     count += 1;
            //                 }
            //             }
            //             count == uses.len()
            //         })
            //         .copied()
            //         .collect::<Vec<BBId>>();
            //     if candidates.is_empty() {
            //         continue;
            //     }
            //     // pick the most downstream candidate
            //     let mut best_candidate = *candidates.first().unwrap();
            //     for candidate in &candidates[1..] {
            //         if doms[&best_candidate].contains(candidate) {
            //             best_candidate = *candidate;
            //         }
            //     }

            //     if let Some(v) = uses.iter().find(|it| it.0 == best_candidate) {
            //         // already computed in the best candidate block, reuse in all other blocks
            //         for (_, use_dest) in uses {
            //             if use_dest != &v.1 {
            //                 builder.replace_value(*use_dest, v.1);
            //                 changed = true;
            //                 removed_vals.push(*use_dest);
            //             }
            //         }
            //         continue;
            //     }

            //     // println!(
            //     //     "Hoisting expression {:?} from {:?} to {:?}",
            //     //     rvalue, uses, best_candidate
            //     // );

            //     // create a new value in the best_candidate block
            //     let first_use = &uses[0];
            //     let new_dest = builder.allocate_value(first_use.1.1, first_use.1.2);
            //     builder.add_instruction(
            //         best_candidate,
            //         CfgInstruction::Assign {
            //             dest: new_dest,
            //             val: rvalue.clone(),
            //         },
            //     );

            //     // replace uses in all use blocks
            //     for (_, use_dest) in uses {
            //         builder.replace_value(*use_dest, new_dest);
            //         removed_vals.push(*use_dest);
            //     }
            //     changed = true;
        }

        // attempt to hoist out of branches (function calls)

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
    // Block deduplication pass
    // ==========================
    //
    // This pass identifies basic blocks that have no instructions of their own, and identical tails,
    // and merges them to reduce redundancy in the control flow graph (CFG).
    //
    // For example, consider the following CFG:
    //
    //            A
    //           / \
    //          B   C
    //           \ /
    //            D
    //
    // If blocks B and C have no instructions and both lead to block D, they can be merged into a single block,
    // reducing the number of blocks in the CFG.
    //
    // In the case outlined above, this pass is only valid if D has no phi nodes.
    let mut block_map = BTreeMap::<BBId, Vec<BBId>>::new();

    // never reuse entry block
    for i in 1..builder.blocks.len() {
        if block_map.contains_key(&builder.blocks[i].id) {
            continue;
        }
        let bb_i = &builder.blocks[i];
        if !bb_i.phi.is_empty() || !bb_i.instructions.is_empty() || bb_i.predecessors.len() > 1 {
            continue;
        }
        for j in (i + 1)..builder.blocks.len() {
            let bb_j = &builder.blocks[j];
            if bb_j.phi.is_empty() && bb_j.instructions.is_empty() && bb_i.tail == bb_j.tail {
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
    // Tail unification pass
    // ==========================
    //
    // This pass identifies conditional branches that lead to the same target block
    // and simplifies them into unconditional branches.
    //
    // For example, in a CFG like this:
    //
    //              ---
    //               A
    //               |
    //    br_cond %cond ? B : B
    //              / \
    //             |   |
    //              \ /
    //              ---
    //               B
    //
    // Where block A ends with a conditional branch that leads to block B regardless of the condition,
    // we can simplify the branch to an unconditional branch to block B:
    //
    //            ---
    //             A
    //             |
    //            br B
    //             |
    //            ---
    //             B
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

trait EqUnordered<T> {
    fn eq_unordered(&self, other: &[T]) -> bool
    where
        T: Eq + Ord;
}

impl<T> EqUnordered<T> for Vec<T> {
    fn eq_unordered(&self, other: &[T]) -> bool
    where
        T: Eq + Ord,
    {
        if self.len() != other.len() {
            return false;
        }
        let mut self_counts = BTreeMap::<&T, usize>::new();
        for item in self {
            *self_counts.entry(item).or_default() += 1;
        }
        let mut other_counts = BTreeMap::<&T, usize>::new();
        for item in other {
            *other_counts.entry(item).or_default() += 1;
        }
        self_counts == other_counts
    }
}

fn phi_to_sel(builder: &mut CfgBuilder) {
    // The Phi to Select pass replaces phi nodes that select between two values based on a condition
    // with a select instruction.
    //
    // For example, in a CFG like this:
    //
    //            A
    //           / \
    //          B   C
    //           \ /
    //            D
    //
    // If block D has a phi node like:
    //
    //   %x = phi(%a@B, %b@C)
    //
    // And block A ends with a conditional branch based on condition %cond:
    //
    //   br_cond %cond ? B : C
    //
    // With B and C having no other predecessors than A, and no instructions, we can simplify the phi node in D to a select instruction in A:
    //
    //   %x = select %cond, %a, %b
    //
    // This optimization reduces the number of phi nodes and can enable further optimizations (block_dedup, tail_unification, and block_inliner).
    //
    // This should also handle the following CFG:
    //
    //            A
    //          /   \
    //         B     |
    //          \   /
    //            C

    loop {
        let mut changed = false;
        let mut to_replace = Vec::new();

        for bb in &builder.blocks {
            if let TailCfgInstruction::CondBranch {
                cond,
                then_bb,
                else_bb,
            } = &bb.tail
                && then_bb != else_bb
                && builder[*then_bb].predecessors == [bb.id]
                && builder[*else_bb].predecessors == [bb.id]
                && builder[*then_bb].instructions.is_empty()
                && builder[*else_bb].instructions.is_empty()
                && builder[*then_bb].successors.len() == 1
                && builder[*else_bb].successors.len() == 1
                && let after = builder[*then_bb].successors[0]
                && after == builder[*else_bb].successors[0]
                && builder[*then_bb].successors[0] != bb.id
                && builder[after]
                    .predecessors
                    .eq_unordered(&[*then_bb, *else_bb])
                && builder[after].phi.len() > 0
            {
                for phi in &builder[after].phi {
                    let then_value = phi.sources.get(then_bb).unwrap();
                    let else_value = phi.sources.get(else_bb).unwrap();
                    to_replace.push((
                        bb.id,
                        after,
                        phi.clone(),
                        *cond,
                        then_value.clone(),
                        else_value.clone(),
                    ));
                }
            } else if let TailCfgInstruction::CondBranch {
                cond,
                then_bb,
                else_bb,
            } = &bb.tail
                && then_bb != else_bb
                && builder[*then_bb].predecessors == [bb.id]
                && builder[*else_bb]
                    .predecessors
                    .eq_unordered(&[bb.id, *then_bb])
                && builder[*then_bb].instructions.is_empty()
                && builder[*then_bb].successors.len() == 1
                && let after = builder[*then_bb].successors[0]
                && after == builder[*else_bb].id
                && builder[*then_bb].successors[0] != bb.id
                && builder[after].predecessors.eq_unordered(&[*then_bb, bb.id])
                && builder[after].phi.len() > 0
            {
                for phi in &builder[after].phi {
                    let then_value = phi.sources.get(then_bb).unwrap();
                    let else_value = phi.sources.get(&bb.id).unwrap();
                    to_replace.push((
                        bb.id,
                        after,
                        phi.clone(),
                        *cond,
                        then_value.clone(),
                        else_value.clone(),
                    ));
                }
            }
        }

        let mut replaced = BTreeMap::<ValueRef, ValueRef>::new();

        for (bb_id, after_bb, phi, cond, then_value, else_value) in to_replace {
            let new_dest = builder.allocate_value(phi.dest.1, phi.dest.2);
            let then_val = match then_value {
                ValueRefOrConst::Value(value_ref) => {
                    let mut target = value_ref;
                    while let Some(repl) = replaced.get(&target) {
                        target = *repl;
                    }
                    target
                }
                ValueRefOrConst::Const(c) => {
                    let const_value = builder.allocate_value(phi.dest.1, phi.dest.2);
                    builder.add_instruction(
                        bb_id,
                        CfgInstruction::Assign {
                            dest: const_value,
                            val: RValue::Const(c),
                        },
                    );
                    const_value
                }
                ValueRefOrConst::ConstBool(c) => {
                    let const_value = builder.allocate_value(phi.dest.1, phi.dest.2);
                    builder.add_instruction(
                        bb_id,
                        CfgInstruction::Assign {
                            dest: const_value,
                            val: RValue::ConstBool(c),
                        },
                    );
                    const_value
                }
            };
            let else_val = match else_value {
                ValueRefOrConst::Value(value_ref) => {
                    let mut target = value_ref;
                    while let Some(repl) = replaced.get(&target) {
                        target = *repl;
                    }
                    target
                }
                ValueRefOrConst::Const(c) => {
                    let const_value = builder.allocate_value(phi.dest.1, phi.dest.2);
                    builder.add_instruction(
                        bb_id,
                        CfgInstruction::Assign {
                            dest: const_value,
                            val: RValue::Const(c),
                        },
                    );
                    const_value
                }
                ValueRefOrConst::ConstBool(c) => {
                    let const_value = builder.allocate_value(phi.dest.1, phi.dest.2);
                    builder.add_instruction(
                        bb_id,
                        CfgInstruction::Assign {
                            dest: const_value,
                            val: RValue::ConstBool(c),
                        },
                    );
                    const_value
                }
            };
            builder.add_instruction(
                bb_id,
                CfgInstruction::Assign {
                    dest: new_dest,
                    val: RValue::Select {
                        cond,
                        then_val,
                        else_val,
                    },
                },
            );
            builder.replace_value(phi.dest, new_dest);

            builder.blocks[after_bb.0]
                .phi
                .retain(|p| p.dest != phi.dest);

            replaced.insert(phi.dest, new_dest);
            changed = true;
        }

        if !changed {
            break;
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

    phi_to_sel(builder);
    block_dedup(builder);
    tail_unification(builder);
    dead_value_elimination(builder, true);
    builder.trim_dead_blocks();
    block_inliner(builder);
}
