use std::collections::BTreeMap;
use std::path::Path;

use crate::{
    backend::llvm_api::{
        BRef, Builder, Ctxt, IntCmp, Module, StandardTypes, TargetMachine, Ty, VRef,
    },
    cfg::{
        def::{
            BBId, CfgInstruction, ControlFlowGraph, PhiType, RValue, TailCfgInstruction, ValueRef,
            ValueRefOrConst,
        },
        sema::{SemaResults, VarType},
    },
    parser::ast::{self, Program, TopLevel, Type, VarId},
};

mod llvm_api;

pub struct Backend {
    standard_ty: StandardTypes,

    functions: BTreeMap<VarId, (VRef, Ty)>,
    sema: SemaResults,

    // keep them ordered like this for drop order
    builder: Builder,
    module: Module,
    context: Ctxt,
}

impl Backend {
    pub fn new(sema: SemaResults) -> Self {
        let context = Ctxt::new();
        let module = context.create_module_with_name("module");
        let builder = context.builder();
        let standard_ty = StandardTypes::get_from_context(&context);

        Self {
            context,
            module,
            builder,

            functions: BTreeMap::new(),
            sema,

            standard_ty,
        }
    }

    pub fn init_functions(&mut self, program: &Program) {
        for top_level in &program.items {
            if let TopLevel::Function(func, var_id) = &top_level.node {
                let mut param_tys: Vec<Ty> = func
                    .params
                    .iter()
                    .map(|(ty, _, _)| self.standard_ty.ty_from_ast_type(&ty.node))
                    .collect();
                let ret_ty = self.standard_ty.ty_from_ast_type(&func.return_type.node);
                let func_ty = Ty::function(&mut param_tys, ret_ty, false);
                let func_val = self.module.add_function(&func.name.node, func_ty.clone());
                self.functions.insert(*var_id, (func_val, func_ty));
            }
        }
    }

    fn const_int(&self, value: i64) -> VRef {
        self.standard_ty
            .i64_ty()
            .const_int(u64::from_be_bytes(value.to_be_bytes()), false)
    }

    fn const_bool(&self, value: bool) -> VRef {
        self.standard_ty
            .bool_ty()
            .const_int(if value { 1 } else { 0 }, false)
    }

    pub fn lower_function(&mut self, var_id: VarId, func: &ControlFlowGraph) {
        let mut bb_map = BTreeMap::<BBId, BRef>::new();
        let mut vmap = BTreeMap::<ValueRef, VRef>::new();
        let mut func_tys = BTreeMap::<ValueRef, Ty>::new();
        let func_val = self.functions[&var_id].0;

        for bb in &func.basic_blocks {
            let block =
                self.builder
                    .create_block(&self.context, func_val, &format!("bb{}", bb.id.0));
            bb_map.insert(bb.id, block);
        }

        let mut remaining_phis = Vec::new();

        for bb in &func.basic_blocks {
            let builder = self.builder.build_block(bb_map[&bb.id]);
            for instr in &bb.phi {
                let dest_ty = match instr.phi_type {
                    PhiType::Bool => self.standard_ty.bool_ty(),
                    PhiType::Int => self.standard_ty.i64_ty(),
                    PhiType::Infer => instr
                        .dest
                        .2
                        .map(|var_id| &self.sema.var_types[&var_id])
                        .map_or(self.standard_ty.i64_ty(), |ty| match ty {
                            VarType::Int => self.standard_ty.i64_ty(),
                            VarType::Bool => self.standard_ty.bool_ty(),
                            _ => unimplemented!("Unsupported phi type inference"),
                        }),
                };
                let dest_vref = builder.phi(dest_ty, |phi| {
                    for (pred_bb_id, source_valref) in &instr.sources {
                        let source_vref = match source_valref {
                            ValueRefOrConst::Value(vr) => vmap.get(vr).cloned(),
                            ValueRefOrConst::Const(c) => Some(self.const_int(*c)),
                            ValueRefOrConst::ConstBool(b) => Some(self.const_bool(*b)),
                        };
                        if let Some(source_vref) = source_vref {
                            phi.branch(bb_map[pred_bb_id], source_vref);
                        } else {
                            // source value not yet available, defer
                            // this happens in loops where the phi source comes from a later block
                            // we defer adding the incoming values until after all blocks are built
                            remaining_phis.push((
                                phi.target_vref(),
                                *pred_bb_id,
                                match source_valref {
                                    ValueRefOrConst::Value(vr) => *vr,
                                    ValueRefOrConst::Const(_) | ValueRefOrConst::ConstBool(_) => {
                                        // should not happen
                                        unreachable!("Const in deferred phi")
                                    }
                                },
                            ));
                        }
                    }
                });
                vmap.insert(instr.dest, dest_vref);
            }

            for instr in &bb.instructions {
                match instr {
                    CfgInstruction::Assign { dest, val } => {
                        let rvalue = match val {
                            RValue::Const(c) => self.const_int(*c),
                            RValue::ConstBool(b) => self.const_bool(*b),
                            RValue::Param { param_index } => {
                                func_val.get_param(*param_index as u32)
                            }
                            RValue::Value(v) => vmap[v],
                            RValue::Add { left, right } => builder.add(vmap[left], vmap[right]),
                            RValue::Sub { left, right } => builder.sub(vmap[left], vmap[right]),
                            RValue::Mul { left, right } => builder.mul(vmap[left], vmap[right]),
                            RValue::Div { left, right } => builder.idiv(vmap[left], vmap[right]),
                            RValue::Modulus { left, right } => {
                                builder.imod(vmap[left], vmap[right])
                            }
                            RValue::Function { var_id } => {
                                let (func_vref, func_ty) = self.functions[var_id];
                                func_tys.insert(*dest, func_ty.clone());
                                func_vref
                            }
                            RValue::_VarId(var_id) => unreachable!(),
                            RValue::EqCheck { left, right } => {
                                builder.int_cmp(IntCmp::EQ, vmap[left], vmap[right])
                            }
                            RValue::NEqCheck { left, right } => {
                                builder.int_cmp(IntCmp::NE, vmap[left], vmap[right])
                            }
                            RValue::LtCheck { left, right } => {
                                builder.int_cmp(IntCmp::SLT, vmap[left], vmap[right])
                            }
                            RValue::GtCheck { left, right } => {
                                builder.int_cmp(IntCmp::SGT, vmap[left], vmap[right])
                            }
                            RValue::LEqCheck { left, right } => {
                                builder.int_cmp(IntCmp::SLE, vmap[left], vmap[right])
                            }
                            RValue::GEqCheck { left, right } => {
                                builder.int_cmp(IntCmp::SGE, vmap[left], vmap[right])
                            }
                            RValue::BitwiseAnd { left, right } => {
                                builder.bitwise_and(vmap[left], vmap[right])
                            }
                            RValue::BitwiseOr { left, right } => {
                                builder.bitwise_or(vmap[left], vmap[right])
                            }
                            RValue::BitwiseXor { left, right } => {
                                builder.bitwise_xor(vmap[left], vmap[right])
                            }
                            RValue::BitShiftLeft { left, right } => {
                                builder.shl(vmap[left], vmap[right])
                            }
                            RValue::BitShiftRight { left, right } => {
                                builder.shr(vmap[left], vmap[right])
                            }
                            RValue::BitwiseNot { expr } => builder.bitwise_not(vmap[expr]),
                            RValue::Call { func, args } => {
                                let mut arg_vrefs: Vec<VRef> =
                                    args.iter().map(|arg| vmap[arg]).collect();
                                let func_vref = vmap[func];
                                let func_ty = func_tys[func];
                                builder.call(func_vref, func_ty, &mut arg_vrefs)
                            }
                            RValue::Select {
                                cond,
                                then_val,
                                else_val,
                            } => {
                                let c = vmap[cond];
                                if c.ty_is_bool() {
                                    builder.select(vmap[cond], vmap[then_val], vmap[else_val])
                                } else {
                                    // non-bool condition, compare against zero
                                    let zero = self.standard_ty.i64_ty().const_int(0, false);
                                    let cond_bool = builder.int_cmp(IntCmp::NE, c, zero);
                                    builder.select(cond_bool, vmap[then_val], vmap[else_val])
                                }
                            }
                        };
                        vmap.insert(*dest, rvalue);
                    }
                    CfgInstruction::_AssignVar { var_id: _, val: _ } => {
                        // should not appear here after SSA conversion
                        unreachable!()
                    }
                }
            }

            match &bb.tail {
                TailCfgInstruction::Return { value } => {
                    if let Some(ret_val) = value {
                        builder.ret(vmap[ret_val]);
                    } else {
                        builder.ret_void();
                    }
                }
                TailCfgInstruction::UncondBranch { target } => {
                    builder.br(bb_map[target]);
                }
                TailCfgInstruction::CondBranch {
                    cond,
                    then_bb,
                    else_bb,
                } => {
                    let v = vmap[cond];
                    if v.ty_is_bool() {
                        builder.cond_br(v, bb_map[then_bb], bb_map[else_bb]);
                    } else {
                        // non-bool condition, compare against zero
                        let zero = self.standard_ty.i64_ty().const_int(0, false);
                        let cond_bool = builder.int_cmp(IntCmp::NE, v, zero);
                        builder.cond_br(cond_bool, bb_map[then_bb], bb_map[else_bb]);
                    }
                }
                TailCfgInstruction::Undefined => {
                    unreachable!()
                }
            }
        }

        for (phi_vref, pred_bb_id, source_valref) in remaining_phis {
            let source_vref = vmap[&source_valref];
            self.builder
                .add_phi_incoming(phi_vref, bb_map[&pred_bb_id], source_vref);
        }
    }

    pub fn debug(&self) {
        let s = self.module.print_to_string();
        println!("{:?}", s);
    }

    pub fn optimize(&mut self) {
        llvm_api::LLVMTarget::init();
        let tm = TargetMachine::new_default();

        self.module.optimize(llvm_api::OptLevel::O3, &tm);
    }

    pub fn write_bitcode_to_file(&self, path: impl AsRef<Path>) -> Result<(), anyhow::Error> {
        let path = path.as_ref();
        self.module
            .write_bitcode_file(path)
            .map_err(|e| anyhow::anyhow!("Failed to write LLVM bitcode to file {}", path.display()))
    }
}
