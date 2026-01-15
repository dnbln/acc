use std::collections::HashMap;

#[cfg(feature = "wasm")]
use serde::Serialize;
#[derive(Clone)]
pub enum Assoc {
    Left,
    Right,
}

#[derive(Clone)]
pub enum OpType {
    Prefix { bp: u8 },
    Infix { bp: u8, assoc: Assoc },
    Postfix { bp: u8 },
}

#[derive(Clone)]
pub struct OpInfo {
    pub op_type: OpType,
}

pub struct OpConfig {
    pub operators: HashMap<String, Vec<OpInfo>>,
}

impl OpConfig {
    pub fn new() -> Self {
        OpConfig {
            operators: HashMap::new(),
        }
    }

    pub fn prefix(mut self, op: &str, bp: u8) -> Self {
        self.add_op(
            op,
            OpInfo {
                op_type: OpType::Prefix { bp },
            },
        );
        self
    }

    pub fn postfix(mut self, op: &str, bp: u8) -> Self {
        self.add_op(
            op,
            OpInfo {
                op_type: OpType::Postfix { bp },
            },
        );
        self
    }

    pub fn infix(mut self, op: &str, bp: u8, assoc: Assoc) -> Self {
        self.add_op(
            op,
            OpInfo {
                op_type: OpType::Infix { bp, assoc },
            },
        );
        self
    }

    pub fn infix_left(self, op: &str, bp: u8) -> Self {
        self.infix(op, bp, Assoc::Left)
    }

    pub fn infix_right(self, op: &str, bp: u8) -> Self {
        self.infix(op, bp, Assoc::Right)
    }

    fn add_op(&mut self, op: &str, info: OpInfo) {
        self.operators.entry(op.to_string()).or_default().push(info);
    }

    pub fn get_prefix(&self, op: &str) -> Option<u8> {
        self.operators
            .get(op)?
            .iter()
            .find_map(|info| match &info.op_type {
                OpType::Prefix { bp } => Some(*bp),
                _ => None,
            })
    }

    pub fn get_infix(&self, op: &str) -> Option<(u8, u8)> {
        self.operators
            .get(op)?
            .iter()
            .find_map(|info| match &info.op_type {
                OpType::Infix { bp, assoc } => {
                    let (l, r) = match assoc {
                        Assoc::Left => (*bp, *bp + 1),
                        Assoc::Right => (*bp + 1, *bp),
                    };
                    Some((l, r))
                }
                _ => None,
            })
    }

    pub fn get_postfix(&self, op: &str) -> Option<u8> {
        self.operators
            .get(op)?
            .iter()
            .find_map(|info| match &info.op_type {
                OpType::Postfix { bp } => Some(*bp),
                _ => None,
            })
    }
}

pub fn c_operators() -> OpConfig {
    OpConfig::new()
        .infix_right("=", 10)
        .infix_right("+=", 10)
        .infix_right("-=", 10)
        .infix_right("*=", 10)
        .infix_right("/=", 10)
        .infix_right("%=", 10)
        .infix_left("||", 20)
        .infix_left("&&", 25)
        .infix_left("|", 30)
        .infix_left("^", 35)
        .infix_left("&", 40)
        .infix_left("==", 45)
        .infix_left("!=", 45)
        .infix_left("<", 50)
        .infix_left(">", 50)
        .infix_left("<=", 50)
        .infix_left(">=", 50)
        .infix_left("<<", 55)
        .infix_left(">>", 55)
        .infix_left("+", 60)
        .infix_left("-", 60)
        .infix_left("*", 70)
        .infix_left("/", 70)
        .infix_left("%", 70)
        .prefix("!", 80)
        .prefix("~", 80)
        .prefix("-", 80)
        .prefix("+", 80)
        .prefix("*", 80)
        .prefix("&", 80)
        .prefix("++", 80)
        .prefix("--", 80)
        .postfix("++", 90)
        .postfix("--", 90)
}

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    BitwiseNot,
    Increment,
    Decrement,
}

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModulusAssign,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}
