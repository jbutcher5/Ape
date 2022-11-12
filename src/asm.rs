use std::cmp::Ordering;

#[inline]
const fn name_size(size: &u64) -> &'static str {
    match size {
        1 => "BYTE",
        2 => "WORD",
        4 => "DWORD",
        8 => "QWORD",
        _ => panic!("Invalid byte size given"),
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Register {
    RAX,
    EAX,
    AX,
    AH,
    AL,
    RBX,
    EBX,
    BX,
    BH,
    BL,
    RCX,
    ECX,
    CX,
    CH,
    CL,
    RDX,
    EDX,
    DX,
    DH,
    DL,
    RSI,
    ESI,
    SI,
    SIL,
    RDI,
    EDI,
    DI,
    DIL,
    RSP,
    ESP,
    SP,
    SPL,
    RBP,
    EBP,
    BP,
    BPL,
    R(u8),
    RD(u8),
    RW(u8),
    RB(u8),
    Stack(i64, u64),
    Data(usize),
}

impl Register {
    pub fn byte_size(&self) -> u64 {
        use Register::*;

        match self.clone() {
            RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP | R(_) | Data(_) => 8,
            EAX | EBX | ECX | EDX | ESI | EDI | ESP | EBP | RD(_) => 4,
            AX | AH | BX | BH | CX | CH | DX | DH | SI | DI | SP | BP | RW(_) => 2,
            Stack(_, s) => s,
            _ => 1,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Reg(Register),
    Value(String),
}

#[derive(Debug, Clone)]
pub enum Instr {
    Raw(String),
    Push(Operand),
    Pop(Register),
    Mov(Register, Operand),
    Movzx(Register, Register),
    Call(String),
    NullReg(Register),
    Add(Register, Operand),
    Sub(Register, Operand),
    Lea(Register, Register),
    Return,
    Syscall,
    Cmp(Operand, Operand),
    DefineLabel(Label),
    Jmp(Label),
    Je(Label),
}

#[derive(Debug, Clone)]
pub enum Label {
    Named(String),
    Numbered(u64),
}

impl ToString for Label {
    fn to_string(&self) -> String {
        match self {
            Self::Named(string) => string.to_string(),
            Self::Numbered(n) => format!(".L{n}"),
        }
    }
}

impl ToString for Register {
    fn to_string(&self) -> String {
        use Register::*;

        match self {
            R(n) => format!("r{n}"),
            RD(n) => format!("r{n}d"),
            RW(n) => format!("r{n}w"),
            RB(n) => format!("r{n}b"),
            Stack(i, size) => {
                let position: i64 = *i - *size as i64;

                match position.cmp(&0) {
                    Ordering::Equal => format!("{} [rbp]", name_size(size)),
                    Ordering::Greater => format!("{} [rbp+{position}]", name_size(size)),
                    Ordering::Less => format!("{} [rbp{position}]", name_size(size)),
                }
            }
            Data(index) => format!("s{index}"),
            _ => match self {
                RAX => "rax",
                EAX => "eax",
                AX => "ax",
                AH => "ah",
                AL => "al",
                RBX => "rbx",
                EBX => "ebx",
                BX => "bx",
                BH => "bh",
                BL => "bl",
                RCX => "rcx",
                ECX => "ecx",
                CX => "cx",
                CH => "ch",
                CL => "cl",
                RDX => "rdx",
                EDX => "edx",
                DX => "dx",
                DH => "dh",
                DL => "dl",
                RSI => "rsi",
                ESI => "esi",
                SI => "si",
                SIL => "sil",
                RDI => "rdi",
                EDI => "edi",
                DI => "di",
                DIL => "dil",
                RSP => "rsp",
                ESP => "esp",
                SP => "sp",
                SPL => "spl",
                RBP => "rbp",
                EBP => "ebp",
                BP => "bp",
                BPL => "bpl",
                _ => panic!(),
            }
            .to_string(),
        }
    }
}

impl ToString for Operand {
    fn to_string(&self) -> String {
        use Operand::*;

        match self {
            Reg(reg) => reg.to_string(),
            Value(val) => val.clone(),
        }
    }
}

impl ToString for Instr {
    fn to_string(&self) -> String {
        use Instr::*;

        match self {
            Movzx(reg1, reg2) => format!("movzx {}, {}", reg1.to_string(), reg2.to_string()),
            Lea(reg1, reg2) => format!("lea {}, {}", reg1.to_string(), reg2.to_string()),
            Push(reg) => format!("push {}", reg.to_string()),
            Pop(reg) => format!("pop {}", reg.to_string()),
            Mov(reg, op) => format!("mov {}, {}", reg.to_string(), op.to_string()),
            Call(name) => format!("call {name}"),
            NullReg(reg) => format!("xor {}, {}", reg.to_string(), reg.to_string()),
            Add(reg1, op) => format!("add {}, {}", reg1.to_string(), op.to_string()),
            Sub(reg1, reg2) => format!("sub {}, {}", reg1.to_string(), reg2.to_string()),
            Return => "ret".to_string(),
            Syscall => "syscall".to_string(),
            Raw(string) => string.to_string(),
            DefineLabel(label) => format!("{}:", label.to_string()),
            Jmp(label) => format!("jmp {}", label.to_string()),
            Je(label) => format!("je {}", label.to_string()),
            Cmp(op1, op2) => format!("cmp {}, {}", op1.to_string(), op2.to_string()),
        }
    }
}

use {Instr::*, Operand::*, Register::*};

#[inline]
pub fn next_aligned_stack(bytes: u64) -> u64 {
    match bytes {
        0 => 16,
        start => (start as f64 / 16.0).ceil() as u64 * 16 - bytes,
    }
}

#[inline]
pub fn push_reg(r: Register, stack_size: u64) -> Vec<Instr> {
    let temp_reg = match r.byte_size() {
        1 => AL,
        2 => AX,
        4 => EAX,
        8 => RAX,
        _ => panic!("Unknown register for byte size {}", r.byte_size()),
    };

    vec![
        Sub(RSP, Value(r.byte_size().to_string())),
        Mov(temp_reg.clone(), Reg(r.clone())),
        Mov(Stack(-(stack_size as i64), r.byte_size()), Reg(temp_reg)),
    ]
}

#[inline]
pub fn mov_reg(to: Register, from: Register) -> Instr {
    if to.byte_size() > from.byte_size() {
        Movzx(to, from)
    } else {
        Mov(to, Reg(from))
    }
}

#[inline]
pub fn reference_reg(to: Register, from: Register) -> Vec<Instr> {
    vec![Lea(RAX, from), mov_reg(to, RAX)]
}
