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
    Data(String),
}

impl Register {
    pub fn byte_size(&self) -> u64 {
        use Register::*;

        match self.clone() {
            RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP | R(_) | Data(_) => 8,
            EAX | EBX | ECX | EDX | ESI | EDI | ESP | EBP | RD(_) => 4,
            AX | AH | BX | BH | CX | CH | DX | DH | SI | DI | SP | BP | RW(_) => 2,
            Stack(_, s) => s.clone(),
            _ => 1,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Reg(Register),
    Value(i64),
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
    Return,
    Syscall,
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
                if i >= &0 {
                    format!("{} [rbp+{}]", name_size(size), i)
                } else {
                    format!("{} [rbp{}]", name_size(size), i)
                }
            }
            Data(name) => name.to_owned(),
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
            Value(val) => val.to_string(),
        }
    }
}

impl ToString for Instr {
    fn to_string(&self) -> String {
        use Instr::*;

        match self {
            Movzx(reg1, reg2) => format!("movzx {}, {}", reg1.to_string(), reg2.to_string()),
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
        }
    }
}
