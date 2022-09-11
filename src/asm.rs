#[derive(Debug, Clone)]
pub enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RSP,
    RBP,
    R(u8),
    Stack(i64),
    Data(String),
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
            RAX => "rax".to_string(),
            RBX => "rbx".to_string(),
            RCX => "rcx".to_string(),
            RDX => "rdx".to_string(),
            RSI => "rsi".to_string(),
            RDI => "rdi".to_string(),
            RSP => "rsp".to_string(),
            RBP => "rbp".to_string(),
            R(n) => format!("r{n}"),
            Stack(i) => {
                if i >= &0 {
                    format!("[rbp+{}]", i)
                } else {
                    format!("[rbp{}]", i)
                }
            }
            Data(name) => name.to_owned(),
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
