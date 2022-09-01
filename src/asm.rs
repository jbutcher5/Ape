#[inline]
fn next_multiple(start: u64, n: u64) -> u64 {
    (start as f64 / n as f64).ceil() as u64 * n
}

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
    Stack(u64),
    Data(String),
}

#[derive(Debug, Clone)]
pub enum Instr {
    Pushl(i64),
    Push(Register),
    Pop(Register),
    Movl(Register, i64),
    Mov(Register, Register),
    Call(String),
    NullReg(Register),
    Add(Register, Register),
    Addl(i64, i64),
    Sub(Register, Register),
    Subl(i64, i64),
    Return,
    Syscall,
}

#[derive(Debug, Clone)]
pub struct DefineBytes {
    pub name: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub name: String,
    pub used_bytes: u64,
    pub instructions: Vec<Instr>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub text: Vec<Scope>,
    pub data: Vec<DefineBytes>,
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
            Stack(i) => format!("[rbp-{i}]"),
            Data(name) => name.to_owned(),
        }
    }
}

impl ToString for Instr {
    fn to_string(&self) -> String {
        use Instr::*;
        use Register::*;

        match self {
            Pushl(n) => [Movl(RAX, *n), Push(RAX)]
                .iter()
                .map(Instr::to_string)
                .intersperse("\n    ".to_string())
                .collect::<String>(),
            Push(reg) => format!("push {}", reg.to_string()),
            Pop(reg) => format!("pop {}", reg.to_string()),
            Movl(reg, n) => format!("mov {}, {n}", reg.to_string()),
            Mov(reg1, reg2) => format!("mov {}, {}", reg1.to_string(), reg2.to_string()),
            Call(name) => format!("call {name}"),
            NullReg(reg) => Movl(reg.to_owned(), 0).to_string(),
            Add(reg1, reg2) => format!("add {}, {}", reg1.to_string(), reg2.to_string()),
            Addl(l1, l2) => [Movl(RAX, *l1), Movl(RBX, *l2), Add(RAX, RBX)]
                .iter()
                .map(Instr::to_string)
                .intersperse("\n    ".to_string())
                .collect::<String>(),
            Sub(reg1, reg2) => format!("sub {}, {}", reg1.to_string(), reg2.to_string()),
            Subl(l1, l2) => [Movl(RAX, *l1), Movl(RBX, *l2), Sub(RAX, RBX)]
                .iter()
                .map(Instr::to_string)
                .intersperse("\n    ".to_string())
                .collect::<String>(),
            Return => "ret".to_string(),
            Syscall => "syscall".to_string(),
        }
    }
}

impl ToString for DefineBytes {
    fn to_string(&self) -> String {
        format!("{}:  db `{}`", self.name, self.value)
    }
}

impl ToString for Scope {
    fn to_string(&self) -> String {
        use Instr::*;
        use Register::*;

        let stack_alignment = match self.used_bytes {
            0 => 16,
            start => next_multiple(start, 16),
        };

        let header = vec![
            Push(RBP),
            Mov(RBP, RSP),
            Movl(RAX, stack_alignment as i64),
            Sub(RSP, RAX),
        ];

        let footer = if self.name == "main".to_string() {
            vec![Pop(RBP), Movl(RAX, 60), NullReg(RDI), Syscall, Return]
        } else {
            vec![Pop(RBP), Return]
        };

        let body: Vec<Instr> = vec![header, (*self.instructions).to_vec(), footer].concat();

        [
            format!("{}:\n", self.name),
            body.iter()
                .map(|instr| format!("    {}\n", instr.to_string()))
                .collect::<String>(),
        ]
        .concat()
    }
}

impl Scope {
    pub fn append(&mut self, instructions: Vec<Instr>) {
        self.instructions.extend(instructions);
    }

    pub fn res_bytes(&mut self, bytes: u64) {
        self.used_bytes += bytes;
    }

    pub fn res_bytes_loc(&mut self, bytes: u64) -> u64 {
        self.res_bytes(bytes);
        self.used_bytes
    }
}

impl Program {
    pub fn encode(&self) -> Vec<u8> {
        [
            include_bytes!("header.asm"),
            self.text
                .iter()
                .map(Scope::to_string)
                .intersperse("\n".to_string())
                .collect::<String>()
                .as_bytes(),
            format!(
                "section .data\n{}",
                self.data
                    .iter()
                    .map(|db| format!("    {}\n", db.to_string()))
                    .collect::<String>()
            )
            .as_bytes(),
        ]
        .concat()
    }
}
