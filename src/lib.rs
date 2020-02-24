use std::io::prelude::*;
use std::io::stdout;
use std::ops::{Index,IndexMut};
use std::fmt;

const MEM_MAX: usize = 4096;
const STACK_MIN: usize = 16;

fn putc(c: u8) {
    stdout().write(&[c as u8]).expect("error writing to stdout");
    stdout().flush().expect("error flushing stdout");
}

fn small_to_chars(small: u16) -> [Option<u8>; 2] {
    fn convert(small: u8) -> Option<u8> {
        match small {
            0..=25  => Some(b'A' + small),
            26..=51 => Some(b'a' + (small - 26)),
            52..=61 => Some(b'0' + (small - 52)),
            62      => Some(b' '),
            _       => None,
        }
    }

    let latter = (small >> 6) & 0x3F;
    let former = small & 0x3F;
    [convert(former as u8), convert(latter as u8)]
}

enum Instruction {
    Halt,
    Jump,
    JumpZ,
    Load,
    Store,
    LShft,
    RShft,
    Xor,
    And,
    SFull,
    Add,
    Pop,
    Push,
    NoOp,
    Unknown(u16),
}

impl From<u16> for Instruction {
    fn from(n: u16) -> Instruction {
        use Instruction::*;
        match n {
            0x0 => Halt,
            0x2 => Jump,
            0x3 => JumpZ,
            0x4 => Load,
            0x5 => Store,
            0x6 => LShft,
            0x7 => RShft,
            0x8 => Xor,
            0x9 => And,
            0xA => SFull,
            0xB => Add,
            0xD => Pop,
            0xE => Push,
            0xF => NoOp,
            n   => Unknown(n),
        }
    }
}

trait Word {
    fn inst(&self) -> Self;
    fn data(&self) -> Self;
    fn set_data(&mut self, val: Self);
}

impl Word for u16 {
    #[inline(always)]
    fn inst(&self) -> u16 {
        (self >> 12) & 0x000Fu16
    }

    #[inline(always)]
    fn data(&self) -> u16 {
        self & 0x0FFF
    }

    #[inline(always)]
    fn set_data(&mut self, val: Self) {
        *self = val;
    }
}

#[derive(Clone, Copy)]
pub struct Memory([u16; MEM_MAX]);

impl Index<u16> for Memory {
    type Output = u16;

    fn index(&self, address: u16) -> &u16 {
        &self.0[(address & 0xFFF) as usize]
    }
}

impl IndexMut<u16> for Memory {
    fn index_mut(&mut self, address: u16) -> &mut u16 {
        &mut self.0[(address & 0xFFF) as usize]
    }
}

impl fmt::Debug for Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!();
    }
}

#[derive(Debug)]
pub struct Sma16 {
    memory: Memory,
    stack: Vec<u16>,
    program_counter: u16,
    instruction_register: u16,
    accumulator: u16,
    flag_halt: bool,
    flag_zero: bool,
}

impl Sma16 {
    const INTERRUPT_REASON_REGISTER: u16 = 0x008;
    const INTERRUPT_RETURN_REGISTER: u16 = 0x009;
    const STACK_SIZE_REGISTER: u16       = 0x00D;

    const RESET_VECTOR: u16    = 0x000;
    const FAULT_VECTOR: u16    = 0x001;
    const ASCII_OUT: u16       = 0x00A;
    const SMALL_OUT: u16       = 0x00B;

    pub fn with_blank_memory() -> Sma16 {
        Sma16::with_memory(Memory([0; MEM_MAX]))
    }

    pub fn with_memory(mem: Memory) -> Sma16 {
        let mut sma16 = Sma16 {
            memory: mem,
            stack: Vec::with_capacity(STACK_MIN),
            program_counter: Sma16::RESET_VECTOR,
            instruction_register: 0,
            accumulator: 0,
            flag_halt: false,
            flag_zero: false,
        };
        sma16.memory[Sma16::STACK_SIZE_REGISTER] = 0xFFFF;
        sma16
    }

    pub fn load_memory(&mut self, start: u16, mem: &[u16]) {
        for (i, word) in mem.iter().enumerate() {
            self.memory[start + (i as u16)] = *word;
        }
    }

    pub fn reinitialize(&mut self) {
        *self = Sma16::with_memory(self.memory);
    }

    pub fn fault(&mut self, reason: u16) {
        self.memory[Sma16::INTERRUPT_RETURN_REGISTER] = self.program_counter + 1;
        self.memory[Sma16::INTERRUPT_REASON_REGISTER] = reason;
        self.program_counter = Sma16::FAULT_VECTOR;
    }

    pub fn read_memory(&self, address: u16) -> u16 {
        self.memory[address.data()]
    }

    pub fn write_memory(&mut self, address: u16, value: u16) {
        self.memory[address.data()] = value;
        match address {
            Sma16::ASCII_OUT => putc((value & 0x00FF) as u8),
            Sma16::SMALL_OUT => {
                for c in &small_to_chars(value.data()) {
                    c.map(putc);
                }
            }
            _ => {},
        }
    }

    pub fn write_memory_data(&mut self, address: u16, value: u16) {
        self.write_memory(address, self.read_memory(address) & 0xF000 | value.data())
    }

    pub fn step(&mut self) {
        use Instruction::*;
        self.instruction_register = self.read_memory(self.program_counter);
        let inc_pc_normally = match Instruction::from(self.instruction_register.inst()) {
            Halt => {
                self.flag_halt = true;
                true
            },
            Jump => {
                self.program_counter = self.instruction_register.data();
                false
            },
            JumpZ => {
                if self.flag_zero {
                    self.program_counter = self.instruction_register.data();
                }
                !self.flag_zero
            },
            Load => {
                self.accumulator = self.read_memory(self.instruction_register.data());
                true
            },
            Store => {
                self.write_memory_data(self.instruction_register.data(), self.accumulator.data());
                true
            },
            LShft => {
                let amount = self.instruction_register.data() >> 1;
                if self.instruction_register.data() & 1 == 1 {
                    self.accumulator.set_data(self.accumulator.data() << amount);
                } else {
                    self.accumulator <<= amount;
                }
                true
            },
            RShft => {
                let amount = self.instruction_register.data() >> 1;
                if self.instruction_register.data() & 1 == 1 {
                    self.accumulator.set_data(self.accumulator.data() >> amount);
                } else {
                    self.accumulator >>= amount;
                }
                true
            },
            Xor => {
                self.accumulator.set_data(self.accumulator.data() ^ self.instruction_register.data());
                true
            },
            And => {
                self.accumulator.set_data(self.accumulator.data() & self.instruction_register.data());
                true
            },
            SFull => {
                self.write_memory(self.instruction_register.data(), self.accumulator);
                true
            },
            Add => {
                self.accumulator.set_data(self.accumulator.data() + self.instruction_register.data());
                self.flag_zero = self.accumulator.data() == 0;
                true
            },
            Pop => {
                self.accumulator = self.stack.pop().unwrap_or(0);
                true
            },
            Push => {
                self.stack.push(self.accumulator);
                true
            },
            NoOp => true,
            Unknown(n) => {
                self.fault(0x0ff0 | n.inst());
                false
            },
        };

        if inc_pc_normally {
            self.program_counter += 1;
        }
    }

    pub fn run(&mut self) {
        self.flag_halt = false;
        while !self.flag_halt {
            self.step();
        }
    }
}
