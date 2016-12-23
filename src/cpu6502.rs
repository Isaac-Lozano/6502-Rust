//! This module emulates the 6502 CPU.
use memory::{Memory};

use opcode::{FetchType, Operation, Opcode, OP_TABLE};

use std::fmt;
use std::error::Error;

const CPU6502_NMI: u16 = 0xFFFA;
const CPU6502_RESET: u16 = 0xFFFC;
const CPU6502_IRQ: u16 = 0xFFFE;

const CPU6502_STACK: u16 = 0x0100;

/// A bitflags object that represents the processor status.
bitflags!
{
    pub flags PS: u8 {
        /* carry flag */
        const CPU6502_FLAG_C = 0b00000001,
        /* zero flag */
        const CPU6502_FLAG_Z = 0b00000010,
        /* interrupt flag */
        const CPU6502_FLAG_I = 0b00000100,
        /* decimal flag */
        const CPU6502_FLAG_D = 0b00001000,
        /* break flag */
        const CPU6502_FLAG_B = 0b00010000,
        /* unused (reserved) flag */
        const CPU6502_FLAG_U = 0b00100000,
        /* overflow flag */
        const CPU6502_FLAG_V = 0b01000000,
        /* sign flag */
        const CPU6502_FLAG_S = 0b10000000,
    }
}

impl PS
{
    /* TODO: Branchless versions of everything */
    fn set_sign(&mut self, value: u8)
    {
        if value & 0x80 == 0x80
        {
            self.insert(CPU6502_FLAG_S);
        }
        else
        {
            self.remove(CPU6502_FLAG_S);
        }
    }

    fn set_zero(&mut self, value: u8)
    {
        if value == 0
        {
            self.insert(CPU6502_FLAG_Z);
        }
        else
        {
            self.remove(CPU6502_FLAG_Z);
        }
    }

    fn set_carry(&mut self, carry: bool)
    {
        if carry
        {
            self.insert(CPU6502_FLAG_C); 
        }
        else
        {
            self.remove(CPU6502_FLAG_C);
        }
    }

    fn set_overflow(&mut self, overflow: bool)
    {
        if overflow
        {
            self.insert(CPU6502_FLAG_V); 
        }
        else
        {
            self.remove(CPU6502_FLAG_V);
        }
    }
}

impl fmt::Display for PS
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result
    {
        if self.contains(CPU6502_FLAG_S)
        {
            try!(write!(fmt, "S"));
        }
        else
        {
            try!(write!(fmt, "s"));
        }
        if self.contains(CPU6502_FLAG_V)
        {
            try!(write!(fmt, "V"));
        }
        else
        {
            try!(write!(fmt, "v"));
        }
        if self.contains(CPU6502_FLAG_U)
        {
            try!(write!(fmt, "U"));
        }
        else
        {
            try!(write!(fmt, "u"));
        }
        if self.contains(CPU6502_FLAG_B)
        {
            try!(write!(fmt, "B"));
        }
        else
        {
            try!(write!(fmt, "b"));
        }
        if self.contains(CPU6502_FLAG_D)
        {
            try!(write!(fmt, "D"));
        }
        else
        {
            try!(write!(fmt, "d"));
        }
        if self.contains(CPU6502_FLAG_I)
        {
            try!(write!(fmt, "I"));
        }
        else
        {
            try!(write!(fmt, "i"));
        }
        if self.contains(CPU6502_FLAG_Z)
        {
            try!(write!(fmt, "Z"));
        }
        else
        {
            try!(write!(fmt, "z"));
        }
        if self.contains(CPU6502_FLAG_C)
        {
            try!(write!(fmt, "C"));
        }
        else
        {
            try!(write!(fmt, "c"));
        }
        Ok(())
    }
}

/****************
 * Cpu6502Error *
 ****************/
#[derive(Debug)]
pub enum Cpu6502Error
{
    IllegalOpcode(u8),
}

impl fmt::Display for Cpu6502Error
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result
    {
        match *self
        {
            Cpu6502Error::IllegalOpcode(op) =>
                write!(fmt, "IllegalOpcode: 0x{:02X}", op),
        }
    }
}

impl Error for Cpu6502Error
{
    fn description(&self) -> &str
    {
        match *self
        {
            Cpu6502Error::IllegalOpcode(_) =>
                "An llegal opcode was encountered"
        }
    }

    fn cause(&self) -> Option<&Error>
    {
        match *self
        {
            Cpu6502Error::IllegalOpcode(_) =>
                None,
        }
    }
}

pub type Cpu6502Result<T> = Result<T, Cpu6502Error>;

/***********
 * Cpu6502 *
 ***********/
/* Bread and butter
 */
/// 6502 CPU emulator
pub struct Cpu6502<M>
{
    /* Registers */
    pub x: u8,
    pub y: u8,
    pub a: u8,
    pub sp: u8,
    pub pc: u16,
    pub ps: PS,

    /* Memory obj */
    pub memory: M,

    /* Stats data */
    pub cycles: u64,
}

impl<M: Memory<u8> + Memory<u16>> Cpu6502<M>
{
    /// Returns a new Cpu6502<M>.
    ///
    /// # `Input`
    /// Takes in an object that implements the Memory trait. This object acts as
    /// the memory mapper for the cpu.
    pub fn new(mem: M) -> Cpu6502<M>
    {
        let mut cpu = Cpu6502
        {
            x: 0,
            y: 0,
            a: 0,
            sp: 0,
            pc: 0,
            ps: PS::from_bits_truncate(0b00100000),
            memory: mem,
            cycles: 0,
        };
        cpu.reset();
        cpu
    }

    /// Runs the cpu for at least `cycles` cycles.
    ///
    /// Will run instructions until the amount of cycles run passes `cycles`.
    /// Running this with `cycles` = 1 will run a single instruction.
    pub fn run(&mut self, cycles: u32) -> Cpu6502Result<u32>
    {
        let mut passed: u32 = 0;

        /* XXX: Potential bug when cycles ~= MAX_INT */
        while passed < cycles
        {
            passed += try!(self.run_opcode());
        }

        Ok(passed)
    }

    /* Utility functions I guess? */
    /// Triggers a reset in the CPU.
    pub fn reset(&mut self)
    {
        self.pc = self.memory.read(CPU6502_RESET)
    }

    /// Triggers a NMI in the CPU.
    pub fn nmi(&mut self)
    {
        self.pc = self.memory.read(CPU6502_NMI)
    }

    fn push(&mut self, value: u8)
    {
        self.memory.write(CPU6502_STACK | self.sp as u16, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop(&mut self) -> u8
    {
        self.sp = self.sp.wrapping_add(1);
        self.memory.read(CPU6502_STACK | self.sp as u16)
    }

    /* Opcode implementations */
    fn adc(&mut self, src: u8)
    {
        /* TODO: Add binary mode */
        let carry_in;
        if self.ps.contains(CPU6502_FLAG_C)
        {
            carry_in = 1;
        }
        else
        {
            carry_in = 0;
        }
        let (mut tmp, mut carry) = self.a.overflowing_add(src);
        let add_carry_result = tmp.overflowing_add(carry_in);
        tmp = add_carry_result.0;
        carry |= add_carry_result.1;
        self.ps.set_sign(tmp);
        self.ps.set_overflow(((self.a ^ tmp) & (src ^ tmp) & 0x80) == 0x80);
        self.ps.set_zero(tmp);
        self.ps.set_carry(carry);
        self.a = tmp;
    }

    fn and(&mut self, src: u8)
    {
        self.a &= src;
        self.ps.set_sign(self.a);
        self.ps.set_zero(self.a);
    }

    fn asl(&mut self, src: u8, addr: u16, fetch_type: FetchType)
    {
        let carry = (src & 0x80) == 0x80;
        let tmp = src << 1;
        self.ps.set_carry(carry);
        self.ps.set_sign(tmp);
        self.ps.set_zero(tmp);
        match fetch_type
        {
            FetchType::Accumulator =>
            {
                self.a = tmp;
            },
            _ =>
            {
                self.memory.write(addr, tmp);
            },
        }
    }

    fn bcc(&mut self, src: u8, additional_cycles: &mut u32)
    {
        if !self.ps.contains(CPU6502_FLAG_C)
        {
            /* We need pc and src to be the same type to add together.
             * But src needs to be interpreted as a signed value.
             * But we can't go from u8 -> i16 while maintining the
             * proper signedness, so we have to go u8 -> i8 -> i16.
             * pc has to be cast to i16 to add to this value. Then
             * we finally cast as u16 to put back into pc
             */
            let new_pc = (self.pc as i16 + src as i8 as i16) as u16;
            if (new_pc & 0xFF00) != (self.pc & 0xFF00)
            {
                *additional_cycles = 2;
            }
            else
            {
                *additional_cycles = 1;
            }
            self.pc = new_pc;
        }
    }

    fn bcs(&mut self, src: u8, additional_cycles: &mut u32)
    {
        if self.ps.contains(CPU6502_FLAG_C)
        {
            /* We need pc and src to be the same type to add together.
             * But src needs to be interpreted as a signed value.
             * But we can't go from u8 -> i16 while maintining the
             * proper signedness, so we have to go u8 -> i8 -> i16.
             * pc has to be cast to i16 to add to this value. Then
             * we finally cast as u16 to put back into pc
             */
            let new_pc = (self.pc as i16 + src as i8 as i16) as u16;
            if (new_pc & 0xFF00) != (self.pc & 0xFF00)
            {
                *additional_cycles = 2;
            }
            else
            {
                *additional_cycles = 1;
            }
            self.pc = new_pc;
        }
    }

    fn beq(&mut self, src: u8, additional_cycles: &mut u32)
    {
        if self.ps.contains(CPU6502_FLAG_Z)
        {
            /* We need pc and src to be the same type to add together.
             * But src needs to be interpreted as a signed value.
             * But we can't go from u8 -> i16 while maintining the
             * proper signedness, so we have to go u8 -> i8 -> i16.
             * pc has to be cast to i16 to add to this value. Then
             * we finally cast as u16 to put back into pc
             */
            let new_pc = (self.pc as i16 + src as i8 as i16) as u16;
            if (new_pc & 0xFF00) != (self.pc & 0xFF00)
            {
                *additional_cycles = 2;
            }
            else
            {
                *additional_cycles = 1;
            }
            self.pc = new_pc;
        }
    }

    fn bit(&mut self, src: u8)
    {
        self.ps.set_sign(src);
        self.ps.set_overflow((src & 0x40) == 0x40);
        self.ps.set_zero(src & self.a);
    }

    fn bmi(&mut self, src: u8, additional_cycles: &mut u32)
    {
        if self.ps.contains(CPU6502_FLAG_S)
        {
            /* We need pc and src to be the same type to add together.
             * But src needs to be interpreted as a signed value.
             * But we can't go from u8 -> i16 while maintining the
             * proper signedness, so we have to go u8 -> i8 -> i16.
             * pc has to be cast to i16 to add to this value. Then
             * we finally cast as u16 to put back into pc
             */
            let new_pc = (self.pc as i16 + src as i8 as i16) as u16;
            if (new_pc & 0xFF00) != (self.pc & 0xFF00)
            {
                *additional_cycles = 2;
            }
            else
            {
                *additional_cycles = 1;
            }
            self.pc = new_pc;
        }
    }

    fn bne(&mut self, src: u8, additional_cycles: &mut u32)
    {
        if !self.ps.contains(CPU6502_FLAG_Z)
        {
            /* We need pc and src to be the same type to add together.
             * But src needs to be interpreted as a signed value.
             * But we can't go from u8 -> i16 while maintining the
             * proper signedness, so we have to go u8 -> i8 -> i16.
             * pc has to be cast to i16 to add to this value. Then
             * we finally cast as u16 to put back into pc
             */
            let new_pc = (self.pc as i16 + src as i8 as i16) as u16;
            if (new_pc & 0xFF00) != (self.pc & 0xFF00)
            {
                *additional_cycles = 2;
            }
            else
            {
                *additional_cycles = 1;
            }
            self.pc = new_pc;
        }
    }

    fn bpl(&mut self, src: u8, additional_cycles: &mut u32)
    {
        if !self.ps.contains(CPU6502_FLAG_S)
        {
            /* We need pc and src to be the same type to add together.
             * But src needs to be interpreted as a signed value.
             * But we can't go from u8 -> i16 while maintining the
             * proper signedness, so we have to go u8 -> i8 -> i16.
             * pc has to be cast to i16 to add to this value. Then
             * we finally cast as u16 to put back into pc
             */
            let new_pc = (self.pc as i16 + src as i8 as i16) as u16;
            if (new_pc & 0xFF00) != (self.pc & 0xFF00)
            {
                *additional_cycles = 2;
            }
            else
            {
                *additional_cycles = 1;
            }
            self.pc = new_pc;
        }
    }

    fn brk(&mut self)
    {
        self.pc += 1;
        let pc_high = (self.pc >> 8) as u8;
        let pc_low = self.pc as u8;
        self.push(pc_high);
        self.push(pc_low);
        self.ps.insert(CPU6502_FLAG_B);
        let ps_bits = self.ps.bits;
        self.push(ps_bits);
        self.ps.insert(CPU6502_FLAG_I);
        self.pc = self.memory.read(CPU6502_IRQ);
    }

    fn bvc(&mut self, src: u8, additional_cycles: &mut u32)
    {
        if !self.ps.contains(CPU6502_FLAG_V)
        {
            /* We need pc and src to be the same type to add together.
             * But src needs to be interpreted as a signed value.
             * But we can't go from u8 -> i16 while maintining the
             * proper signedness, so we have to go u8 -> i8 -> i16.
             * pc has to be cast to i16 to add to this value. Then
             * we finally cast as u16 to put back into pc
             */
            let new_pc = (self.pc as i16 + src as i8 as i16) as u16;
            if (new_pc & 0xFF00) != (self.pc & 0xFF00)
            {
                *additional_cycles = 2;
            }
            else
            {
                *additional_cycles = 1;
            }
            self.pc = new_pc;
        }
    }

    fn bvs(&mut self, src: u8, additional_cycles: &mut u32)
    {
        if self.ps.contains(CPU6502_FLAG_V)
        {
            /* We need pc and src to be the same type to add together.
             * But src needs to be interpreted as a signed value.
             * But we can't go from u8 -> i16 while maintining the
             * proper signedness, so we have to go u8 -> i8 -> i16.
             * pc has to be cast to i16 to add to this value. Then
             * we finally cast as u16 to put back into pc
             */
            let new_pc = (self.pc as i16 + src as i8 as i16) as u16;
            if (new_pc & 0xFF00) != (self.pc & 0xFF00)
            {
                *additional_cycles = 2;
            }
            else
            {
                *additional_cycles = 1;
            }
            self.pc = new_pc;
        }
    }

    fn clc(&mut self)
    {
        self.ps.remove(CPU6502_FLAG_C);
    }

    fn cld(&mut self)
    {
        self.ps.remove(CPU6502_FLAG_D);
    }

    fn cli(&mut self)
    {
        self.ps.remove(CPU6502_FLAG_I);
    }

    fn clv(&mut self)
    {
        self.ps.remove(CPU6502_FLAG_V);
    }

    fn cmp(&mut self, src: u8)
    {
        let tmp = self.a.wrapping_sub(src);
        self.ps.set_carry(self.a >= src);
        self.ps.set_sign(tmp);
        self.ps.set_zero(tmp);
    }

    fn cpx(&mut self, src: u8)
    {
        let tmp = self.x.wrapping_sub(src);
        self.ps.set_carry(self.x >= src);
        self.ps.set_sign(tmp);
        self.ps.set_zero(tmp);
    }

    fn cpy(&mut self, src: u8)
    {
        let tmp = self.y.wrapping_sub(src);
        self.ps.set_carry(self.y >= src);
        self.ps.set_sign(tmp);
        self.ps.set_zero(tmp);
    }

    fn dec(&mut self, mut src: u8, addr: u16)
    {
        src = src.wrapping_sub(1);
        self.ps.set_sign(src);
        self.ps.set_zero(src);
        self.memory.write(addr, src);
    }

    fn dex(&mut self)
    {
        self.x = self.x.wrapping_sub(1);
        self.ps.set_sign(self.x);
        self.ps.set_zero(self.x);
    }

    fn dey(&mut self)
    {
        self.y = self.y.wrapping_sub(1);
        self.ps.set_sign(self.y);
        self.ps.set_zero(self.y);
    }

    fn eor(&mut self, src: u8)
    {
        self.a ^= src;
        self.ps.set_sign(self.a);
        self.ps.set_zero(self.a);
    }

    fn inc(&mut self, mut src: u8, addr: u16)
    {
        src = src.wrapping_add(1);
        self.ps.set_sign(src);
        self.ps.set_zero(src);
        self.memory.write(addr, src);
    }

    fn inx(&mut self)
    {
        self.x = self.x.wrapping_add(1);
        self.ps.set_sign(self.x);
        self.ps.set_zero(self.x);
    }

    fn iny(&mut self)
    {
        self.y = self.y.wrapping_add(1);
        self.ps.set_sign(self.y);
        self.ps.set_zero(self.y);
    }

    fn jmp(&mut self, addr: u16)
    {
        self.pc = addr;
    }

    fn jsr(&mut self, addr: u16)
    {
        let tmp = self.pc - 1;
        self.push((tmp >> 8) as u8);
        self.push(tmp as u8);
        self.pc = addr;
    }

    fn lda(&mut self, src: u8)
    {
        self.a = src;
        self.ps.set_sign(self.a);
        self.ps.set_zero(self.a);
    }

    fn ldx(&mut self, src: u8)
    {
        self.x = src;
        self.ps.set_sign(self.x);
        self.ps.set_zero(self.x);
    }

    fn ldy(&mut self, src: u8)
    {
        self.y = src;
        self.ps.set_sign(self.y);
        self.ps.set_zero(self.y);
    }

    fn lsr(&mut self, src: u8, addr: u16, fetch_type: FetchType)
    {
        let carry = (src & 0x01) == 0x01;
        let tmp = src >> 1;
        self.ps.set_carry(carry);
        self.ps.set_sign(tmp);
        self.ps.set_zero(tmp);
        match fetch_type
        {
            FetchType::Accumulator =>
            {
                self.a = tmp;
            },
            _ =>
            {
                self.memory.write(addr, tmp);
            },
        }
    }

    /* NOP goes here. */

    fn ora(&mut self, src: u8)
    {
        self.a |= src;
        self.ps.set_sign(self.a);
        self.ps.set_zero(self.a);
    }

    fn pha(&mut self)
    {
        let a = self.a;
        self.push(a);
    }

    fn php(&mut self)
    {
        /* This sets the break flag for some
         * ungodly reason.
         * (It's probably a good reason, but
         * I just don't know it yet.)
         * (I've always hated PHP...)
         */
        self.ps.insert(CPU6502_FLAG_B);
        let ps_bits = self.ps.bits;
        self.push(ps_bits);
    }

    fn pla(&mut self)
    {
        self.a = self.pop();
        self.ps.set_sign(self.a);
        self.ps.set_zero(self.a);
    }

    fn plp(&mut self)
    {
        /* The UNUSED FLAG IS ALWAYS SET.
         * IF YOU TRY TO CIRCUMVENT THIS,
         * WE WILL REPORT YOU TO THE
         * AUTHORITIES.
         *
         * if !self.ps.contains(CPU6502_FLAG_U)
         * {
         *     CONTACT_AUTHORITIES(get_country());
         * }
         */
        self.ps = PS::from_bits_truncate(self.pop());
        self.ps.insert(CPU6502_FLAG_U);
    }

    fn rol(&mut self, src: u8, addr: u16, fetch_type: FetchType)
    {
        let shift_in;
        if self.ps.contains(CPU6502_FLAG_C)
        {
            shift_in = 0x01;
        }
        else
        {
            shift_in = 0x00;
        }
        self.ps.set_carry((src & 0x80) == 0x80);
        let mut tmp = src << 1;
        tmp |= shift_in;
        self.ps.set_sign(tmp);
        self.ps.set_zero(tmp);
        match fetch_type
        {
            FetchType::Accumulator =>
            {
                self.a = tmp;
            },
            _ =>
            {
                self.memory.write(addr, tmp);
            },
        }
    }

    fn ror(&mut self, src: u8, addr: u16, fetch_type: FetchType)
    {
        let shift_in;
        if self.ps.contains(CPU6502_FLAG_C)
        {
            shift_in = 0x80;
        }
        else
        {
            shift_in = 0x00;
        }
        self.ps.set_carry((src & 0x01) == 0x01);
        let mut tmp = src >> 1;
        tmp |= shift_in;
        self.ps.set_sign(tmp);
        self.ps.set_zero(tmp);
        match fetch_type
        {
            FetchType::Accumulator =>
            {
                self.a = tmp;
            },
            _ =>
            {
                self.memory.write(addr, tmp);
            },
        }
    }

    fn rti(&mut self)
    {
        let new_ps = self.pop();
        self.ps = PS::from_bits_truncate(new_ps);
        self.pc = self.pop() as u16;
        self.pc |= (self.pop() as u16) << 8;
    }

    fn rts(&mut self)
    {
        self.pc = self.pop() as u16;
        self.pc |= (self.pop() as u16) << 8;
        self.pc += 1;
    }

    fn sbc(&mut self, src: u8)
    {
        /* TODO: Add binary mode */
        let carry_in;
        if self.ps.contains(CPU6502_FLAG_C)
        {
            carry_in = 0;
        }
        else
        {
            carry_in = 1;
        }
        let (mut tmp, mut not_carry) = self.a.overflowing_sub(src);
        let carry_sub_result = tmp.overflowing_sub(carry_in);
        tmp = carry_sub_result.0;
        not_carry |= carry_sub_result.1;
        self.ps.set_sign(tmp);
        self.ps.set_overflow(((self.a ^ src) & (self.a ^ tmp) & 0x80) == 0x80);
        self.ps.set_zero(tmp);
        self.ps.set_carry(!not_carry);
        self.a = tmp;
    }

    fn sec(&mut self)
    {
        self.ps.insert(CPU6502_FLAG_C);
    }

    fn sed(&mut self)
    {
        self.ps.insert(CPU6502_FLAG_D);
    }

    fn sei(&mut self)
    {
        self.ps.insert(CPU6502_FLAG_I);
    }

    fn sta(&mut self, addr: u16)
    {
        self.memory.write(addr, self.a);
    }

    fn stx(&mut self, addr: u16)
    {
        self.memory.write(addr, self.x);
    }

    fn sty(&mut self, addr: u16)
    {
        self.memory.write(addr, self.y);
    }

    fn tax(&mut self)
    {
        self.x = self.a;
        self.ps.set_sign(self.x);
        self.ps.set_zero(self.x);
    }

    fn tay(&mut self)
    {
        self.y = self.a;
        self.ps.set_sign(self.y);
        self.ps.set_zero(self.y);
    }

    fn tsx(&mut self)
    {
        self.x = self.sp;
        self.ps.set_sign(self.x);
        self.ps.set_zero(self.x);
    }

    fn txa(&mut self)
    {
        self.a = self.x;
        self.ps.set_sign(self.a);
        self.ps.set_zero(self.a);
    }

    fn txs(&mut self)
    {
        self.sp = self.x;
    }

    fn tya(&mut self)
    {
        self.a = self.y;
        self.ps.set_sign(self.a);
        self.ps.set_zero(self.a);
    }

    /* Meat and bones */
    /// Runs a single instruction.
    pub fn run_opcode(&mut self) -> Cpu6502Result<u32>
    {
        let op_val: u8 = self.memory.read(self.pc);
        let op_el: Opcode = OP_TABLE[op_val as usize];
        let mut additional_cycles = 0;
        let mut src: u8 = 0;
        let mut addr: u16 = 0;

        match op_el.fetch
        {
            FetchType::Accumulator =>
            {
                src = self.a;
                self.pc += 1;
            },
            FetchType::Immediate =>
            {
                src = self.memory.read(self.pc + 1);
                self.pc += 2;
            },
            FetchType::ZeroPage =>
            {
                let addr_u8: u8 = self.memory.read(self.pc + 1);
                addr = addr_u8 as u16;
                src = self.memory.read(addr);
                self.pc += 2;
            },
            FetchType::ZeroPageX =>
            {
                let addr_u8: u8 = self.memory.read(self.pc + 1);
                addr = (addr_u8.wrapping_add(self.x)) as u16;
                src = self.memory.read(addr);
                self.pc += 2;
            },
            FetchType::ZeroPageY =>
            {
                let addr_u8: u8 = self.memory.read(self.pc + 1);
                addr = (addr_u8.wrapping_add(self.y)) as u16;
                src = self.memory.read(addr);
                self.pc += 2;
            },
            FetchType::Absolute =>
            {
                addr = self.memory.read(self.pc + 1);
                src = self.memory.read(addr);
                self.pc += 3;
            },
            FetchType::AbsoluteX =>
            {
                let absolute: u16 = self.memory.read(self.pc + 1);
                let result: (u16, bool) = absolute.overflowing_add(self.x as u16);
                addr = result.0;
                src = self.memory.read(addr);

                /* if an overflow occurs */
                if result.1
                {
                    additional_cycles = 1;
                }
                self.pc += 3;
            },
            FetchType::AbsoluteY =>
            {
                let absolute: u16 = self.memory.read(self.pc + 1);
                let result: (u16, bool) = absolute.overflowing_add(self.y as u16);
                addr = result.0;
                src = self.memory.read(addr);

                /* if an overflow occurs */
                if result.1
                {
                    additional_cycles = 1;
                }
                self.pc += 3;
            },
            FetchType::Indirect =>
            {
                /* TODO: 6502 bug */
                let absolute: u16 = self.memory.read(self.pc + 1);
                addr = self.memory.read(absolute);
                self.pc += 3;
            },
            FetchType::IndirectX =>
            {
                /* TODO: 6502 bug */
                let addr_u8: u8 = self.memory.read(self.pc + 1);
                let indirect_addr = (addr_u8.wrapping_add(self.x)) as u16;
                addr = self.memory.read(indirect_addr);
                src = self.memory.read(addr);
                self.pc += 2;
            },
            FetchType::IndirectY =>
            {
                /* TODO: 6502 bug */
                let indirect_addr: u8 = self.memory.read(self.pc + 1);
                let preindexed_addr: u16 = self.memory.read(indirect_addr as u16);
                let result: (u16, bool) = preindexed_addr.overflowing_add(self.y as u16);
                addr = result.0;
                src = self.memory.read(addr);

                /* if an overflow occurs */
                if result.1
                {
                    additional_cycles = 1;
                }
                self.pc += 2;
            },
            FetchType::Implicit =>
            {
                self.pc += 1;
            },
        }

//        println!("src = {:02X} addr = {:04X}", src, addr);

        match op_el.op
        {
            Operation::Adc =>
            {
                self.adc(src);
            },
            Operation::And =>
            {
                self.and(src);
            },
            Operation::Asl =>
            {
                self.asl(src, addr, op_el.fetch);
            },
            Operation::Bcc =>
            {
                self.bcc(src, &mut additional_cycles);
            },
            Operation::Bcs =>
            {
                self.bcs(src, &mut additional_cycles);
            },
            Operation::Beq =>
            {
                self.beq(src, &mut additional_cycles);
            },
            Operation::Bit =>
            {
                self.bit(src);
            },
            Operation::Bmi =>
            {
                self.bmi(src, &mut additional_cycles);
            },
            Operation::Bne =>
            {
                self.bne(src, &mut additional_cycles);
            },
            Operation::Bpl =>
            {
                self.bpl(src, &mut additional_cycles);
            },
            Operation::Brk =>
            {
                self.brk();
            },
            Operation::Bvc =>
            {
                self.bvc(src, &mut additional_cycles);
            },
            Operation::Bvs =>
            {
                self.bvs(src, &mut additional_cycles);
            },
            Operation::Clc =>
            {
                self.clc();
            },
            Operation::Cld =>
            {
                self.cld();
            },
            Operation::Cli =>
            {
                self.cli();
            },
            Operation::Clv =>
            {
                self.clv();
            },
            Operation::Cmp =>
            {
                self.cmp(src);
            },
            Operation::Cpx =>
            {
                self.cpx(src);
            },
            Operation::Cpy =>
            {
                self.cpy(src);
            },
            Operation::Dec =>
            {
                self.dec(src, addr);
            },
            Operation::Dex =>
            {
                self.dex();
            },
            Operation::Dey =>
            {
                self.dey();
            },
            Operation::Eor =>
            {
                self.eor(src);
            },
            Operation::Inc =>
            {
                self.inc(src, addr);
            },
            Operation::Inx =>
            {
                self.inx();
            },
            Operation::Iny =>
            {
                self.iny();
            },
            Operation::Jmp =>
            {
                self.jmp(addr);
            },
            Operation::Jsr =>
            {
                self.jsr(addr);
            },
            Operation::Lda =>
            {
                self.lda(src);
            },
            Operation::Ldx =>
            {
                self.ldx(src);
            },
            Operation::Ldy =>
            {
                self.ldy(src);
            },
            Operation::Lsr =>
            {
                self.lsr(src, addr, op_el.fetch);
            },
            Operation::Nop =>
            {
            },
            Operation::Ora =>
            {
                self.ora(src);
            },
            Operation::Pha =>
            {
                self.pha();
            },
            Operation::Php =>
            {
                self.php();
            },
            Operation::Pla =>
            {
                self.pla();
            },
            Operation::Plp =>
            {
                self.plp();
            },
            Operation::Rol =>
            {
                self.rol(src, addr, op_el.fetch);
            },
            Operation::Ror =>
            {
                self.ror(src, addr, op_el.fetch);
            },
            Operation::Rti =>
            {
                self.rti();
            },
            Operation::Rts =>
            {
                self.rts();
            },
            Operation::Sbc =>
            {
                self.sbc(src);
            },
            Operation::Sec =>
            {
                self.sec();
            },
            Operation::Sed =>
            {
                self.sed();
            },
            Operation::Sei =>
            {
                self.sei();
            },
            Operation::Sta =>
            {
                self.sta(addr);
            },
            Operation::Stx =>
            {
                self.stx(addr);
            },
            Operation::Sty =>
            {
                self.sty(addr);
            },
            Operation::Tax =>
            {
                self.tax();
            },
            Operation::Tay =>
            {
                self.tay();
            },
            Operation::Tsx =>
            {
                self.tsx();
            },
            Operation::Txa =>
            {
                self.txa();
            },
            Operation::Txs =>
            {
                self.txs();
            },
            Operation::Tya =>
            {
                self.tya();
            },
            Operation::Unknown =>
            {
                return Err(Cpu6502Error::IllegalOpcode(op_val));
            },
        }

        let cycles = op_el.cycles + additional_cycles;
        self.cycles += cycles as u64;
        Ok(cycles)
    }
}

impl<M> fmt::Debug for Cpu6502<M>
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result
    {
//        let op: u8 = self.memory.read_without_mm(self.pc);
//        try!(write!(fmt, "OP: 0x{:02X}\n", op));
        try!(write!(fmt, " X: 0x{:02X}     Y: 0x{:02X}\n", self.x, self.y));
        try!(write!(fmt, " A: 0x{:02X}    PS: {}\n", self.a, self.ps));
        try!(write!(fmt, "PC: 0x{:04X}  SP: 0x{:02X}", self.pc, self.sp));
        Ok(())
    }
}
