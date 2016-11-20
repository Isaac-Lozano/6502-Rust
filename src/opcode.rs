/*************
 * FetchType *
 *************/
#[derive(Clone,Copy)]
pub enum FetchType
{
    Accumulator,
    /* Immediate also doubles as Relative */
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    Implicit,
}

/*************
 * Operation *
 *************/
#[derive(Clone,Copy)]
pub enum Operation
{
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
    Unknown,
}


/**********
 * Opcode *
 **********/
/* This is an adapted version of what FCEUX does for opcodes */
#[derive(Clone,Copy)]
pub struct Opcode
{
    pub fetch: FetchType,
    pub cycles: u32,
    pub op: Operation,
}

impl Opcode
{
    pub fn print(&self, operand: u16)
    {
        match self.op
        {
            Operation::Adc =>
                print!("ADC"),
            Operation::And =>
                print!("AND"),
            Operation::Asl =>
                print!("ASL"),
            Operation::Bcc =>
                print!("BCC"),
            Operation::Bcs =>
                print!("BCS"),
            Operation::Beq =>
                print!("BEQ"),
            Operation::Bit =>
                print!("BIT"),
            Operation::Bmi =>
                print!("BMI"),
            Operation::Bne =>
                print!("BNE"),
            Operation::Bpl =>
                print!("BPL"),
            Operation::Brk =>
                print!("BRK"),
            Operation::Bvc =>
                print!("BVC"),
            Operation::Bvs =>
                print!("BVS"),
            Operation::Clc =>
                print!("CLC"),
            Operation::Cld =>
                print!("CLD"),
            Operation::Cli =>
                print!("CLI"),
            Operation::Clv =>
                print!("CLV"),
            Operation::Cmp =>
                print!("CMP"),
            Operation::Cpx =>
                print!("CPX"),
            Operation::Cpy =>
                print!("CPY"),
            Operation::Dec =>
                print!("DEC"),
            Operation::Dex =>
                print!("DEX"),
            Operation::Dey =>
                print!("DEY"),
            Operation::Eor =>
                print!("EOR"),
            Operation::Inc =>
                print!("INC"),
            Operation::Inx =>
                print!("INX"),
            Operation::Iny =>
                print!("INY"),
            Operation::Jmp =>
                print!("JMP"),
            Operation::Jsr =>
                print!("JSR"),
            Operation::Lda =>
                print!("LDA"),
            Operation::Ldx =>
                print!("LDX"),
            Operation::Ldy =>
                print!("LDY"),
            Operation::Lsr =>
                print!("LSR"),
            Operation::Nop =>
                print!("NOP"),
            Operation::Ora =>
                print!("ORA"),
            Operation::Pha =>
                print!("PHA"),
            Operation::Php =>
                print!("PHP"),
            Operation::Pla =>
                print!("PLA"),
            Operation::Plp =>
                print!("PLP"),
            Operation::Rol =>
                print!("ROL"),
            Operation::Ror =>
                print!("ROR"),
            Operation::Rti =>
                print!("RTI"),
            Operation::Rts =>
                print!("RTS"),
            Operation::Sbc =>
                print!("SBC"),
            Operation::Sec =>
                print!("SEC"),
            Operation::Sed =>
                print!("SED"),
            Operation::Sei =>
                print!("SEI"),
            Operation::Sta =>
                print!("STA"),
            Operation::Stx =>
                print!("STX"),
            Operation::Sty =>
                print!("STY"),
            Operation::Tax =>
                print!("TAX"),
            Operation::Tay =>
                print!("TAY"),
            Operation::Tsx =>
                print!("TSX"),
            Operation::Txa =>
                print!("TXA"),
            Operation::Txs =>
                print!("TXS"),
            Operation::Tya =>
                print!("TYA"),
            Operation::Unknown =>
                print!("ERR"),
        }

        match self.fetch
        {
            FetchType::Accumulator =>
            {
            }
            FetchType::Immediate =>
                print!("#${:02x}", operand as u8),
            FetchType::ZeroPage =>
                print!("${:02x}", operand as u8),
            FetchType::ZeroPageX =>
                print!("${:02x}, x", operand as u8),
            FetchType::ZeroPageY =>
                print!("${:02x}, y", operand as u8),
            FetchType::Absolute =>
                print!("${:04x}", operand),
            FetchType::AbsoluteX =>
                print!("${:04x}, x", operand),
            FetchType::AbsoluteY =>
                print!("${:04x}, y", operand),
            FetchType::Indirect =>
                print!("(${:04x})", operand),
            FetchType::IndirectX =>
                print!("(${:02x}, x)", operand as u8),
            FetchType::IndirectY =>
                print!("(${:02x}), y", operand as u8),
            FetchType::Implicit =>
            {
            }
        }
        println!("");
    }
}

pub static OP_TABLE: [Opcode; 0x100] =
[
/* 0x00 */    Opcode{ fetch: FetchType::Implicit, cycles: 7, op: Operation::Brk },
/* 0x01 */   Opcode{ fetch: FetchType::IndirectX, cycles: 6, op: Operation::Ora },
/* 0x02 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x03 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x04 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x05 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Ora },
/* 0x06 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 5, op: Operation::Asl },
/* 0x07 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x08 */   Opcode{ fetch: FetchType::Implicit, cycles: 3, op: Operation::Php },
/* 0x09 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Ora },
/* 0x0a */   Opcode{ fetch: FetchType::Accumulator, cycles: 2, op: Operation::Asl },
/* 0x0b */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x0c */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x0d */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Ora },
/* 0x0e */   Opcode{ fetch: FetchType::Absolute, cycles: 6, op: Operation::Asl },
/* 0x0f */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x10 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Bpl },
/* 0x11 */   Opcode{ fetch: FetchType::IndirectY, cycles: 5, op: Operation::Ora },
/* 0x12 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x13 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x14 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x15 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 4, op: Operation::Ora },
/* 0x16 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 5, op: Operation::Asl },
/* 0x17 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x18 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Clc },
/* 0x19 */   Opcode{ fetch: FetchType::AbsoluteY, cycles: 4, op: Operation::Ora },
/* 0x1a */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x1b */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x1c */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x1d */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 4, op: Operation::Ora },
/* 0x1e */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 7, op: Operation::Asl },
/* 0x1f */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x20 */   Opcode{ fetch: FetchType::Absolute, cycles: 6, op: Operation::Jsr },
/* 0x21 */   Opcode{ fetch: FetchType::IndirectX, cycles: 6, op: Operation::And },
/* 0x22 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x23 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x24 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Bit },
/* 0x25 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::And },
/* 0x26 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 5, op: Operation::Rol },
/* 0x27 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x28 */   Opcode{ fetch: FetchType::Implicit, cycles: 4, op: Operation::Plp },
/* 0x29 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::And },
/* 0x2a */   Opcode{ fetch: FetchType::Accumulator, cycles: 2, op: Operation::Rol },
/* 0x2b */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x2c */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Bit },
/* 0x2d */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::And },
/* 0x2e */   Opcode{ fetch: FetchType::Absolute, cycles: 6, op: Operation::Rol },
/* 0x2f */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x30 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Bmi },
/* 0x31 */   Opcode{ fetch: FetchType::IndirectY, cycles: 5, op: Operation::And },
/* 0x32 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x33 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x34 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x35 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 4, op: Operation::And },
/* 0x36 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 6, op: Operation::Rol },
/* 0x37 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x38 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Sec },
/* 0x39 */   Opcode{ fetch: FetchType::AbsoluteY, cycles: 4, op: Operation::And },
/* 0x3a */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x3b */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x3c */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x3d */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 4, op: Operation::And },
/* 0x3e */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 7, op: Operation::Rol },
/* 0x3f */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x40 */   Opcode{ fetch: FetchType::Implicit, cycles: 6, op: Operation::Rti },
/* 0x41 */   Opcode{ fetch: FetchType::IndirectY, cycles: 5, op: Operation::Eor },
/* 0x42 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x43 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x44 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x45 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Eor },
/* 0x46 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 5, op: Operation::Lsr },
/* 0x47 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x48 */   Opcode{ fetch: FetchType::Implicit, cycles: 3, op: Operation::Pha },
/* 0x49 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Eor },
/* 0x4a */   Opcode{ fetch: FetchType::Accumulator, cycles: 2, op: Operation::Lsr },
/* 0x4b */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x4c */   Opcode{ fetch: FetchType::Absolute, cycles: 3, op: Operation::Jmp },
/* 0x4d */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Eor },
/* 0x4e */   Opcode{ fetch: FetchType::Absolute, cycles: 6, op: Operation::Lsr },
/* 0x4f */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x50 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Bvc },
/* 0x51 */   Opcode{ fetch: FetchType::IndirectY, cycles: 5, op: Operation::Eor },
/* 0x52 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x53 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x54 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x55 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 4, op: Operation::Eor },
/* 0x56 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 6, op: Operation::Lsr },
/* 0x57 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x58 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Cli },
/* 0x59 */   Opcode{ fetch: FetchType::AbsoluteY, cycles: 4, op: Operation::Eor },
/* 0x5a */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x5b */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x5c */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x5d */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 4, op: Operation::Eor },
/* 0x5e */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 7, op: Operation::Lsr },
/* 0x5f */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x60 */   Opcode{ fetch: FetchType::Implicit, cycles: 6, op: Operation::Rts },
/* 0x61 */   Opcode{ fetch: FetchType::IndirectX, cycles: 6, op: Operation::Adc },
/* 0x62 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x63 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x64 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x65 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Adc },
/* 0x66 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 5, op: Operation::Ror },
/* 0x67 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x68 */   Opcode{ fetch: FetchType::Implicit, cycles: 4, op: Operation::Pla },
/* 0x69 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Adc },
/* 0x6a */   Opcode{ fetch: FetchType::Accumulator, cycles: 2, op: Operation::Ror },
/* 0x6b */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x6c */   Opcode{ fetch: FetchType::Indirect, cycles: 5, op: Operation::Jmp },
/* 0x6d */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Adc },
/* 0x6e */   Opcode{ fetch: FetchType::Absolute, cycles: 6, op: Operation::Ror },
/* 0x6f */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x70 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Bvs },
/* 0x71 */   Opcode{ fetch: FetchType::IndirectY, cycles: 5, op: Operation::Adc },
/* 0x72 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x73 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x74 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x75 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 4, op: Operation::Adc },
/* 0x76 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 6, op: Operation::Ror },
/* 0x77 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x78 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Sei },
/* 0x79 */   Opcode{ fetch: FetchType::AbsoluteY, cycles: 4, op: Operation::Adc },
/* 0x7a */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x7b */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x7c */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x7d */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 4, op: Operation::Adc },
/* 0x7e */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 7, op: Operation::Ror },
/* 0x7f */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x80 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x81 */   Opcode{ fetch: FetchType::IndirectX, cycles: 6, op: Operation::Sta },
/* 0x82 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x83 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x84 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Sty },
/* 0x85 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Sta },
/* 0x86 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Stx },
/* 0x87 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x88 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Dey },
/* 0x89 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x8a */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Txa },
/* 0x8b */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x8c */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Sty },
/* 0x8d */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Sta },
/* 0x8e */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Stx },
/* 0x8f */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x90 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Bcc },
/* 0x91 */   Opcode{ fetch: FetchType::IndirectY, cycles: 6, op: Operation::Sta },
/* 0x92 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x93 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x94 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 4, op: Operation::Sty },
/* 0x95 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 4, op: Operation::Sta },
/* 0x96 */   Opcode{ fetch: FetchType::ZeroPageY, cycles: 4, op: Operation::Stx },
/* 0x97 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x98 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Tya },
/* 0x99 */   Opcode{ fetch: FetchType::AbsoluteY, cycles: 5, op: Operation::Sta },
/* 0x9a */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Txs },
/* 0x9b */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x9c */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x9d */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 5, op: Operation::Sta },
/* 0x9e */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0x9f */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xa0 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Ldy },
/* 0xa1 */   Opcode{ fetch: FetchType::IndirectX, cycles: 6, op: Operation::Lda },
/* 0xa2 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Ldx },
/* 0xa3 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xa4 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Ldy },
/* 0xa5 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Lda },
/* 0xa6 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Ldx },
/* 0xa7 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xa8 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Tay },
/* 0xa9 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Lda },
/* 0xaa */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Tax },
/* 0xab */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xac */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Ldy },
/* 0xad */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Lda },
/* 0xae */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Ldx },
/* 0xaf */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xb0 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Bcs },
/* 0xb1 */   Opcode{ fetch: FetchType::IndirectY, cycles: 5, op: Operation::Lda },
/* 0xb2 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xb3 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xb4 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 4, op: Operation::Ldy },
/* 0xb5 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 4, op: Operation::Lda },
/* 0xb6 */   Opcode{ fetch: FetchType::ZeroPageY, cycles: 4, op: Operation::Ldx },
/* 0xb7 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xb8 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Clv },
/* 0xb9 */   Opcode{ fetch: FetchType::AbsoluteY, cycles: 4, op: Operation::Lda },
/* 0xba */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Tsx },
/* 0xbb */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xbc */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 4, op: Operation::Ldy },
/* 0xbd */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 4, op: Operation::Lda },
/* 0xbe */   Opcode{ fetch: FetchType::AbsoluteY, cycles: 4, op: Operation::Ldx },
/* 0xbf */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xc0 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Cpy },
/* 0xc1 */   Opcode{ fetch: FetchType::IndirectX, cycles: 6, op: Operation::Cmp },
/* 0xc2 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xc3 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xc4 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Cpy },
/* 0xc5 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Cmp },
/* 0xc6 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 5, op: Operation::Dec },
/* 0xc7 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xc8 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Iny },
/* 0xc9 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Cmp },
/* 0xca */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Dex },
/* 0xcb */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xcc */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Cpy },
/* 0xcd */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Cmp },
/* 0xce */   Opcode{ fetch: FetchType::Absolute, cycles: 6, op: Operation::Dec },
/* 0xcf */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xd0 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Bne },
/* 0xd1 */   Opcode{ fetch: FetchType::IndirectY, cycles: 5, op: Operation::Cmp },
/* 0xd2 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xd3 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xd4 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xd5 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 4, op: Operation::Cmp },
/* 0xd6 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 6, op: Operation::Dec },
/* 0xd7 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xd8 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Cld },
/* 0xd9 */   Opcode{ fetch: FetchType::AbsoluteY, cycles: 4, op: Operation::Cmp },
/* 0xda */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xdb */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xdc */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xdd */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 4, op: Operation::Cmp },
/* 0xde */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 7, op: Operation::Dec },
/* 0xdf */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xe0 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Cpx },
/* 0xe1 */   Opcode{ fetch: FetchType::IndirectX, cycles: 6, op: Operation::Sbc },
/* 0xe2 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xe3 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xe4 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Cpx },
/* 0xe5 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 3, op: Operation::Sbc },
/* 0xe6 */   Opcode{ fetch: FetchType::ZeroPage, cycles: 5, op: Operation::Inc },
/* 0xe7 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xe8 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Inx },
/* 0xe9 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Sbc },
/* 0xea */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Nop },
/* 0xeb */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xec */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Cpx },
/* 0xed */   Opcode{ fetch: FetchType::Absolute, cycles: 4, op: Operation::Sbc },
/* 0xee */   Opcode{ fetch: FetchType::Absolute, cycles: 6, op: Operation::Inc },
/* 0xef */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xf0 */   Opcode{ fetch: FetchType::Immediate, cycles: 2, op: Operation::Beq },
/* 0xf1 */   Opcode{ fetch: FetchType::IndirectY, cycles: 5, op: Operation::Sbc },
/* 0xf2 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xf3 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xf4 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xf5 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 4, op: Operation::Sbc },
/* 0xf6 */   Opcode{ fetch: FetchType::ZeroPageX, cycles: 6, op: Operation::Inc },
/* 0xf7 */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xf8 */   Opcode{ fetch: FetchType::Implicit, cycles: 2, op: Operation::Sed },
/* 0xf9 */   Opcode{ fetch: FetchType::AbsoluteY, cycles: 4, op: Operation::Sbc },
/* 0xfa */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xfb */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xfc */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
/* 0xfd */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 4, op: Operation::Sbc },
/* 0xfe */   Opcode{ fetch: FetchType::AbsoluteX, cycles: 7, op: Operation::Inc },
/* 0xff */   Opcode{ fetch: FetchType::Implicit, cycles: 0, op: Operation::Unknown },
];
