pub mod cpu6502;
pub mod memory;
pub mod opcode;

#[macro_use]
extern crate bitflags;

#[cfg(test)]
mod tests {
    use cpu6502::CPU6502;
    use memory::{SimpleMemory, Memory};

    use std::io::prelude::*;
    use std::fs::File;
    use std::error::Error;

    #[test]
    fn cpu_simple_test() {
        let mut mem = SimpleMemory::new();

        /* Set reset vector */
        mem.set(0xFFFD, 0xFF as u8);
        mem.set(0xFFFF, 0xFC as u8);

        /* adc #$42 */
        mem.set(0xFF00, 0x69 as u8);
        mem.set(0xFF01, 0x42 as u8);

        /* adc $FF00 ; ($69)*/
        mem.set(0xFF02, 0x6D as u8);
        mem.set(0xFF03, 0x00 as u8);
        mem.set(0xFF04, 0xFF as u8);

        let mut cpu = CPU6502::new(mem);
        cpu.reset();
        println!("{:?}\n", cpu);
        cpu.run(1).unwrap();
        println!("{:?}\n", cpu);
        assert!(cpu.a == 0x42);
        cpu.run(1).unwrap();
        println!("{:?}\n", cpu);
        assert!(cpu.a == 0xAB);
    }

    #[test]
    fn cpu_full_test() {
        let mut mem = SimpleMemory::new();
        let mut file = File::open("test_data/6502_functional_test.bin").unwrap();
        file.read(&mut mem.mem).unwrap();
        let mut cpu = CPU6502::new(mem);

        let mut cycle_count = 0;
        let mut last_pc = 0x0000;
        cpu.pc = 0x1000;
        while last_pc != cpu.pc
        {
            last_pc = cpu.pc;
            match cpu.run(1)
            {
                Ok(cycles) =>
                    cycle_count += cycles,
                Err(err) =>
                {
                    println!("Error {}: {}", err, err.description());
                    assert!(false);
                }
            }
        }
        println!("Program exited normally after {} cycles.", cycle_count);
    }
}
