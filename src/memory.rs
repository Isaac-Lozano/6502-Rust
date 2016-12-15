const NUM_PAGES: usize = 0x100;
const PAGE_SIZE: usize = 0x100;

/**********
 * Memory *
 **********/
/* Lets us get u8 or u16 from
 * mem as needed
 */
/// Memory mapper trait.
///
/// Contains read and write functions for getting and setting memory. Also includes
/// non-memory-mapped versions in case one wants to implement a debugger and
/// wants to be able to read memory without triggering memory maps.
pub trait Memory<T>
{
    fn read_without_mm(&mut self, addr: u16) -> T;
    fn write_without_mm(&mut self, addr: u16, val: T);

    fn read(&mut self, addr: u16) -> T
    {
        self.read_without_mm(addr)
    }

    fn write(&mut self, addr: u16, val: T)
    {
        self.write_without_mm(addr, val)
    }
}

impl<T> Memory<u16> for T where T:Memory<u8>
{
    fn read(&mut self, addr: u16) -> u16
    {
        let low_byte: u8 = self.read(addr);
        let high_byte: u8 = self.read(addr + 1);
        low_byte as u16 | (high_byte as u16) << 8
    }

    fn read_without_mm(&mut self, addr: u16) -> u16
    {
        let low_byte: u8 = self.read_without_mm(addr);
        let high_byte: u8 = self.read_without_mm(addr + 1);
        low_byte as u16 | (high_byte as u16) << 8
    }

    fn write(&mut self, addr: u16, val: u16)
    {
        self.write(addr, (val & 0xFF) as u8);
        self.write(addr + 1, (val >> 8) as u8);
    }

    fn write_without_mm(&mut self, addr: u16, val: u16)
    {
        self.write_without_mm(addr, (val & 0xFF) as u8);
        self.write_without_mm(addr + 1, (val >> 8) as u8);
    }
}

/// A simple memory implementation.
///
/// SimpleMemory is a simple contiguous 64k RAM with no memory mapping. Useful
/// for simple 6502 tests.
pub struct SimpleMemory
{
    pub mem: [u8; NUM_PAGES * PAGE_SIZE],
}

impl SimpleMemory
{
    pub fn new() -> SimpleMemory
    {
        SimpleMemory{ mem: [0x00; NUM_PAGES * PAGE_SIZE] }
    }
}

impl Memory<u8> for SimpleMemory
{
    fn read_without_mm(&mut self, addr: u16) -> u8
    {
        self.mem[addr as usize]
    }

    fn write_without_mm(&mut self, addr: u16, val: u8)
    {
        self.mem[addr as usize] = val;
    }
}
