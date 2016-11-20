const NUM_PAGES: usize = 0x100;
const PAGE_SIZE: usize = 0x100;

/**********
 * Memory *
 **********/
/* Lets us get u8 or u16 from
 * mem as needed
 */
pub trait Memory<T>
{
    fn get_without_mm(&mut self, addr: u16) -> T;
    fn set_without_mm(&mut self, addr: u16, val: T);

    fn get(&mut self, addr: u16) -> T
    {
        self.get_without_mm(addr)
    }

    fn set(&mut self, addr: u16, val: T)
    {
        self.set_without_mm(addr, val)
    }
}

impl<T> Memory<u16> for T where T:Memory<u8>
{
    fn get(&mut self, addr: u16) -> u16
    {
        let low_byte: u8 = self.get(addr);
        let high_byte: u8 = self.get(addr + 1);
        low_byte as u16 | (high_byte as u16) << 8
    }

    fn get_without_mm(&mut self, addr: u16) -> u16
    {
        let low_byte: u8 = self.get_without_mm(addr);
        let high_byte: u8 = self.get_without_mm(addr + 1);
        low_byte as u16 | (high_byte as u16) << 8
    }

    fn set(&mut self, addr: u16, val: u16)
    {
        self.set(addr, (val & 0xFF) as u8);
        self.set(addr + 1, (val >> 8) as u8);
    }

    fn set_without_mm(&mut self, addr: u16, val: u16)
    {
        self.set_without_mm(addr, (val & 0xFF) as u8);
        self.set_without_mm(addr + 1, (val >> 8) as u8);
    }
}

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
    fn get_without_mm(&mut self, addr: u16) -> u8
    {
        self.mem[addr as usize]
    }

    fn set_without_mm(&mut self, addr: u16, val: u8)
    {
        self.mem[addr as usize] = val;
    }
}
