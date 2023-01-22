use core::mem::transmute;
use core::ops::Range;

pub type Register = u32;

#[derive(Copy, Clone, Default, Debug, PartialEq, Eq)]
pub enum DataType {
    #[default]
    Unknown,
    I32,
    F32,
    Range,
}

pub fn register_to_int(reg: Register) -> i32 {
    unsafe { transmute(reg) }
}

pub fn int_to_register(val: i32) -> Register {
    unsafe { transmute(val) }
}

#[cfg(test)]
pub fn register_to_float(reg: Register) -> f32 {
    unsafe { transmute(reg) }
}

pub fn float_to_register(val: f32) -> Register {
    unsafe { transmute(val) }
}

pub fn range_to_register(start: u16, end_exclusive: u16) -> Register {
    let [a, b] = start.to_be_bytes();
    let [c, d] = end_exclusive.to_be_bytes();
    u32::from_be_bytes([a, b, c, d])
}

pub fn register_to_range(reg: Register) -> Range<usize> {
    let [a, b, c, d] = reg.to_be_bytes();
    let start = u16::from_be_bytes([a, b]) as usize;
    let end = u16::from_be_bytes([c, d]) as usize;
    start..end
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integers() {
        assert_eq!(register_to_int(int_to_register(0)), 0);
        assert_eq!(register_to_int(int_to_register(2)), 2);
        assert_eq!(register_to_int(int_to_register(-1)), -1);
        assert_eq!(register_to_int(int_to_register(i32::MAX)), i32::MAX);
        assert_eq!(register_to_int(int_to_register(i32::MIN)), i32::MIN);
    }

    #[test]
    fn floats() {
        assert_eq!(register_to_float(float_to_register(0.0)), 0.0);
        assert_eq!(register_to_float(float_to_register(2.0)), 2.0);
        assert_eq!(register_to_float(float_to_register(-1.0)), -1.0);
        assert_eq!(register_to_float(float_to_register(-1.5)), -1.5);
        assert_eq!(register_to_float(float_to_register(0.125)), 0.125);
        assert_eq!(register_to_float(float_to_register(f32::MAX)), f32::MAX);
        assert_eq!(register_to_float(float_to_register(f32::MIN)), f32::MIN);
    }

    #[test]
    fn ranges() {
        assert_eq!(register_to_range(range_to_register(0, 1)), 0..1);
        assert_eq!(register_to_range(range_to_register(0, 2)), 0..2);
        assert_eq!(register_to_range(range_to_register(5, 19)), 5..19);
    }
}
