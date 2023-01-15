use core::mem::transmute;

pub type Register = u32;

#[derive(Copy, Clone, Default, Debug, PartialEq, Eq)]
pub enum DataType {
    #[default]
    I32,
    F32,
}

pub fn register_to_int(reg: Register) -> i32 {
    unsafe { transmute(reg) }
}

pub fn int_to_register(val: i32) -> Register {
    unsafe { transmute(val) }
}

pub fn register_to_float(reg: Register) -> f32 {
    unsafe { transmute(reg) }
}

pub fn float_to_register(val: f32) -> Register {
    unsafe { transmute(val) }
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
}
