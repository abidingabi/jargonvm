// see section 2.3 of the spec
enum Primitive {
    Integral(Integral),
    Floating(Float),
    Boolean(bool),
    ReturnAddress(u32),
}

enum Integral {
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Char(u16),
}

enum Floating {
    Float(f32),
    Double(f64),
}
