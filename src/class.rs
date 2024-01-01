use std::convert::{From, TryFrom};
use std::io::{Error, ErrorKind, Read};
use std::rc::Rc;

#[derive(Debug)]
pub struct ClassFile {
    minor_version: u16,
    major_version: u16,
    constant_pool: Vec<Constant>,
    access_flags: ClassAccessFlags,
    this_class: u16,
    super_class: u16,
    interfaces: Vec<u16>,
    fields: Vec<Field>,
}

// see section 4.1 of the spec
pub fn parse_class_file<T: Read>(reader: &mut T) -> std::io::Result<ClassFile> {
    // check magic bytes
    if parse_u32(reader)? != 0xCAFEBABE {
        return make_parse_err("class file must start with 0xCAFEBABE");
    }

    let minor_version = parse_u16(reader)?;
    let major_version = parse_u16(reader)?;

    let constant_pool = {
        let constant_pool_count = parse_u16(reader)?;
        // constant pool count is 1 plus the number of constants
        let mut constant_pool = Vec::with_capacity((constant_pool_count - 1).into());
        for _ in 1..constant_pool_count {
            constant_pool.push(parse_constant(reader)?);
        }

        constant_pool
    };

    let access_flags: ClassAccessFlags = parse_u16(reader)?.try_into()?;

    // TODO: the appropriate checks at the end of 4.1 of the spec for modules

    let this_class = parse_u16(reader)?;
    let super_class = parse_u16(reader)?;

    let interfaces = {
        let interface_count = parse_u16(reader)?;
        let mut interfaces = Vec::with_capacity(interface_count.into());
        for _ in 0..interface_count {
            interfaces.push(parse_u16(reader)?);
        }

        interfaces
    };

    let fields = {
        let field_count = parse_u16(reader)?;
        let mut fields = Vec::with_capacity(field_count.into());
        for _ in 0..field_count {
            fields.push(parse_field(reader, constant_pool)?);
        }

        fields
    };

    Ok(ClassFile {
        minor_version,
        major_version,
        constant_pool,
        access_flags,
        this_class,
        super_class,
        interfaces,
        fields,
    })
}

// See section 4.4 of the spec
#[derive(Debug)]
pub enum Constant {
    Utf8(String),
    FieldRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    MethodRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    InterfaceMethodRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    NameAndType {
        name_index: u16,
        descriptor_index: u16,
    },
    InvokeDynamic {
        bootstrap_method_attr_index: u16,
        name_and_type_index: u16,
    },
    Module {
        name_index: u16,
    },
    Package {
        name_index: u16,
    },
    Loadable(LoadableConstant),
}

#[derive(Debug)]
pub enum LoadableConstant {
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class {
        name_index: u16,
    },
    String {
        string_index: u16,
    },
    MethodHandle {
        reference_kind: ReferenceKind,
        reference_index: u16,
    },
    MethodType {
        descriptor_index: u16,
    },
    Dynamic {
        bootstrap_method_attr_index: u16,
        name_and_type_index: u16,
    },
}

impl From<LoadableConstant> for Constant {
    fn from(loadable_constant: LoadableConstant) -> Self {
        Constant::Loadable(loadable_constant)
    }
}

#[derive(Debug)]
pub enum ReferenceKind {
    GetField,
    GetStatic,
    PutField,
    PutStatic,
    InvokeVirtual,
    InvokeStatic,
    InvokeSpecial,
    NewInvokeSpecial,
    InvokeInterface,
}

impl TryFrom<u8> for ReferenceKind {
    type Error = Error;

    fn try_from(reference_kind_int: u8) -> Result<Self, Self::Error> {
        match reference_kind_int {
            1 => Ok(ReferenceKind::GetField),
            2 => Ok(ReferenceKind::GetStatic),
            3 => Ok(ReferenceKind::PutField),
            4 => Ok(ReferenceKind::PutStatic),
            5 => Ok(ReferenceKind::InvokeVirtual),
            6 => Ok(ReferenceKind::InvokeStatic),
            7 => Ok(ReferenceKind::InvokeSpecial),
            8 => Ok(ReferenceKind::NewInvokeSpecial),
            9 => Ok(ReferenceKind::InvokeInterface),
            _ => make_parse_err("Invalid reference kind for method handle!"),
        }
    }
}

pub fn parse_constant<T: Read>(reader: &mut T) -> std::io::Result<Constant> {
    use Constant as C;
    use LoadableConstant as LC;

    let tag = parse_u8(reader)?;
    match tag {
        // CONSTANT_Utf8
        1 => Ok(C::Utf8(parse_utf8(reader)?)),

        // Constant_Integer
        3 => {
            let mut buf = [0u8; 4];
            reader.read_exact(&mut buf)?;
            Ok(LC::Integer(i32::from_be_bytes(buf)).into())
        }

        // CONSTANT_Float
        4 => {
            let mut buf = [0u8; 4];
            reader.read_exact(&mut buf)?;
            Ok(LC::Float(f32::from_be_bytes(buf)).into())
        }

        // CONSTANT_Long
        5 => {
            let mut buf = [0u8; 8];
            reader.read_exact(&mut buf)?;
            Ok(LC::Long(i64::from_be_bytes(buf)).into())
        }

        // CONSTANT_Double
        6 => {
            let mut buf = [0u8; 8];
            reader.read_exact(&mut buf)?;
            Ok(LC::Double(f64::from_be_bytes(buf)).into())
        }

        // CONSTANT_Class
        7 => Ok(LC::Class {
            name_index: parse_u16(reader)?,
        }
        .into()),

        // CONSTANT_String
        8 => Ok(LC::String {
            string_index: parse_u16(reader)?,
        }
        .into()),

        // CONSTANT_Fieldref
        9 => Ok(C::FieldRef {
            class_index: parse_u16(reader)?,
            name_and_type_index: parse_u16(reader)?,
        }),

        // CONSTANT_Methodref
        10 => Ok(C::MethodRef {
            class_index: parse_u16(reader)?,
            name_and_type_index: parse_u16(reader)?,
        }),

        // CONSTANT_InterfaceMethodref
        11 => Ok(C::InterfaceMethodRef {
            class_index: parse_u16(reader)?,
            name_and_type_index: parse_u16(reader)?,
        }),

        // CONSTANT_NameAndType
        12 => Ok(C::NameAndType {
            name_index: parse_u16(reader)?,
            descriptor_index: parse_u16(reader)?,
        }),

        // CONSTANT_MethodHandle
        15 => Ok(LC::MethodHandle {
            reference_kind: ReferenceKind::try_from(parse_u8(reader)?)?,
            reference_index: parse_u16(reader)?,
        }
        .into()),

        // CONSTANT_MethodType
        16 => Ok(LC::MethodType {
            descriptor_index: parse_u16(reader)?,
        }
        .into()),

        // CONSTANT_Dynamic
        17 => Ok(LC::Dynamic {
            bootstrap_method_attr_index: parse_u16(reader)?,
            name_and_type_index: parse_u16(reader)?,
        }
        .into()),

        // CONSTANT_InvokeDynamic
        18 => Ok(Constant::InvokeDynamic {
            bootstrap_method_attr_index: parse_u16(reader)?,
            name_and_type_index: parse_u16(reader)?,
        }),

        // CONSTANT_Module
        19 => Ok(Constant::Module {
            name_index: parse_u16(reader)?,
        }),

        // CONSTANT_Package
        20 => Ok(Constant::Package {
            name_index: parse_u16(reader)?,
        }),

        _ => make_parse_err("Invalid tag for constant in constant pool!"),
    }
}

// See section 4.4.7 of the spec
// excerpt: "String content is encoded in modified UTF-8."
// ohno.png
fn parse_utf8<T: Read>(reader: &mut T) -> std::io::Result<String> {
    let length = parse_u16(reader)?;
    // length does not necessarily correspond to the number of codepoints here,
    // but it is a decent estimate
    let mut result = String::with_capacity(length.into());

    let mut i = 0;
    while i < length {
        let generic_err = Error::new(ErrorKind::Other, "Invalid utf8 constant.");

        let byte0 = parse_u8(reader)?;

        if byte0 >> 7 == 0 {
            result.push(char::from_u32(Into::<u32>::into(byte0)).ok_or(generic_err)?);
            i += 1;
            continue;
        }

        let byte1 = parse_u8(reader)?;
        if byte0 >> 5 == 0b110 && byte1 >> 6 == 0b10 {
            result.push(
                char::from_u32(
                    (Into::<u32>::into(byte0 & 0x1f) << 6) + Into::<u32>::into(byte1 & 0x3F),
                )
                .ok_or(generic_err)?,
            );
            i += 2;
            continue;
        }

        let byte2 = parse_u8(reader)?;
        if byte0 >> 4 == 0b1110 && byte1 >> 6 == 0b10 && byte2 >> 6 == 0b10 {
            result.push(
                char::from_u32(
                    (Into::<u32>::into(byte0 & 0xF) << 12)
                        + (Into::<u32>::into(byte1 & 0x3F) << 6)
                        + Into::<u32>::into(byte2 & 0x3F),
                )
                .ok_or(generic_err)?,
            );
            i += 3;
            continue;
        }

        let byte3 = parse_u8(reader)?;
        let byte4 = parse_u8(reader)?;
        let byte5 = parse_u8(reader)?;

        if byte0 == 0b1110_1101
            && byte1 >> 4 == 0b1010
            && byte2 >> 6 == 0b10
            && byte3 == 0b1110_1101
            && byte4 >> 4 == 0b1011
            && byte5 >> 6 == 0b10
        {
            result.push(
                char::from_u32(
                    0x10000
                        + (Into::<u32>::into(byte1 & 0x0f) << 16)
                        + (Into::<u32>::into(byte2 & 0x3f) << 10)
                        + (Into::<u32>::into(byte4 & 0x0f) << 6)
                        + Into::<u32>::into(byte5 & 0x3f),
                )
                .ok_or(generic_err)?,
            );
            i += 6;
            continue;
        }
    }

    result.shrink_to_fit();
    Ok(result)
}

#[derive(Debug)]
pub struct ClassAccessFlags {
    // class declared public
    is_public: bool,
    // class declared final
    is_final: bool,
    // treat superclass methods specially when invoked by invokespecial
    treat_superclass_special: bool,
    // is an interface
    is_interface: bool,
    // is abstract; must not be instantiated
    is_abstract: bool,
    // not present in source code
    is_synthetic: bool,
    // is an annotation interface
    is_annotation: bool,
    // is an enum class
    is_enum: bool,
    // is a module, not a class or interface
    is_module: bool,
}

impl TryFrom<u16> for ClassAccessFlags {
    type Error = Error;

    fn try_from(mask: u16) -> Result<Self, Self::Error> {
        let flags = ClassAccessFlags {
            is_public: mask & 0x0001 != 0,
            is_final: mask & 0x0010 != 0,
            treat_superclass_special: mask & 0x0020 != 0,
            is_interface: mask & 0x0200 != 0,
            is_abstract: mask & 0x0400 != 0,
            is_synthetic: mask & 0x1000 != 0,
            is_annotation: mask & 0x2000 != 0,
            is_enum: mask & 0x4000 != 0,
            is_module: mask & 0x8000 != 0,
        };

        // check that the access flags are valid

        // for an interface, is_abstract is required,
        // and a whole bunch of flags are not allowed
        if flags.is_interface
            && (!flags.is_abstract
                || flags.is_final
                || flags.treat_superclass_special
                || flags.is_enum
                || flags.is_module)
        {
            return make_parse_err("Illegal set of access flags for an interface!");
        } else if !flags.is_interface && flags.is_final && flags.is_abstract {
            return make_parse_err(
                "Illegal set of access flags! Classes cannot be both abstract and final.",
            );
        } else if flags.is_module
            && (flags.is_public
                || flags.is_final
                || flags.treat_superclass_special
                || flags.is_interface
                || flags.is_synthetic
                || flags.is_annotation
                || flags.is_enum)
        {
            return make_parse_err(
                "Illegal set of access flags! Modules may not have any other access flags set.",
            );
        }

        Ok(flags)
    }
}

#[derive(Debug)]
pub struct Field {
    access_flags: FieldAccessFlags,
    name_index: u16,
    descriptor_index: u16,
    attributes: Vec<Attribute>,
}

// See Table 4.5-A
#[derive(Debug)]
pub struct FieldAccessFlags {
    // declared public
    is_public: bool,
    // declared private
    is_private: bool,
    // declared protected
    is_protected: bool,
    // declared static
    is_static: bool,
    // declared final
    is_final: bool,
    // declared volatile (cannot be cached)
    is_volatile: bool,
    // declared transient (not written or read by a persistent object manager)
    is_transient: bool,
    // declared synthetic (not present in source code)
    is_synthetic: bool,
    // declared as an element of an enum class
    from_enum: bool,
}

impl TryFrom<u16> for FieldAccessFlags {
    type Error = Error;

    fn try_from(mask: u16) -> Result<Self, Self::Error> {
        let flags = FieldAccessFlags {
            is_public: mask & 0x0001 != 0,
            is_private: mask & 0x0002 != 0,
            is_protected: mask & 0x0004 != 0,
            is_static: mask & 0x0008 != 0,
            is_final: mask & 0x0010 != 0,
            is_volatile: mask & 0x0040 != 0,
            is_transient: mask & 0x0080 != 0,
            is_synthetic: mask & 0x1000 != 0,
            from_enum: mask & 0x4000 != 0,
        };

        // check that the access flags are valid

        // at most one accessibility modifier can be set at once
        if (flags.is_public as u8 + flags.is_private as u8 + flags.is_protected as u8) > 1 {
            return make_parse_err(
                "Only one accessibility modifier can be set on a field at once!",
            );
        } else if flags.is_final && flags.is_volatile {
            return make_parse_err("Only one of final and volatile can be set for a field!");
        }
        // TODO: cover details around fields of interfaces

        Ok(flags)
    }
}

fn parse_field<T: Read>(reader: &mut T, constant_pool: Vec<Constant>) -> std::io::Result<Field> {
    todo!()
}

// See section 4.7 of the spec
// only the attributes which are:
// "critical to correct interpretation of the class file by the JVM"
#[derive(Debug)]
enum Attribute {
    ConstantValue {
        constant_value_index: u16,
    },
    Code {
        max_stack: u16,
        max_locals: u16,
        code: Vec<u8>,
        exception_table: Vec<ExceptionHandler>,
        attributes: Vec<Attribute>,
    },
    StackMapTable {
        entries: Vec<StackMapFrame>,
    },
    BootstrapMethods {
        methods: Vec<BootstrapMethod>,
    },
    NestHost {
        // constant_pool entry at index must be a CONSTANT_Class_info
        host_class_index: u16,
    },
    NestMembers {
        // constant_pool entries at indices must be CONSTANT_Class_infos
        classes: Vec<u16>,
    },
    PermittedSubclasses {
        // constant_pool entries at indices must be CONSTANT_Class_infos
        classes: Vec<u16>,
    },
}

// See the Code_attribute struct in the spec
#[derive(Debug)]
struct ExceptionHandler {
    // range of code for which the exception handler is active
    actives_pcs: std::ops::Range<u16>,
    // program counter for the handler (must be a valid index for code)
    handler_pc: u16,
    // If not None, an index into the constant_pool for what types this
    // exception handler is valid for
    catch_type: Option<u16>,
}

// See the StackMapTable_attribute struct in the spec
#[derive(Debug)]
enum StackMapFrame {
    // tags: [0-63]
    // this frame has exactly same locals as previous frame,
    // operand stack is empty
    SameFrame {
        frame_type: u8,
    },
    // tags: [64-127]
    // this frame has exactly same locals as previous frame,
    // operand stack has one entry
    SameLocals1StackItemFrame {
        frame_type: u8,
        stack: [VerificationTypeInfo; 1],
    },
    // tags: [128-246] are reserved

    // tag: 247
    // this frame has exactly same locals as previous frame,
    // operand stack has one entry
    SameLocals1StackItemFrameExtended {
        offset_delta: u16,
        frame_type: u8,
        stack: [VerificationTypeInfo; 1],
    },
    // tags: [248-250]
    // this frame has the same local variables as the previous frame,
    // except for the last chopped variables
    ChopFrame {
        offset_delta: u16,
        chopped: u8,
    },
    // tag: 251
    // this frame has the same local variables as the previous frame,
    // operand stack is empty
    SameFrameExtended {
        offset_delta: u16,
    },
    // tags: [252-254]
    // this frame has the same locals as the previous frame,
    // except that [1-3] additional locals are defined,
    // operand stack is empty
    AppendFrame {
        offset_delta: u16,
        additional_locals: Vec<VerificationTypeInfo>,
    },
    // tag: 255
    FullFrame {
        offset_delta: u16,
        locals: Vec<VerificationTypeInfo>,
        stack: Vec<VerificationTypeInfo>,
    },
}

impl StackMapFrame {
    fn offset_delta(&self) -> u16 {
        use StackMapFrame::*;
        match &self {
            SameFrame { frame_type } => (*frame_type).into(),
            SameLocals1StackItemFrame { frame_type, .. } => (frame_type - 64).into(),
            SameLocals1StackItemFrameExtended { offset_delta, .. } => *offset_delta,
            ChopFrame { offset_delta, .. } => *offset_delta,
            SameFrameExtended { offset_delta } => *offset_delta,
            AppendFrame { offset_delta, .. } => *offset_delta,
            FullFrame { offset_delta, .. } => *offset_delta,
        }
    }
}

#[derive(Debug)]
enum VerificationTypeInfo {
    Top,
    Integer,
    Float,
    Null,
    UninitializedThis,
    Object { constant_pool_index: u16 },
    UninitializedVariable { index: u16 },
    Long,
    Double,
}

// See the BootstrapMethods_attribute struct in the spec
#[derive(Debug)]
struct BootstrapMethod {
    method_ref: u16,
    arguments: Rc<[u16]>,
}

fn parse_attribute<T: Read>(reader: &mut T, constant_pool: Vec<Constant>) -> std::io::Result<Option<Attribute>> {
    let name_index = parse_u16(reader)?;
    let name = match constant_pool.get(name_index.into()) {
        Some(Constant::Utf8(str)) => { Ok(str) },
        _ => { make_parse_err("attribute name_index points to non-string!") },
    }?;

    let attribute_length = parse_u32(reader)?;

    Ok(match name.as_ref() {
        "ConstantValue" => { Some(parse_constant_value(reader)?) },
        "Code" => {},
        "StackMapTable" => {},
        "BootstrapMethods" => {},
        "NestHost" => {},
        "NestMembers" => {},
        "PermittedSubclasses" => {},
        _ => {
            // advance the reader attribute_length bytes,
            // so we skip over this attribute
            // TODO: use Seek so this is less inefficient
            reader.take(attribute_length.into());
            None
        }
    })
}

fn parse_constant_value<T: Read>(reader: &mut T) -> std::io::Result<Attribute> {
    Ok(Attribute::ConstantValue {
        constant_value_index: parse_u16(reader)?
    })
}

fn parse_code<T: Read>(reader: &mut T) -> std::io::Result<Attribute> {

}

fn parse_u32<T: Read>(reader: &mut T) -> std::io::Result<u32> {
    let mut buf = [0u8; 4];
    reader.read_exact(&mut buf)?;
    Ok(u32::from_be_bytes(buf))
}

fn parse_u16<T: Read>(reader: &mut T) -> std::io::Result<u16> {
    let mut buf = [0u8; 2];
    reader.read_exact(&mut buf)?;
    Ok(u16::from_be_bytes(buf))
}

// this really should not count as parsing, but name consistency is useful
fn parse_u8<T: Read>(reader: &mut T) -> std::io::Result<u8> {
    let mut buf = [0u8; 1];
    reader.read_exact(&mut buf)?;
    Ok(buf[0])
}

fn make_parse_err<T>(message: &str) -> std::io::Result<T> {
    Err(Error::new(ErrorKind::Other, message))
}
