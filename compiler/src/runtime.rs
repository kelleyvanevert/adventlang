use std::borrow::Cow;

use ast::{AlRegex, Type};

/**
 * We follow V8's wisdom: a value is either a pointer to a heap value, or a "small integer" (SMI).
 * The least significant bit determines the former (1) or the latter (0)
 */
#[derive(Clone, Copy, Debug)]
struct AdlaValue(u64);

impl AdlaValue {
    // Constants for tagging
    const SMI_TAG: u64 = 0; // Small integer (least significant bit = 0)
    const POINTER_TAG: u64 = 1; // Pointer (least significant bit = 1)
    const TAG_MASK: u64 = 1; // Mask to extract the tag bit

    // Create a small integer (SMI)
    fn new_smi(value: i32) -> Self {
        // Shift left by 1 to make room for tag bit, then add tag (0)
        let encoded = ((value as i64 as u64) << 1) | Self::SMI_TAG;
        AdlaValue(encoded)
    }

    // Create a pointer value
    fn new_pointer(ptr: *mut ()) -> Self {
        // Ensure pointer is aligned (typically pointers are aligned to at least 4 or 8 bytes)
        assert!((ptr as usize & 1) == 0, "Pointer must be properly aligned");
        // Add tag bit (1) to indicate this is a pointer
        let encoded = (ptr as u64) | Self::POINTER_TAG;
        AdlaValue(encoded)
    }

    // Check if value is a small integer
    fn is_smi(&self) -> bool {
        (self.0 & Self::TAG_MASK) == Self::SMI_TAG
    }

    // Extract the small integer value
    fn get_smi_value(&self) -> Option<i32> {
        if self.is_smi() {
            // Shift right by 1 to remove the tag bit
            Some(((self.0 >> 1) as i64) as i32)
        } else {
            None
        }
    }

    // Extract the pointer
    fn get_pointer(&self) -> Option<*mut ()> {
        if !self.is_smi() {
            // Clear the tag bit to get original pointer
            Some((self.0 & !Self::TAG_MASK) as *mut ())
        } else {
            None
        }
    }
}

/**
 * A heap value is always concretely typed: there's no such thing as, say, an "any" value.
 * However, it can contain inconcrete type specs inside, e.g. a function that receives an otherwise underspecified "fn" or returns an "any"
 */
#[derive(Clone, Debug)]
enum AdlaHeapValue {
    // Any,
    Nil,
    Bool(bool),
    Str(String),
    Int(i64),
    Float(f64),
    Regex(AlRegex),
    Fun {
        params: Vec<Type>,
        ret: Type,

        // these are fully specified at compile-time, for the given function
        closure: *mut (),
        code: *const (),
    },
    List {
        element_ty: Type, // this needs to be specified, because otherwise if the list is empty, it wouldn't be known
        elements: Vec<AdlaValue>,
    },
    Tuple {
        // here, the types of the elements are specified by the elements themselves -- an empty tuple is just an empty tuple
        elements: Vec<AdlaValue>,
    },
    Dict {
        key_ty: Type,
        value_ty: Type,
        data: *mut (), // TODO
    },
}

pub fn str_concat(a: Cow<str>, b: Cow<str>) -> Cow<'static, str> {
    format!("{a}{b}").into()
}

pub fn str_len(a: Cow<str>) -> usize {
    a.len()
}
