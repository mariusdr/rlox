use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoxType {
    String,
    Number,
    Boolean,
    Nil,
}

impl fmt::Display for LoxType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let out = match self {
            LoxType::String => "String",
            LoxType::Number => "Number",
            LoxType::Boolean => "Boolean",
            LoxType::Nil => "Nil",
        };
        write!(f, "{}", out)
    }
}
