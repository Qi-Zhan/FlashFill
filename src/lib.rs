mod data_structure;
mod eval;
#[cfg(test)]
mod examples;
mod language;
mod synthesis;
mod util;

pub use eval::Eval;
pub use synthesis::generate_string_program;
pub use util::splits;
