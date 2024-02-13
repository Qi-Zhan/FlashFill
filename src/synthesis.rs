use crate::data_structure::*;
use crate::eval::InputState;
use crate::language::*;

fn generate_string_program(s: &[(InputState, String)]) -> StringExpr {
    todo!()
}

fn generate_partitions(s: &[(InputState, String)]) {}

fn generate_bool_classifier(sigma1: &InputState, sigma2: &InputState) {}

fn generate_str(sigma: &InputState, s: &str) {}

fn generate_loop(sigma: &InputState, s: &str) {}

fn generate_substring(sigma: &InputState, s: &str) {}

fn generate_position(s: &str, k: i32) {}

fn generate_regex(r: &RegularExpr, s: &str) -> RegularExpr {
    todo!()
}

// /// Test cases below are taken from the paper examples
// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::example::*;

//     /// Example 1: extract phone number from a string
//     #[test]
//     fn test1() {}

//     /// Example 2: extract the quantity of the purchase
//     #[test]
//     fn test2() {}

//     /// Example 3: extract directory name from a path
//     #[test]
//     fn test3() {}

//     /// Example 4: generate abbreviations of a string
//     #[test]
//     fn test4() {}

//     /// Example 5: split odds
//     #[test]
//     fn test5() {}

//     /// Example 6: remove excess spaces
//     #[test]
//     fn test6() {}

//     /// Example 7: conditional concatenation
//     #[test]
//     fn test7() {}

//     /// Example 8: mixed date parsing
//     #[test]
//     fn test8() {}

//     /// Example 9: name parsing
//     #[test]
//     fn test9() {}

//     /// Example 10: phone number
//     #[test]
//     fn test10() {}

//     /// Example 11
//     #[test]
//     fn test11() {}

//     /// Example 12: Synthesis of part of a futre extension of itself
//     #[test]
//     fn test12() {}

//     /// Example 13: filtering task
//     #[test]
//     fn test13() {}

//     /// Example 14: arithmetic
//     #[test]
//     fn test14() {}
// }
