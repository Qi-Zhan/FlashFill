/// holds values for m string variables v1, ..., vm
///
/// denoting the multiple input columns in a spreadsheet
pub type InputState = Vec<String>;

pub fn split(example: &[&str]) -> (InputState, String) {
    let (input, expected) = example.split_at(example.len() - 1);
    let input = input.iter().map(|f| f.to_string()).collect();
    (input, expected[0].to_string())
}

pub fn splits<const W: usize, const H: usize>(
    examples: &[[&str; W]; H],
) -> Vec<(InputState, String)> {
    examples.iter().map(|f| split(f)).collect()
}
