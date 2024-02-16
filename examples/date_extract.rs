use flashfill::{generate_string_program, splits, Eval};

fn main() {
    let train = [
        ["01/21/2001", "01"],
        ["22.02.2002", "02"],
        ["2003-23-03", "03"],
    ];
    let test = [
        ["00/21/2001", "00"],
        ["22.11.2002", "11"],
        ["2003-23-05", "05"],
    ];
    let train_data = splits(&train);
    let program = generate_string_program(&train_data);
    println!("{program}");
    let test_data = splits(&test);
    for (input, expected) in test_data {
        assert_eq!(program.eval(&input).unwrap(), expected);
    }
}
