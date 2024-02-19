use flashfill::{generate_string_program, splits, Eval};

fn main() {
    let train = [
        ["01/21/2001", "01"],
        ["22.02.2004", "02"],
        ["2003-23-03", "03"],
    ];
    let test = train;
    let train_data = splits(&train);
    let timer = std::time::Instant::now();
    let program = generate_string_program(&train_data);
    println!("{} millis", timer.elapsed().as_millis());
    println!("{program}");
    let test_data = splits(&test);
    for (input, expected) in test_data {
        assert_eq!(program.eval(&input).unwrap(), expected);
    }
}
