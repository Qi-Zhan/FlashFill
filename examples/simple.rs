use flashfill::{generate_string_program, splits, Eval};

fn main() {
    let train = [
        ["BTR KRNL WK CORN 15Z", "15Z"],
        ["CAMP DRY DBL NDL 3.6 OZ", "3.6 OZ"],
        ["CHORE BOY HD SC SPNG 1 PK", "1 PK"],
        ["FRENCH WORCESTERSHIRE 5 Z", "5 Z"],
        ["O F TOMATO PASTE 6 OZ", "6 OZ"],
    ];
    let test = train;
    let train_data = splits(&train);
    let timer = std::time::Instant::now();
    let program = generate_string_program(&train_data);
    println!("{program}");
    println!("{} millis", timer.elapsed().as_millis());
    let test_data = splits(&test);
    for (input, expected) in test_data {
        assert_eq!(program.eval(&input).unwrap(), expected);
    }
}
