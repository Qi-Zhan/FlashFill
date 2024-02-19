use flashfill::{generate_string_program, splits, Eval};

fn main() {
    let train = [
        ["International Business Machines", "IBM"],
        ["Principle Of Programming Languages", "POPL"],
        ["Internaltional Conference on Software Engineering", "ICSE"],
    ];
    let test = [
        ["International Business Machines", "IBM"],
        ["Principle Of Programming Languages", "POPL"],
        ["Internaltional Conference on Software Engineering", "ICSE"],
        ["Operating System Design and Implemenatation", "OSDI"],
    ];
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
