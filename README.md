# Flash Fill

My implementation of the Flash Fill algorithm from the paper [Automating String Processing in Spreadsheets using Input-Output Examples](https://www.microsoft.com/en-us/research/publication/automating-string-processing-spreadsheets-using-input-output-examples/) by Sumit Gulwani.

## Structure

```text
src/
├── eval.rs        # Semantic of the language
├── examples.rs    # Examples copied from the paper
├── language.rs    # Syntax of the language
├── data_struct.rs # Data Structure from the paper
├── lib.rs
└── synthesis.rs   # Synthesis algorithm from the paper
```

## Tests

tests are located in `src/eval.rs` and `src/synthesis` can be run with `cargo test`.
