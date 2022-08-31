use toyscript::compile_and_run;

#[test]
fn fib_test() {
    assert_eq!(compile_and_run(include_str!("simple.toy")).unwrap(), 1);
}