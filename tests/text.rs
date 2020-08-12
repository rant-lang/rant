use rant::Rant;


macro_rules! run_rant {
  ($src:expr) => {{
    let mut r = Rant::new();
    let pgm = r.compile_quiet(None, $src).expect("compile failed");
    r.run(&pgm).expect("runtime error")
  }}
}

#[test]
fn empty_program() {
  assert_eq!("", run_rant!(""));
}

#[test]
fn single_fragment() {
  assert_eq!("foo", run_rant!("foo"));
}

#[test]
fn spaced_fragments() {
  assert_eq!("foo bar", run_rant!("foo bar"));
}