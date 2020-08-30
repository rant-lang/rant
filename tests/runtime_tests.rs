/*
  Runtime Tests

  These are tests that verify the runtime (+ stdlib) works as expected.
  It is assumed that all test programs in this file compile successfully.

*/

use rant::Rant;
use assert_matches::*;

macro_rules! run_rant {
  ($src:literal) => {{
    let mut r = Rant::new();
    let pgm = r.compile_quiet($src).expect("compile failed");
    r.run_into_string(&pgm).as_ref().map(|o| o.as_str())
  }}
}

#[test]
fn empty_program() {
  assert_matches!(run_rant!(""), Ok(""));
}

#[test]
fn single_fragment() {
  assert_matches!(run_rant!("foo"), Ok("foo"));
}

#[test]
fn spaced_fragments() {
  assert_matches!(run_rant!("foo bar"), Ok("foo bar"));
}

#[test]
fn sinked_block() {
  assert_matches!(run_rant!("_{test}"), Ok(""));
}

#[test]
fn single_element_block() {
  assert_matches!(run_rant!("{test}"), Ok("test"))
}

#[test]
fn repeater() {
  assert_matches!(run_rant!("[rep:10]{a}"), Ok("aaaaaaaaaa"));
}

#[test]
fn repeater_with_value_sep() {
  assert_matches!(run_rant!(r#"[rep:10][sep:\s]{a}"#), Ok("a a a a a a a a a a"));
}

#[test]
fn repeater_with_closure_sep() {
  assert_matches!(run_rant!(r#"[rep:10][sep:[?]{b}]{a}"#), Ok("abababababababababa"));
}

#[test]
fn selector_forward() {
  assert_matches!(run_rant!(r#"[rep:16][sel:[mksel:forward]]{a|b|c|d|e|f|g|h}"#), Ok("abcdefghabcdefgh"));
}

#[test]
fn selector_reverse() {
  assert_matches!(run_rant!(r#"[rep:16][sel:[mksel:reverse]]{a|b|c|d|e|f|g|h}"#), Ok("hgfedcbahgfedcba"));
}

#[test]
fn selector_ping() {
  assert_matches!(run_rant!(r#"[rep:16][sel:[mksel:ping]]{a|b|c|d|e|f|g|h}"#), Ok("abcdefghgfedcbab"));
}

#[test]
fn selector_pong() {
  assert_matches!(run_rant!(r#"[rep:16][sel:[mksel:pong]]{a|b|c|d|e|f|g|h}"#), Ok("hgfedcbabcdefghg"));
}

#[test]
fn func_no_params() {
  assert_matches!(run_rant!(r#"[$example-func]{test}[example-func]"#), Ok("test"));
}

#[test]
fn func_with_required_param() {
  assert_matches!(run_rant!(r#"[$square:x]{[mul:<x>;<x>]} [square:3]"#), Ok("9"));
}

#[test]
fn func_with_variadic_star() {
  assert_matches!(run_rant!(r#"[$list-args: args*]{[rep:[len:<args>]][sep:\s]{<args/{[step-index]}>}} [list-args]\n[list-args:a]\n[list-args:a;b;c;d]"#), Ok("\na\na b c d"));
}

#[test]
fn func_with_variadic_plus() {
  assert_matches!(run_rant!(r#"[$list-args: args+]{[rep:[len:<args>]][sep:\s]{<args/{[step-index]}>}} [list-args:a]\n[list-args:a;b;c;d]"#), Ok("a\na b c d"));
}

#[test]
fn func_with_optional_param() {
  assert_matches!(run_rant!(r#"[$arg-or-foo:arg?]{[alt:<arg>;foo]} [arg-or-foo]\n[arg-or-foo:bar]"#), Ok("foo\nbar"));
}