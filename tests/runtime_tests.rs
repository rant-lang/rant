/*
  Runtime Tests

  These are tests that verify the runtime (+ stdlib) works as expected.
  It is assumed that all test programs in this file compile successfully.

*/

use rant::*;

use assert_matches::*;

macro_rules! test_rant_file {
  ($src_path:literal raises $runtime_err_variant:pat) => {{
    use rant::runtime::{RuntimeError, RuntimeErrorType::*};
    let mut r = Rant::with_options(RantOptions {
      debug_mode: true,
      .. Default::default()
    });
    let pgm = r.compile_quiet(include_str!($src_path)).expect("failed to compile program");
    assert_matches!(r.run_into_string(&pgm).as_ref().map(|o| o.as_str()), Err(RuntimeError { error_type: $runtime_err_variant, ..}));
  }};
  ($src_path:literal) => {{
    let mut r = Rant::with_options(RantOptions {
      debug_mode: true,
      .. Default::default()
    });
    let pgm = r.compile_quiet(include_str!($src_path)).expect("failed to compile program");
    assert_matches!(r.run_into_string(&pgm).as_ref().map(|o| o.as_str()), Ok(_));
  }};
  ($src_path:literal, $expected:literal) => {{
    let mut r = Rant::with_options(RantOptions {
      debug_mode: true,
      .. Default::default()
    });
    let pgm = r.compile_quiet(include_str!($src_path)).expect("failed to compile program");
    assert_matches!(r.run_into_string(&pgm).as_ref().map(|o| o.as_str()), Ok($expected));
  }};
}

macro_rules! test_rant {
  ($src:literal, $expected:literal) => {{
    let mut r = Rant::new();
    let pgm = r.compile_quiet($src).expect("failed to compile program");
    assert_matches!(r.run_into_string(&pgm).as_ref().map(|o| o.as_str()), Ok($expected));
  }}
}

#[test]
fn empty_program() {
  test_rant!("", "");
}

#[test]
fn single_fragment() {
  test_rant!("foo", "foo");
}

#[test]
fn spaced_fragments() {
  test_rant!("foo bar", "foo bar");
}

#[test]
fn sinked_block() {
  test_rant!("_{test}", "");
}

#[test]
fn single_element_block() {
  test_rant!("{test}", "test");
}

#[test]
fn repeater() {
  test_rant!("[rep:10]{a}", "aaaaaaaaaa");
}

#[test]
fn repeater_with_value_sep() {
  test_rant!(r#"[rep:10][sep:\s]{a}"#, "a a a a a a a a a a");
}

#[test]
fn repeater_with_closure_sep() {
  test_rant!(r#"[rep:10][sep:[?]{b}]{a}"#, "abababababababababa");
}

#[test]
fn selector_forward() {
  test_rant!(r#"[rep:16][sel:[mksel:forward]]{a|b|c|d|e|f|g|h}"#, "abcdefghabcdefgh");
}

#[test]
fn selector_reverse() {
  test_rant!(r#"[rep:16][sel:[mksel:reverse]]{a|b|c|d|e|f|g|h}"#, "hgfedcbahgfedcba");
}

#[test]
fn selector_ping() {
  test_rant!(r#"[rep:16][sel:[mksel:ping]]{a|b|c|d|e|f|g|h}"#, "abcdefghgfedcbab");
}

#[test]
fn selector_pong() {
  test_rant!(r#"[rep:16][sel:[mksel:pong]]{a|b|c|d|e|f|g|h}"#, "hgfedcbabcdefghg");
}

#[test]
fn func_no_params() {
  test_rant!(r#"[$example-func]{test}[example-func]"#, "test");
}

#[test]
fn func_with_required_param() {
  test_rant!(r#"[$square:x]{[mul:<x>;<x>]} [square:3]"#, "9");
}

#[test]
fn func_with_variadic_star() {
  test_rant_file!(
    "sources/func_with_variadic_star.rant",
    "\na\na b\na b c\na b c d"
  );
}

#[test]
fn func_with_variadic_plus() {
  test_rant_file!(
    "sources/func_with_variadic_plus.rant",
    "a\na b\na b c\na b c d"
  );
}

#[test]
fn func_with_optional_param() {
  test_rant_file!(
    "sources/func_with_optional_param.rant", 
    "foo\nbar"
  );
}

#[test]
fn shadowed_global() {
  test_rant!(r#"<$/test=foo><$test=bar><test>"#, "bar")
}

#[test]
fn shadowed_local() {
  test_rant!(r#"<$test=foo>{<$test=bar><test>}"#, "bar")
}

#[test]
fn override_shadowed_global_with_explicit_global() {
  test_rant!(r#"<$/example=foo><$example=bar></example>"#, "foo");
}

#[test]
fn override_shadowed_global_with_descope() {
  test_rant!(r#"<$/example=foo><$example=bar><^example>"#, "foo");
}

#[test]
fn override_shadowed_local_with_descope() {
  test_rant!(r#"<$test=foo>{<$test=bar><^test>}"#, "foo")
}

#[test]
fn override_shadowed_locals_with_multi_descope() {
  test_rant_file!(
    "sources/override_shadowed_locals_with_multi_descope.rant",
    "foo bar baz"
  );
}

#[test]
fn multi_accessor_empty_defs() {
  test_rant!(r#"<$foo; $bar> '[type:<foo>] '[type:<bar>]"#, "empty empty");
}

#[test]
fn multi_accessor_defs() {
  test_rant!(r#"<$foo=8; $bar=2; $baz=[sub:<foo>;<bar>]; baz>"#, "6");
}

#[test]
fn multi_accessor_reassign() {
  test_rant!(r#"<$foo=bar; foo=baz; foo>"#, "baz");
}

#[test]
fn multi_accessor_delim_term() {
  test_rant!(r#"<$foo=8; $bar=2; $baz=[add:<foo>;<bar>]; baz;>"#, "10");
}

#[test]
fn dynamic_index_setter() {
  test_rant_file!(
    "sources/dynamic_index_setter.rant",
    "1, 2, 4"
  );
}

#[test]
fn dynamic_multi_index_setter() {
  test_rant_file!(
    "sources/dynamic_multi_index_setter.rant",
    "1, 2, 4, 4, 5, 6"
  )
}

#[test]
fn closure_capture_var() {
  test_rant_file!(
    "sources/closure_capture_var.rant",
    "foo"
  );
}

#[test]
fn closure_capture_arg() {
  test_rant_file!(
    "sources/closure_capture_arg.rant",
    "foo"
  );
}

#[test]
fn closure_mutate_captured_value() {
  test_rant_file!(
    "sources/closure_mutate_captured_value.rant",
    "0 1 2 3"
  );
}

#[test]
fn filter_with_native_predicate() {
  test_rant_file!(
    "sources/filter_with_native_predicate.rant",
    "1, 3, 5, 7, 9"
  );
}

#[test]
fn filter_with_user_predicate() {
  test_rant_file!(
    "sources/filter_with_user_predicate.rant",
    "1, 3, 5, 7, 9"
  );
}

#[test]
fn map_with_native_callback() {
  test_rant_file!(
    "sources/map_with_native_callback.rant",
    "-1, -2, -3, -4, -5, -6, -7, -8, -9, -10"
  )
}

#[test]
fn map_with_user_callback() {
  test_rant_file!(
    "sources/map_with_user_callback.rant",
    "-1, -2, -3, -4, -5, -6, -7, -8, -9, -10"
  )
}

#[test]
fn zip_with_native_callback() {
  test_rant_file!(
    "sources/zip_with_native_callback.rant",
    "5, 7, 9"
  );
}

#[test]
fn zip_with_user_callback() {
  test_rant_file!(
    "sources/zip_with_user_callback.rant",
    "5, 7, 9"
  );
}

#[test]
fn trickle_down_func_lookup() {
  test_rant_file!(
    "sources/trickle_down_func_lookup.rant",
    "global\nlocal\nvery local"
  );
}

#[test]
fn anon_getter() {
  test_rant_file!(
    "sources/anon_getter.rant",
    "foo bar"
  );
}

#[test]
fn dynamic_anon_getter() {
  test_rant_file!(
    "sources/dynamic_anon_getter.rant",
    "6"
  );
}

#[test]
fn anon_setter() {
  test_rant_file!(
    "sources/anon_setter.rant",
    "baz qux"
  );
}

#[test]
fn dynamic_anon_setter() {
  test_rant_file!(
    "sources/dynamic_anon_setter.rant",
    "7"
  );
}

#[test]
fn inv_index_get() {
  test_rant_file!(
    "sources/inv_index_get.rant",
    "3, 2, 1"
  );
}

#[test]
fn inv_index_set() {
  test_rant_file!(
    "sources/inv_index_set.rant",
    "4, 5, 6"
  );
}

#[test]
fn function_composition() {
  test_rant_file!(
    "sources/function_composition.rant",
    "the fox the dog"
  );
}

#[test]
fn function_composition_callback() {
  test_rant_file!(
    "sources/function_composition_callback.rant",
    "foo bar"
  )
}

#[test]
fn getter_fallback_from_var() {
  test_rant_file!(
    "sources/getter_fallback_from_var.rant",
    "123, fallback"
  )
}

#[test]
fn getter_fallback_from_index() {
  test_rant_file!(
    "sources/getter_fallback_from_index.rant",
    "foo, bar, baz, oops"
  )
}

#[test]
fn getter_fallback_from_key() {
  test_rant_file!(
    "sources/getter_fallback_from_key.rant",
    "foo, bar, baz, oops"
  )
}

#[test]
fn top_level_return() {
  test_rant_file!("sources/top_level_return.rant");
}

#[test]
fn assert_pass() {
  test_rant_file!("sources/assert/assert_pass.rant");
}

#[test]
fn assert_fail() {
  test_rant_file!("sources/assert/assert_fail.rant" raises AssertError);
}

#[test]
fn math_min() {
  test_rant_file!("sources/math/min.rant");
}

#[test]
fn math_max() {
  test_rant_file!("sources/math/max.rant");
}

#[test]
fn slice_full() {
  test_rant_file!("sources/slice/full.rant");
}

#[test]
fn slice_between_static() {
  test_rant_file!("sources/slice/between_static.rant");
}

#[test]
fn slice_from_static() {
  test_rant_file!("sources/slice/from_static.rant");
}

#[test]
fn slice_to_static() {
  test_rant_file!("sources/slice/to_static.rant");
}

#[test]
fn slice_between_dynamic() {
  test_rant_file!("sources/slice/between_dynamic.rant");
}

#[test]
fn slice_from_dynamic() {
  test_rant_file!("sources/slice/from_dynamic.rant");
}

#[test]
fn slice_to_dynamic() {
  test_rant_file!("sources/slice/to_dynamic.rant");
}

#[test]
fn splice_static() {
  test_rant_file!("sources/splice/static.rant");
}

#[test]
fn splice_dynamic() {
  test_rant_file!("sources/splice/dynamic.rant");
}

#[test]
fn modules_require() {
  test_rant_file!("sources/modules/require.rant");
}

#[test]
fn spread_all() {
  test_rant_file!("sources/spread/spread_all.rant");
}

#[test]
fn spread_inner() {
  test_rant_file!("sources/spread/spread_inner.rant");
}

#[test]
fn spread_left() {
  test_rant_file!("sources/spread/spread_left.rant");
}

#[test]
fn spread_right() {
  test_rant_file!("sources/spread/spread_right.rant");
}

#[test]
fn spread_multi() {
  test_rant_file!("sources/spread/spread_multi.rant");
}

#[test]
fn spread_variadic_star() {
  test_rant_file!("sources/spread/spread_variadic_star.rant");
}

#[test]
fn spread_variadic_plus() {
  test_rant_file!("sources/spread/spread_variadic_plus.rant");
}

#[test]
fn const_define() {
  test_rant_file!("sources/const/const_define.rant");
}

#[test]
fn const_function() {
  test_rant_file!("sources/const/const_function.rant");
}

#[test]
fn const_redef() {
  test_rant_file!("sources/const/const_redef.rant");
}

#[test]
fn const_shadow() {
  test_rant_file!("sources/const/const_shadow.rant");
}