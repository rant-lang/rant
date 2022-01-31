/*
  Runtime Tests

  These are tests that verify the runtime (+ stdlib) works as expected.
  It is assumed that all test programs in this file compile successfully.

*/

use rant::*;

use assert_matches::*;

#[macro_export]
macro_rules! test_rant_file {
  ($src_path:literal raises $runtime_err_variant:pat) => {{
    use rant::runtime::{RuntimeError, RuntimeErrorType::*};
    let mut r = Rant::with_options(RantOptions {
      debug_mode: true,
      .. Default::default()
    });
    let pgm = r.compile_quiet(include_str!($src_path)).expect("failed to compile program");
    assert_matches!(r.run(&pgm).map(|output| output.to_string()).as_ref().map(|o| o.as_str()), Err(RuntimeError { error_type: $runtime_err_variant, ..}));
  }};
  ($src_path:literal) => {{
    let mut r = Rant::with_options(RantOptions {
      debug_mode: true,
      .. Default::default()
    });
    let pgm = r.compile_quiet(include_str!($src_path)).expect("failed to compile program");
    assert_matches!(r.run(&pgm).map(|output| output.to_string()).as_ref().map(|o| o.as_str()), Ok(_));
  }};
  ($src_path:literal, $expected:literal) => {{
    let mut r = Rant::with_options(RantOptions {
      debug_mode: true,
      .. Default::default()
    });
    let pgm = r.compile_quiet(include_str!($src_path)).expect("failed to compile program");
    assert_matches!(r.run(&pgm).map(|output| output.to_string()).as_ref().map(|o| o.as_str()), Ok($expected));
  }};
}

macro_rules! test_rant {
  ($src:literal, $expected:literal) => {{
    let mut r = Rant::new();
    let pgm = r.compile_quiet($src).expect("failed to compile program");
    assert_matches!(r.run(&pgm).map(|output| output.to_string()).as_ref().map(|o| o.as_str()), Ok($expected));
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
  test_rant!("test ~{test} test", "testtesttest");
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
    "sources/func/func_with_variadic_star.rant",
    "\na\na b\na b c\na b c d"
  );
}

#[test]
fn func_with_variadic_plus() {
  test_rant_file!(
    "sources/func/func_with_variadic_plus.rant",
    "a\na b\na b c\na b c d"
  );
}

#[test]
fn func_with_optional_param() {
  test_rant_file!(
    "sources/func/func_with_optional_param.rant", 
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
    "sources/access/override_shadowed_locals_with_multi_descope.rant",
    "foo bar baz"
  );
}

#[test]
fn multi_accessor_empty_defs() {
  test_rant!(r#"<$foo; $bar> `[type:<foo>] `[type:<bar>]"#, "empty empty");
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
    "sources/access/dynamic_index_setter.rant",
    "1, 2, 4"
  );
}

#[test]
fn dynamic_multi_index_setter() {
  test_rant_file!(
    "sources/access/dynamic_multi_index_setter.rant",
    "1, 2, 4, 4, 5, 6"
  )
}

#[test]
fn closure_capture_var() {
  test_rant_file!("sources/closure/closure_capture_var.rant");
}

#[test]
fn closure_capture_arg() {
  test_rant_file!("sources/closure/closure_capture_arg.rant");
}

#[test]
fn closure_mutate_captured_value() {
  test_rant_file!(
    "sources/closure/closure_mutate_captured_value.rant",
    "0 1 2 3"
  );
}

#[test]
fn filter_with_native_predicate() {
  test_rant_file!(
    "sources/collections/filter_with_native_predicate.rant",
    "1, 3, 5, 7, 9"
  );
}

#[test]
fn filter_with_user_predicate() {
  test_rant_file!(
    "sources/collections/filter_with_user_predicate.rant",
    "1, 3, 5, 7, 9"
  );
}

#[test]
fn map_with_native_callback() {
  test_rant_file!(
    "sources/collections/map_with_native_callback.rant",
    "-1, -2, -3, -4, -5, -6, -7, -8, -9, -10"
  )
}

#[test]
fn map_with_user_callback() {
  test_rant_file!(
    "sources/collections/map_with_user_callback.rant",
    "-1, -2, -3, -4, -5, -6, -7, -8, -9, -10"
  )
}

#[test]
fn zip_with_native_callback() {
  test_rant_file!(
    "sources/collections/zip_with_native_callback.rant",
    "5, 7, 9"
  );
}

#[test]
fn zip_with_user_callback() {
  test_rant_file!(
    "sources/collections/zip_with_user_callback.rant",
    "5, 7, 9"
  );
}

#[test]
fn func_percolation() {
  test_rant_file!(
    "sources/func/func_percolation.rant",
    "global\nlocal\nvery local"
  );
}

#[test]
fn anon_getter() {
  test_rant_file!(
    "sources/anonymous/anon_getter.rant",
    "foobar"
  );
}

#[test]
fn dynamic_anon_getter() {
  test_rant_file!(
    "sources/anonymous/dynamic_anon_getter.rant",
    "6"
  );
}

#[test]
fn anon_setter() {
  test_rant_file!(
    "sources/anonymous/anon_setter.rant",
    "bazqux"
  );
}

#[test]
fn dynamic_anon_setter() {
  test_rant_file!(
    "sources/anonymous/dynamic_anon_setter.rant",
    "7"
  );
}

#[test]
fn inv_index_get() {
  test_rant_file!(
    "sources/access/inv_index_get.rant",
    "3, 2, 1"
  );
}

#[test]
fn inv_index_set() {
  test_rant_file!(
    "sources/access/inv_index_set.rant",
    "4, 5, 6"
  );
}

#[test]
fn function_piping() {
  test_rant_file!(
    "sources/func/function_piping.rant",
    "the fox the dog"
  );
}

#[test]
fn function_piping_callback() {
  test_rant_file!(
    "sources/func/function_piping_callback.rant",
    "foobar"
  )
}

#[test]
fn func_assignment_pipe_set() {
  test_rant_file!("sources/func/assignment_pipe_set.rant");
}

#[test]
fn func_assignment_pipe_def_var() {
  test_rant_file!("sources/func/assignment_pipe_def_var.rant");
}

#[test]
fn func_assignment_pipe_def_const() {
  test_rant_file!("sources/func/assignment_pipe_def_const.rant");
}

#[test]
fn func_pipecall_pipeval() {
  test_rant_file!("sources/func/pipecall_pipeval.rant");
}

#[test]
fn getter_fallback_from_var() {
  test_rant_file!(
    "sources/access/getter_fallback_from_var.rant",
    "123, fallback"
  )
}

#[test]
fn getter_fallback_from_index() {
  test_rant_file!(
    "sources/access/getter_fallback_from_index.rant",
    "foo, bar, baz, oops"
  )
}

#[test]
fn getter_fallback_from_key() {
  test_rant_file!(
    "sources/access/getter_fallback_from_key.rant",
    "foo, bar, baz, oops"
  )
}

#[test]
fn charms_top_level_return() {
  test_rant_file!("sources/charms/top_level_return.rant");
}

#[test]
fn charms_func_return_output() {
  test_rant_file!("sources/charms/func_return_output.rant");
}

#[test]
fn charms_func_return_value() {
  test_rant_file!("sources/charms/func_return_value.rant");
}

#[test]
fn charms_rep_continue_output() {
  test_rant_file!("sources/charms/rep_continue_output.rant");
}

#[test]
fn charms_rep_continue_value() {
  test_rant_file!("sources/charms/rep_continue_value.rant");
}

#[test]
fn charms_rep_break_output() {
  test_rant_file!("sources/charms/rep_break_output.rant");
}

#[test]
fn charms_rep_break_value() {
  test_rant_file!("sources/charms/rep_break_value.rant");
}

#[test]
fn charms_weight_all_zero() {
  test_rant_file!("sources/charms/weight_all_zero.rant");
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
fn slice_list_full() {
  test_rant_file!("sources/slice/list/full.rant");
}

#[test]
fn slice_list_between_static() {
  test_rant_file!("sources/slice/list/between_static.rant");
}

#[test]
fn slice_list_from_static() {
  test_rant_file!("sources/slice/list/from_static.rant");
}

#[test]
fn slice_list_to_static() {
  test_rant_file!("sources/slice/list/to_static.rant");
}

#[test]
fn slice_list_between_dynamic() {
  test_rant_file!("sources/slice/list/between_dynamic.rant");
}

#[test]
fn slice_list_from_dynamic() {
  test_rant_file!("sources/slice/list/from_dynamic.rant");
}

#[test]
fn slice_list_to_dynamic() {
  test_rant_file!("sources/slice/list/to_dynamic.rant");
}

#[test]
fn slice_tuple_full() {
  test_rant_file!("sources/slice/tuple/full.rant");
}

#[test]
fn slice_tuple_between_static() {
  test_rant_file!("sources/slice/tuple/between_static.rant");
}

#[test]
fn slice_tuple_from_static() {
  test_rant_file!("sources/slice/tuple/from_static.rant");
}

#[test]
fn slice_tuple_to_static() {
  test_rant_file!("sources/slice/tuple/to_static.rant");
}

#[test]
fn slice_tuple_between_dynamic() {
  test_rant_file!("sources/slice/tuple/between_dynamic.rant");
}

#[test]
fn slice_tuple_from_dynamic() {
  test_rant_file!("sources/slice/tuple/from_dynamic.rant");
}

#[test]
fn slice_tuple_to_dynamic() {
  test_rant_file!("sources/slice/tuple/to_dynamic.rant");
}

#[test]
fn slice_string_full() {
  test_rant_file!("sources/slice/string/full.rant");
}

#[test]
fn slice_string_between_static() {
  test_rant_file!("sources/slice/string/between_static.rant");
}

#[test]
fn slice_string_from_static() {
  test_rant_file!("sources/slice/string/from_static.rant");
}

#[test]
fn slice_string_to_static() {
  test_rant_file!("sources/slice/string/to_static.rant");
}

#[test]
fn slice_string_between_dynamic() {
  test_rant_file!("sources/slice/string/between_dynamic.rant");
}

#[test]
fn slice_string_from_dynamic() {
  test_rant_file!("sources/slice/string/from_dynamic.rant");
}

#[test]
fn slice_string_to_dynamic() {
  test_rant_file!("sources/slice/string/to_dynamic.rant");
}

#[test]
fn slice_range_full() {
  test_rant_file!("sources/slice/range/full.rant");
}

#[test]
fn slice_range_between_static() {
  test_rant_file!("sources/slice/range/between_static.rant");
}

#[test]
fn slice_range_from_static() {
  test_rant_file!("sources/slice/range/from_static.rant");
}

#[test]
fn slice_range_to_static() {
  test_rant_file!("sources/slice/range/to_static.rant");
}

#[test]
fn slice_range_between_dynamic() {
  test_rant_file!("sources/slice/range/between_dynamic.rant");
}

#[test]
fn slice_range_from_dynamic() {
  test_rant_file!("sources/slice/range/from_dynamic.rant");
}

#[test]
fn slice_range_to_dynamic() {
  test_rant_file!("sources/slice/range/to_dynamic.rant");
}

#[test]
fn splice_static_from_tuple() {
  test_rant_file!("sources/splice/static_from_tuple.rant");
}

#[test]
fn splice_dynamic_from_tuple() {
  test_rant_file!("sources/splice/dynamic_from_tuple.rant");
}

#[test]
fn splice_static_from_list() {
  test_rant_file!("sources/splice/static_from_list.rant");
}

#[test]
fn splice_dynamic_from_list() {
  test_rant_file!("sources/splice/dynamic_from_list.rant");
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
fn redef_var_with_const() {
  test_rant_file!("sources/const/redef_var_with_const.rant");
}

#[test]
fn const_shadow() {
  test_rant_file!("sources/const/const_shadow.rant");
}

#[test]
fn list_autoconcat() {
  test_rant_file!("sources/collections/list_autoconcat.rant");
}

#[test]
fn list_autoconcat_repeater() {
  test_rant_file!("sources/collections/list_autoconcat_repeater.rant");
}

#[test]
fn tuple_autoconcat() {
  test_rant_file!("sources/collections/tuple_autoconcat.rant");
}

#[test]
fn tuple_autoconcat_repeater() {
  test_rant_file!("sources/collections/tuple_autoconcat_repeater.rant");
}

#[test]
fn list_tuple_autoconcat() {
  test_rant_file!("sources/collections/list_tuple_autoconcat.rant");
}

#[test]
fn map_autoconcat() {
  test_rant_file!("sources/collections/map_autoconcat.rant");
}

#[test]
fn temporal_one() {
  test_rant_file!("sources/temporal/temporal_one.rant");
}

#[test]
fn temporal_one_mixed() {
  test_rant_file!("sources/temporal/temporal_one_mixed.rant");
}

#[test]
fn temporal_two_samesize() {
  test_rant_file!("sources/temporal/temporal_two_samesize.rant");
}

#[test]
fn temporal_two_samesize_mixed() {
  test_rant_file!("sources/temporal/temporal_two_samesize_mixed.rant");
}

#[test]
fn temporal_two_samesize_sync() {
  test_rant_file!("sources/temporal/temporal_two_samesize_sync.rant");
}

#[test]
fn temporal_two_diffsize() {
  test_rant_file!("sources/temporal/temporal_two_diffsize.rant");
}

#[test]
fn temporal_two_diffsize_mixed() {
  test_rant_file!("sources/temporal/temporal_two_diffsize_mixed.rant");
}

#[test]
fn temporal_pipe_temporal() {
  test_rant_file!("sources/temporal/temporal_pipe_temporal.rant");
}

#[test]
fn range_forward() {
  test_rant_file!("sources/range/range_forward.rant");
}

#[test]
fn range_reverse() {
  test_rant_file!("sources/range/range_reverse.rant");
}

#[test]
fn range_forward_step_divisible() {
  test_rant_file!("sources/range/range_forward_step_divisible.rant");
}

#[test]
fn range_reverse_step_divisible() {
  test_rant_file!("sources/range/range_reverse_step_divisible.rant");
}

#[test]
fn range_forward_step_indivisible() {
  test_rant_file!("sources/range/range_forward_step_indivisible.rant");
}

#[test]
fn range_reverse_step_indivisible() {
  test_rant_file!("sources/range/range_reverse_step_indivisible.rant");
}

#[test]
fn branch_if() {
  test_rant_file!("sources/branch/if.rant")
}

#[test]
fn branch_if_else() {
  test_rant_file!("sources/branch/if-else.rant")
}

#[test]
fn branch_if_elseif() {
  test_rant_file!("sources/branch/if-elseif.rant")
}

#[test]
fn branch_if_elseif_else() {
  test_rant_file!("sources/branch/if-elseif-else.rant")
}

#[test]
fn ops_and() {
  test_rant_file!("sources/ops/and.rant")
}

#[test]
fn ops_and_short_circuit() {
  test_rant_file!("sources/ops/and_short_circuit.rant")
}

#[test]
fn ops_cmp() {
  test_rant_file!("sources/ops/cmp.rant")
}

#[test]
fn ops_math() {
  test_rant_file!("sources/ops/math.rant")
}

#[test]
fn ops_nand() {
  test_rant_file!("sources/ops/nand.rant")
}

#[test]
fn ops_nand_short_circuit() {
  test_rant_file!("sources/ops/nand_short_circuit.rant")
}

#[test]
fn ops_nor() {
  test_rant_file!("sources/ops/nor.rant")
}

#[test]
fn ops_nor_short_circuit() {
  test_rant_file!("sources/ops/nor_short_circuit.rant")
}

#[test]
fn ops_not() {
  test_rant_file!("sources/ops/not.rant")
}

#[test]
fn ops_or() {
  test_rant_file!("sources/ops/or.rant")
}

#[test]
fn ops_or_short_circuit() {
  test_rant_file!("sources/ops/or_short_circuit.rant")
}

#[test]
fn ops_xor() {
  test_rant_file!("sources/ops/xor.rant")
}