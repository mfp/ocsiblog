(* Copyright (C) 2009 Mauricio Fernandez <mfp@acm.org> *)
open OUnit

let tests = "All tests" >:::
            [
              Test_simple_markup.tests;
            ]

let () =
  ignore (run_test_tt_main tests)
