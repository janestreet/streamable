open! Core
open! Import

(* demonstrate grouping logic for how large variants are nested into smaller variants with
   arities no greater than 4 *)

let test ?(show_mapping = false) n =
  let alist = List.init n ~f:Fn.id in
  let repr = Nested_variant.For_testing.create alist in
  print_s [%message (repr : int Nested_variant.For_testing.t)];
  if show_mapping
  then (
    let mappings = Nested_variant.For_testing.paths repr in
    print_s [%message (mappings : (Nested_variant.For_testing.Path.t * int) list)])
;;

let%expect_test "nested-variant-create" =
  require_does_raise ~hide_positions:true (fun () -> test 0 ~show_mapping:true);
  [%expect
    {|
    (ppx/ppx_streamable/src/nested_variant.ml:LINE:COL
     "Error: [nested_variant] of empty constructor list")
    |}];
  test 1 ~show_mapping:true;
  [%expect
    {|
    (repr 0)
    (mappings (("" 0)))
    |}];
  test 2 ~show_mapping:true;
  [%expect
    {|
    (repr (0 1))
    (mappings ((A 0) (B 1)))
    |}];
  test 3 ~show_mapping:true;
  [%expect
    {|
    (repr (0 1 2))
    (mappings ((A 0) (B 1) (C 2)))
    |}];
  test 4 ~show_mapping:true;
  [%expect
    {|
    (repr (0 1 2 3))
    (mappings ((A 0) (B 1) (C 2) (D 3)))
    |}];
  test 5 ~show_mapping:true;
  [%expect
    {|
    (repr ((0 1) (2 3) 4))
    (mappings ((AA 0) (AB 1) (BA 2) (BB 3) (C 4)))
    |}];
  test 6 ~show_mapping:true;
  [%expect
    {|
    (repr ((0 1) (2 3) (4 5)))
    (mappings ((AA 0) (AB 1) (BA 2) (BB 3) (CA 4) (CB 5)))
    |}];
  test 7 ~show_mapping:true;
  [%expect
    {|
    (repr ((0 1) (2 3) (4 5) 6))
    (mappings ((AA 0) (AB 1) (BA 2) (BB 3) (CA 4) (CB 5) (D 6)))
    |}];
  test 8 ~show_mapping:true;
  [%expect
    {|
    (repr ((0 1) (2 3) (4 5) (6 7)))
    (mappings ((AA 0) (AB 1) (BA 2) (BB 3) (CA 4) (CB 5) (DA 6) (DB 7)))
    |}];
  test 9 ~show_mapping:true;
  [%expect
    {|
    (repr ((0 1 2) (3 4 5) (6 7 8)))
    (mappings ((AA 0) (AB 1) (AC 2) (BA 3) (BB 4) (BC 5) (CA 6) (CB 7) (CC 8)))
    |}];
  test 10;
  [%expect {| (repr ((0 1 2) (3 4 5) (6 7 8) 9)) |}];
  test 11;
  [%expect {| (repr ((0 1 2) (3 4 5) (6 7 8) (9 10))) |}];
  test 12;
  [%expect {| (repr ((0 1 2) (3 4 5) (6 7 8) (9 10 11))) |}];
  test 13;
  [%expect {| (repr ((0 1 2 3) (4 5 6 7) (8 9 10 11) 12)) |}];
  test 14;
  [%expect {| (repr ((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13))) |}];
  test 15;
  [%expect {| (repr ((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14))) |}];
  test 16;
  [%expect {| (repr ((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15))) |}];
  test 17 ~show_mapping:true;
  [%expect
    {|
    (repr (((0 1) (2 3) 4) ((5 6) (7 8) 9) ((10 11) (12 13) 14) (15 16)))
    (mappings
     ((AAA 0) (AAB 1) (ABA 2) (ABB 3) (AC 4) (BAA 5) (BAB 6) (BBA 7) (BBB 8)
      (BC 9) (CAA 10) (CAB 11) (CBA 12) (CBB 13) (CC 14) (DA 15) (DB 16)))
    |}];
  test 18;
  [%expect {| (repr (((0 1) (2 3) 4) ((5 6) (7 8) 9) ((10 11) (12 13) 14) (15 16 17))) |}];
  test 19;
  [%expect
    {| (repr (((0 1) (2 3) 4) ((5 6) (7 8) 9) ((10 11) (12 13) 14) (15 16 17 18))) |}];
  test 20 ~show_mapping:true;
  [%expect
    {|
    (repr
     (((0 1) (2 3) 4) ((5 6) (7 8) 9) ((10 11) (12 13) 14) ((15 16) (17 18) 19)))
    (mappings
     ((AAA 0) (AAB 1) (ABA 2) (ABB 3) (AC 4) (BAA 5) (BAB 6) (BBA 7) (BBB 8)
      (BC 9) (CAA 10) (CAB 11) (CBA 12) (CBB 13) (CC 14) (DAA 15) (DAB 16)
      (DBA 17) (DBB 18) (DC 19)))
    |}];
  test (4 * 4 * 4);
  [%expect
    {|
    (repr
     (((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15))
      ((16 17 18 19) (20 21 22 23) (24 25 26 27) (28 29 30 31))
      ((32 33 34 35) (36 37 38 39) (40 41 42 43) (44 45 46 47))
      ((48 49 50 51) (52 53 54 55) (56 57 58 59) (60 61 62 63))))
    |}];
  test ((4 * 4 * 4) + 1);
  [%expect
    {|
    (repr
     ((((0 1) (2 3) 4) ((5 6) (7 8) 9) ((10 11) (12 13) 14) (15 16))
      (((17 18) (19 20) 21) ((22 23) (24 25) 26) ((27 28) (29 30) 31) (32 33))
      (((34 35) (36 37) 38) ((39 40) (41 42) 43) ((44 45) (46 47) 48) (49 50))
      ((51 52 53 54) (55 56 57 58) (59 60 61 62) (63 64))))
    |}]
;;
