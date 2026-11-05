(** Copyright 2026, Phill Clarke *)
(** SPDX-License-Identifier: AGPL-3.0-or-later *)

(** OMazing - window-position validation tests (F-2).

    The config value supplies the X11 geometry string, so the contract is: an
    accepted value renders, a rejected value behaves exactly like an absent
    key. The validator is display-independent - an off-screen value is caught
    at open time, against the real screen - so these tests drive the pure
    checks: the syntax/range contract, placement parsing, and the on-screen
    test against a screen supplied by the test. *)

open Window_position

let check_ok s = Alcotest.(check bool) ("accepted: " ^ s) true (check s)
let check_drop s = Alcotest.(check bool) ("dropped: " ^ s) false (check s)

let geometry_ok () =
  Alcotest.(check string) "absent key" " 1025x950" (geometry 1025 950 "")

let accepted () =
  check_ok "+120+80";
  check_ok "+0+0";
  (* contract edges, inclusive *)
  check_ok "+8192+8192";
  check_ok "-8192-8192";
  (* negatives are legitimate X11 placement: spanning monitors and window
     managers that put decorations above y=0 *)
  check_ok "-40-100"

let rejected_syntax () =
  List.iter check_drop
    [ ""; "120+80"; "+120"; "+a+b"; "+120+"; "++"; "+120+80+1"; "+ 120+80"; "+120-80" ]

let rejected_overflow () =
  (* int16 representability: the X protocol carries coordinates as int16, and
     a value beyond it wraps to a different (plausible-looking) position *)
  List.iter check_drop
    [ "+32767+0"; "+50000+40000"; "+999999999999+0"; "-32000-40"; "+131192+0" ]

let screen_ok s = Alcotest.(check bool) ("on-screen: " ^ s) true (on_screen s ~screen_w:1280 ~screen_h:900 ~win_w:1025 ~win_h:950)
let screen_drop s = Alcotest.(check bool) ("off-screen: " ^ s) false (on_screen s ~screen_w:1280 ~screen_h:900 ~win_w:1025 ~win_h:950)

let off_screen () =
  (* a window starting past the screen's right or bottom edge never receives
     its first Expose and open_graph then blocks forever *)
  List.iter screen_drop
    [ "+8192+0"; "+32767+32767"; "+1280+0"; "+0+900"; "+2000+0"; "+50000+40000"; "+8192+8192" ];
  (* a window overhanging an edge, or with a negative origin (which the server
     shifts back inside), still renders *)
  List.iter screen_ok
    [ "+120+80"; "+0+0"; "-500-500"; "-1024-900"; "+1100+0"; "+1279+0"; "+0+899"; "+120+899" ]

let placement_ok () =
  Alcotest.(check (option (pair int int))) "+120+80" (Some (120, 80)) (placement "+120+80");
  Alcotest.(check (option (pair int int))) "-40-100" (Some (-40, -100)) (placement "-40-100");
  Alcotest.(check (option (pair int int))) "garbage" None (placement "+50000+40000")

let cases =
  [
    ("geometry", `Quick, geometry_ok);
    ("accepted", `Quick, accepted);
    ("rejected syntax", `Quick, rejected_syntax);
    ("rejected overflow", `Quick, rejected_overflow);
    ("off-screen", `Quick, off_screen);
    ("placement", `Quick, placement_ok);
  ]

let tests = [ ("window-position", List.map (fun (n, s, f) -> Alcotest.test_case n s f) cases) ]
