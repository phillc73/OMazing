(** Copyright 2026, Phill Clarke *)
(** SPDX-License-Identifier: AGPL-3.0-or-later *)

(** Window placement syntax for the X11 geometry string: the "+X+Y" suffix of
    an open_graph request. A value is either accepted and rendered at that
    position, or rejected and treated exactly like an absent key. *)

val limit : int
(** Inclusive bound on each coordinate: -[limit] <= x, y <= [limit]. *)

val assumed_screen : int * int
(** Screen extent used when the display cannot be queried. *)

val check : string -> bool
(** [check value] is true when [value] is "+X+Y" or "-X-Y" with both
    coordinates within [limit]. Mixed signs are rejected. *)

val placement : string -> (int * int) option
(** [placement value] is the (x, y) the value names, if [check] accepts it. *)

val on_screen :
  string ->
  screen_w:int ->
  screen_h:int ->
  win_w:int ->
  win_h:int ->
  bool
(** [on_screen value ~screen_w ~screen_h ~win_w ~win_h] is true when the
    placement is valid and its origin lies inside the screen. The window size
    is accepted for callers that pass it; only the origin decides. *)

val geometry : ?screen:int * int -> int -> int -> string -> string
(** [geometry ?screen w h position] is the geometry string for one open_graph
    call. A rejected or missing [position] yields the absent-key string. *)
