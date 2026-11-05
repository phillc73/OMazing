(** Copyright 2026, Phill Clarke *)
(** SPDX-License-Identifier: AGPL-3.0-or-later *)

(** Window placement syntax. The value supplies the "+X+Y" part of the X11
    geometry string, so an accepted value must render and a rejected one must
    behave exactly like an absent key. *)

(** Sanity edge, inclusive: -8192 <= x, y <= 8192. It is checked before any
    display is open, because a value outside it is not representable as an X11
    window coordinate (the protocol carries them as int16) and is far past any
    plausible display. 8192 holds a 4K panel (3840 wide, 2160 high) at 2x its
    width, which covers a multi-monitor desktop with a positive origin and a
    small secondary panel offset up to 8192, while an off-screen window at that
    distance is not a placement anyone can use. *)
let limit = 8192

(** Screen extent used when the display cannot be queried (Xlib error paths):
    the app's documented target panel, 1920x1080. It keeps the on-screen check
    working on a display whose size cannot be read, and matches the README's
    width statement. *)
let assumed_screen = (1920, 1080)

let all_digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

(** One signed coordinate: "+N" or "-N", decimal digits only, magnitude within
    the limit. int_of_string_opt is used on the digit run only, so its hex and
    underscore leniency cannot reach the value. *)
let coord s =
  match String.length s with
  | 0 -> None
  | _ ->
      let sign = s.[0] in
      let digits = String.sub s 1 (String.length s - 1) in
      if (sign <> '+' && sign <> '-') || not (all_digits digits) then None
      else
        match int_of_string_opt digits with
        | Some n when n <= limit -> Some (sign, n)
        | _ -> None

(** Accept only "+X+Y" or "-X-Y" with each coordinate within the limit. The
    mixed-sign form is refused: "+120-80" is a different X11 geometry and was
    never part of this key's contract. *)
let check value =
  let same_sign sign x y =
    match (coord (String.make 1 sign ^ x), coord (String.make 1 sign ^ y)) with
    | Some (sx, _), Some (sy, _) -> sx = sign && sy = sign
    | _ -> false
  in
  match value with
  | _ when String.length value > 1 && value.[0] = '-' -> (
      match String.split_on_char '-' value with
      | [ ""; x; y ] -> same_sign '-' x y
      | _ -> false)
  | _ -> (
      match String.split_on_char '+' value with
      | [ ""; x; y ] -> same_sign '+' x y
      | _ -> false)

(** The placement on a given screen, as (x, y). *)
let placement value =
  if not (check value) then None
  else
    match String.split_on_char '+' value with
    | [ ""; x; y ] -> Some (int_of_string x, int_of_string y)
    | _ -> (
        match String.split_on_char '-' value with
        | [ ""; x; y ] -> Some (-int_of_string x, -int_of_string y)
        | _ -> None)

(** A placement renders unless the window starts at or beyond the screen's
    right or bottom edge. Observed: +2000+0 and +8192+0 on a 1280-wide screen
    mapped no Expose-collecting window and open_graph waited forever; the X
    server had shifted it wholly off-screen. A negative origin is corrected by
    the server (it shifts the window back inside: -500,-500 renders at
    -249,-554), so it is accepted. win_w/win_h are taken so the caller's
    window size is explicit, but the decision needs only the origin. *)
let on_screen value ~screen_w ~screen_h ~win_w ~win_h =
  let (_ : int * int) = (win_w, win_h) in
  match placement value with
  | None -> false
  | Some (x, y) -> x < screen_w && y < screen_h

(** The geometry string for one open_graph call. An empty or rejected
    placement leaves the string exactly as the absent-key case. *)
let geometry ?(screen = assumed_screen) w h position =
  let screen_w, screen_h = screen in
  if on_screen position ~screen_w ~screen_h ~win_w:w ~win_h:h then
    Printf.sprintf " %dx%d%s" w h position
  else Printf.sprintf " %dx%d" w h
