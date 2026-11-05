# Release notes

## 1.0.0

First release.

### Window placement

The `window-position` config key requests a position for the game window.
The request is validated; whether it is honoured depends on the window
manager:

- A manager that owns placement discards the request. This covers tiling
  managers and most compositors on Wayland, where clients cannot set
  their own position and mutter ignores the hint. On such a desktop the
  setting is validated and then has no effect on where the window
  appears.
- A manager that honours position hints places the window as asked. This
  is typical of X11 sessions with a stacking window manager that defers
  to the client.

Validation is unconditional:

- Each coordinate must be decimal digits with a single sign, between
  -8192 and 8192, and the window must start inside the screen it
  launches on. A value outside either limit is rejected: the window
  opens at the default position and one warning line is written to
  stderr. The behaviour is then identical to having no `window-position`
  key at all.
- A negative origin is accepted. X11 uses it for monitors to the left of
  or above the primary, where the window manager honours the hint.

### Fixed

- `Q` at the difficulty menu now quits the program, as the menu prompt
  says. Previously the key was mapped to no action and the program kept
  running.
- A `window-position` value that could not render no longer hangs the
  launch. Values were passed to X11 verbatim, where an out-of-range value
  either wrapped to a different coordinate or placed the window entirely
  off screen; the window then never received its first expose event and
  the program waited indefinitely with no window and no error. Such
  values now fall back to the default position with a warning.
