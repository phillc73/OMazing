# Release notes

## 1.0.0

First release.

### Window placement

The `window-position` config key places the game window. It is best-effort:

- A window manager that owns placement (tiling, or a compositor that
  controls window geometry) may override or ignore the requested
  position. Under mutter and Wayland the placement request is discarded
  by design and the window opens where the compositor decides.
- Each coordinate is between -8192 and 8192, and the window must start
  inside the screen it launches on. A value outside either limit is
  rejected: the window opens at the default position and one warning
  line is written to stderr. The behaviour is then identical to having
  no `window-position` key at all.
- A negative origin is accepted. X11 uses it for monitors to the left of
  or above the primary, and the X server moves a window that crosses the
  left or top edge back inside.

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
