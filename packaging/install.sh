#!/bin/sh
# Copyright 2026, Phill Clarke
# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Installs the OMazing Labyrinth binary from this archive.
#
# usage: sh install.sh [prefix]
#
# The prefix defaults to ~/.local. The binary lands in <prefix>/bin/omazing.
# Any existing binary at that path is copied to omazing.bak-<timestamp> first,
# so an install can always be undone by moving it back. A config file the user
# already has is never touched: window placement lives in
# ~/.config/omazing/config and this script does not write there.
set -eu

PREFIX=${1:-$HOME/.local}
BIN_DIR=$PREFIX/bin
HERE=$(cd "$(dirname "$0")" && pwd)

test -f "$HERE/omazing" || {
  echo "install.sh: omazing binary missing from the archive" >&2
  exit 1
}
test -r "$HERE/omazing" || {
  echo "install.sh: omazing binary is not readable" >&2
  exit 1
}

if test -e "$BIN_DIR" && test ! -d "$BIN_DIR"; then
  echo "install.sh: $BIN_DIR is not a directory" >&2
  exit 1
fi

mkdir -p "$BIN_DIR"
if test -e "$BIN_DIR/omazing"; then
  BACKUP=$BIN_DIR/omazing.bak-$(date +%Y%m%d%H%M%S)
  n=1
  while test -e "$BACKUP"; do
    BACKUP=$BIN_DIR/omazing.bak-$(date +%Y%m%d%H%M%S)-$n
    n=$((n + 1))
  done
  cp -p "$BIN_DIR/omazing" "$BACKUP"
  echo "previous binary saved as $BACKUP"
fi

cp "$HERE/omazing" "$BIN_DIR/omazing"
chmod 755 "$BIN_DIR/omazing"
echo "installed $BIN_DIR/omazing"
