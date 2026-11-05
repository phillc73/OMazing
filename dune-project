(lang dune 3.15)

(package
  (name omazing)
  (synopsis "OMazing Labyrinth - A maze navigation game")
  (description "An interactive maze game with multiple generation algorithms and difficulty levels")
  (license AGPL-3.0-or-later)
  (maintainers "Phill Clarke")
  (depends
    (ocaml (>= 4.08.0))
    (dune (>= 3.0))
    graphics
    unix
    (qcheck :with-test)
    (qcheck-alcotest :with-test)
    (alcotest :with-test))
  (allow_empty)
  )
