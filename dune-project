(lang dune 3.4)

(name ibad)

(generate_opam_files true)

(source (github mitchdzugan/ocaml-ibad))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ibad)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ;; General system dependencies
  (dune (>= 2))
  (ocaml (>= 4.14.0))
  (ppxlib (>= 0.26.0))
  (alcotest (>= 1.6.0))
  (ocaml-lsp-server (>= 1.13.1))
  (ppx_ibad :dev)
  (ppx_map :dev)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
