(lang dune 3.11)

(name pq_lang)

(generate_opam_files true)

(source
 (github ofsouzap/pq-lang))

(authors "Ofsouzap")

(maintainers "Ofsouzap")

(license MIT)

(using menhir 2.0)

(package
 (name pq_lang)
 (synopsis "ML-like language with quotient types")
 (depends ocaml dune menhir ounit2))
