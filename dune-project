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
 (depends
  (ocaml
   (>= "5.2.0"))
  dune
  (core
   (>= "v0.16.2"))
  (core_unix
   (>= "1.15.0"))
  (menhir
   (>= "20240715"))
  (z3
   (= "4.13.0-3"))
  (alcotest
   (>= "1.8.0"))
  (qcheck
   (>= "0.23"))
  (qcheck-alcotest
   (>= "0.23"))))
