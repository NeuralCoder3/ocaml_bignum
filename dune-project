(lang dune 3.11)

(name bignum)

(generate_opam_files true)

(source
 (github NeuralCoder3/ocaml_bignum))

(authors "Marcel Ullrich")

(maintainers "Marcel Ullrich")

(license LICENSE)

(documentation https://github.com/NeuralCoder3/ocaml_bignum)

(package
 (name bignum)
 (synopsis "A self-contained big number implementation from scratch. Supports Nat and Z.")
 (description "This library contains a small and self-contained implementation of unbounded (arbitrary precision) integers, also known as BigNum or BigInt. The library does not use any other libraries and is usuable by copying it into any other OCaml code. For this purpose, the main modules of the library are not separated into individual files.")
 (depends ocaml dune)
 (tags
  (bignum)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
