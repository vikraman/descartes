(lang dune 3.0)

(name descartes)

(generate_opam_files true)

(source
 (github vikraman/descartes))

(authors "Vikraman Choudhury")

(maintainers "Vikraman Choudhury")

(license LICENSE)

(documentation https://github.com/vikraman/descartes)

(package
 (name descartes)
 (synopsis "An interpreter for lambda-lambda-bar")
 (description "An interpreter for the lambda-lambda-bar calculus")
 (depends
  ocaml
  dune
  menhir
  ppx_deriving
  (alcotest :with-test))
 (tags
  (lambda-calculus interpreter programming-languages)))

(using menhir 2.1)
