(lang dune 3.12)

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
 (depends ocaml dune menhir ppx_deriving ounit2)
 (tags
  (lambda-calculus interpreter programming-languages)))

(using menhir 2.1)
