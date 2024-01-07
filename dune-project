(lang dune 3.12)

(name proj)

(generate_opam_files true)

(source
 (github cosmicboots/proj))

(authors "Thom Dickson")

(maintainers "Thom Dickson")

(package
 (name proj)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune cmdliner petrol caqti-driver-sqlite3 caqti-lwt)
 (tags
  (topics "to describe" your project)))
