(lang dune 2.7)
(name bogue-demo)
(version "0.05")
(generate_opam_files true)
(license LGPL-3.0-or-later)
(maintainers "Vu Ngoc San <san.vu-ngoc@laposte.net>")
(authors "Vu Ngoc San <san.vu-ngoc@laposte.net>")
(source (github sanette/bogue-demo))

(package
 (name bogue-demo)
 (synopsis "A demo program for the Bogue library")
 (depends
  (bogue (>= 20221001))
))
