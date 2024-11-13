(lang dune 3.16)

(name lwt_ssl)

(license "LGPL with OpenSSL linking exception")

(authors "Jérôme Vouillon" "Jérémie Dimino")

(maintainers "Anton Bachin <antonbachin@yahoo.com>")

(source
 (github ocsigen/lwt_ssl))

(generate_opam_files true)

(package
 (name lwt_ssl)
 (synopsis "OpenSSL binding with concurrent I/O")
 (depends
  ocaml
  base-unix
  (lwt
   (>= 3.0.0))
  (ssl
   (>= 0.5.13))))
