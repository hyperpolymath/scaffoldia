<<<<<<< HEAD
; SPDX-License-Identifier: MPL-2.0
;; guix.scm — GNU Guix package definition for squisher-corpus
;; Usage: guix shell -f guix.scm
=======
;; SPDX-License-Identifier: MPL-2.0
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
;;
;; Guix package definition for scaffoldia
;;
;; Usage:
;;   guix shell -D -f guix.scm    # Enter development shell
;;   guix build -f guix.scm       # Build package
;;
;; TODO: real build/check phases (Idris2 + Zig) are not yet wired — see the
;; `(delete 'build) (delete 'check)` phases below. No Guix toolchain was
;; available to verify a real build; this stays a stub until it can be.
;; See: https://guix.gnu.org/manual/en/html_node/Defining-Packages.html
>>>>>>> 0a02c7b8b1f0055a8a4871433852b0fbf0681cb4

(use-modules (guix packages)
             (guix build-system gnu)
             (guix licenses))

(package
<<<<<<< HEAD
  (name "squisher-corpus")
=======
  (name "scaffoldia")
>>>>>>> 0a02c7b8b1f0055a8a4871433852b0fbf0681cb4
  (version "0.1.0")
  (source #f)
  (build-system gnu-build-system)
<<<<<<< HEAD
  (synopsis "squisher-corpus")
  (description "squisher-corpus — part of the hyperpolymath ecosystem.")
  (home-page "https://github.com/hyperpolymath/squisher-corpus")
  (license ((@@ (guix licenses) license) "PMPL-1.0-or-later"
             "https://github.com/hyperpolymath/palimpsest-license")))
=======
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       ;; TODO: Customize build phases for your project
       ;; Examples for common stacks:
       ;;
       ;; Rust:
       ;;   (replace 'build (lambda _ (invoke "cargo" "build" "--release")))
       ;;   (replace 'check (lambda _ (invoke "cargo" "test")))
       ;;
       ;; Elixir:
       ;;   (replace 'build (lambda _ (invoke "mix" "compile")))
       ;;   (replace 'check (lambda _ (invoke "mix" "test")))
       ;;
       ;; Zig:
       ;;   (replace 'build (lambda _ (invoke "zig" "build")))
       ;;   (replace 'check (lambda _ (invoke "zig" "build" "test")))
       (delete 'configure)
       (delete 'build)
       (delete 'check)
       (replace 'install
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (mkdir-p (string-append out "/share/doc"))
             (copy-file "README.adoc"
                        (string-append out "/share/doc/README.adoc"))))))))
  (native-inputs
   (list
    ;; TODO: Add build-time dependencies
    ;; Examples:
    ;;   rust (gnu packages rust)
    ;;   elixir (gnu packages elixir)
    ;;   zig (gnu packages zig)
    ))
  (inputs
   (list
    ;; TODO: Add runtime dependencies
    ))
  (home-page "https://github.com/hyperpolymath/scaffoldia")
  (synopsis "Full-featured repository designer for the RSR estate")
  (description "Composes spine, variant pack, features, and profile into new
repositories, and retrofits existing ones.  Consumes each variant template's
VARIANT.a2ml contract; sits at the top of the estate's scaffolding stack
(standards -> rsr-template-repo -> scaffoldia).  See README.adoc for
details.")
  (license (list
            ;; MPL-2.0 extends MPL-2.0
            mpl2.0)))
>>>>>>> 0a02c7b8b1f0055a8a4871433852b0fbf0681cb4
