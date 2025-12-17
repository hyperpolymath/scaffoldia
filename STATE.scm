;;; STATE.scm — scaffoldia
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

(define metadata
  '((version . "0.1.0") (updated . "2025-12-17") (project . "scaffoldia")))

(define current-position
  '((phase . "v0.1 - Foundation Complete")
    (overall-completion . 35)
    (components
     ((rsr-compliance ((status . "complete") (completion . 100)))
      (scm-files ((status . "complete") (completion . 100)))
      (security ((status . "complete") (completion . 100)))
      (ci-cd ((status . "complete") (completion . 100)))
      (registry ((status . "planned") (completion . 0)))
      (builder ((status . "planned") (completion . 0)))
      (cli ((status . "planned") (completion . 0)))
      (ui ((status . "planned") (completion . 0)))))))

(define blockers-and-issues '((critical ()) (high-priority ())))

(define roadmap
  '((v0.1 ((name . "Foundation")
           (status . "complete")
           (items . ("RSR compliance" "SCM files (guix.scm, flake.nix)" "Security workflows" "CI/CD pipelines" "justfile tasks"))))
    (v0.2 ((name . "Core Engine")
           (status . "planned")
           (items . ("Registry templates (Rust, Haskell, Docker, Elixir)"
                     "Builder Nickel configs"
                     "Containerfile"
                     "MiniKanren constraint rules"))))
    (v0.3 ((name . "CLI & Integration")
           (status . "planned")
           (items . ("Haskell CLI (scaffoldia.hs)"
                     "Template validation"
                     "CI/CD injection scripts"))))
    (v0.4 ((name . "Visual Builder")
           (status . "planned")
           (items . ("Svelte/Vite UI"
                     "Blockly-style builder"
                     "Template preview"))))
    (v1.0 ((name . "Production Release")
           (status . "planned")
           (items . ("Full documentation"
                     "Community templates"
                     "Plugin system"
                     "Multi-language support"))))))

(define critical-next-actions
  '((immediate (("Implement registry templates" . high)))
    (this-week (("Add builder Nickel configs" . medium)
                ("Create Containerfile" . medium)))))

(define session-history
  '((snapshots
     ((date . "2025-12-15") (session . "initial") (notes . "SCM files added"))
     ((date . "2025-12-17") (session . "security-review") (notes . "Fixed security.txt, added flake.nix, updated justfile, RSR compliance complete")))))

(define state-summary
  '((project . "scaffoldia")
    (completion . 35)
    (blockers . 0)
    (updated . "2025-12-17")
    (next-milestone . "v0.2 - Core Engine")))
