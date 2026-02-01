;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Project relationship mapping

(ecosystem
  (version "1.0")
  (name "scaffoldia")
  (type "project")
  (purpose "Developer-centred repo scaffolding engine that generates idiomatic, validated, and narratable project structures across languages and tools")

  (position-in-ecosystem
    (role "infrastructure-tool")
    (layer "developer-tooling")
    (description "Scaffoldia sits at the intersection of template engines, build tools, and project generators. It validates templates via Haskell, composes scaffolds with Nickel, infers structure with MiniKanren, and provides visual building with ReScript/Tauri."))

  (related-projects
    ((asdf-ui-plugin
      (relationship . "generated-by")
      (description . "SvelteKit/Vite dashboard plugin generated using Scaffoldia's asdf template. Demonstrates aspect-oriented Nickel config with RBAC, audit logging, and strict CSP.")
      (url . "https://gitlab.com/hyperpolymath/asdf-ui-plugin"))

     (asdf-security-plugin
      (relationship . "generated-by")
      (description . "ReScript+Bun security plugin generated using Scaffoldia. Provides CVE scanning, VirusTotal integration, and provenance tracking for asdf-managed tools.")
      (url . "https://gitlab.com/hyperpolymath/asdf-security-plugin"))

     (asdf-metaiconic-plugin
      (relationship . "generated-by")
      (description . "Nickel-powered aspect-oriented config plugin. Generates deployment artefacts (wrangler.toml, Quadlet units, docs) and weaves cross-cutting concerns into projects.")
      (url . "https://gitlab.com/hyperpolymath/asdf-metaiconic-plugin"))

     (rsr-template-repo
      (relationship . "sibling-standard")
      (description . "Rhodium Standard Repository template that Scaffoldia extends. Scaffoldia adds language/tool-specific templates while maintaining RSR compliance.")
      (url . "https://github.com/hyperpolymath/rsr-template-repo"))

     (robot-repo-automaton
      (relationship . "consumer")
      (description . "Automated repo management system that consumes Scaffoldia templates to apply fixes and maintain standards across repos.")
      (url . "https://github.com/hyperpolymath/robot-repo-automaton"))

     (gitbot-fleet
      (relationship . "consumer")
      (description . "Bot orchestration hub (rhodibot, echidnabot, etc.) that uses Scaffoldia templates for repo initialization and standardization.")
      (url . "https://github.com/hyperpolymath/gitbot-fleet"))

     (hypatia
      (relationship . "integration")
      (description . "Neurosymbolic CI/CD platform that scans Scaffoldia-generated repos and provides security/quality feedback.")
      (url . "https://github.com/hyperpolymath/hypatia"))

     (asdf-vm
      (relationship . "inspiration")
      (description . "Version manager that inspired Scaffoldia's asdf plugin templates. Scaffoldia generates asdf-compliant plugins with full feature support.")
      (url . "https://github.com/asdf-vm/asdf"))))

  (what-this-is
    "Scaffoldia is:\n  - A template validation engine (Haskell registry)\n  - A composition system (Nickel aspect-oriented configs)\n  - A structure inference tool (MiniKanren constraints)\n  - A visual builder (ReScript/Tauri Blockly-style UI)\n  - A project generator with narrative, not just code\n\nIt generates repos that are:\n  - Idiomatic (follows language/tool conventions)\n  - Validated (type-checked configs, structure proofs)\n  - Narratable (self-documenting, explains 'why')\n  - Compliant (RSR, OpenSSF Scorecard, asdf standards)\n  - Aspect-aware (security, licensing, deployment woven in)")

  (what-this-is-not
    "Scaffoldia is NOT:\n  - A generic templating engine (use Mustache/Jinja for that)\n  - A build tool (use Just, Make, Bazel)\n  - A package manager (use Guix, Nix, asdf)\n  - A CI/CD platform (use Hypatia, GitHub Actions)\n  - A code generator (use language-specific tools)\n\nIt's a meta-tool that orchestrates these systems to create complete, production-ready repo scaffolds."))
