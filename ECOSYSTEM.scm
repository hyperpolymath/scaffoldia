;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — scaffoldia

(ecosystem
  (version "1.0.0")
  (name "scaffoldia")
  (type "project")
  (purpose "*Scaffoldia* is a developer-centred, modular, and community-driven repo scaffolding engine.")

  (position-in-ecosystem
    "Part of hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is "*Scaffoldia* is a developer-centred, modular, and community-driven repo scaffolding engine.")
  (what-this-is-not "- NOT exempt from RSR compliance"))
