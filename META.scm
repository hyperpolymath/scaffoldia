;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Project metadata and architectural decisions

(define project-meta
  `((version . "1.0.0")
    (architecture-decisions
      ((adr-001
        ((status . "accepted")
         (date . "2026-02-01")
         (title . "Nickel-Based Aspect-Oriented Configuration")
         (context . "Scaffoldia needs a way to generate repos with cross-cutting concerns (security, licensing, deployment) that don't belong in a single file but affect the entire project structure.")
         (decision . "Use Nickel as the primary configuration language for aspect-oriented project generation. Nickel's contracts, type system, and composability make it ideal for weaving aspects (security policies, deployment configs, license headers) into generated scaffolds.")
         (consequences
          . ("+ Type-safe configuration with contracts"
             "+ Aspect composition without code duplication"
             "+ Declarative, narratable config (vs embedded prose)"
             "+ Generates deployment artefacts (wrangler.toml, Quadlet units, CSP headers)"
             "- Learning curve for Nickel syntax"
             "- Small ecosystem compared to JSON/YAML"))))

       (adr-002
        ((status . "accepted")
         (date . "2026-02-01")
         (title . "Three-Repo asdf Plugin Architecture")
         (context . "The asdf plugin ecosystem requires clear separation of concerns. Monolithic plugins become unmaintainable. Users should be able to install UI, security, or metaiconic features independently.")
         (decision . "Scaffold asdf plugins as three separate repos with distinct responsibilities:\n  - asdf-ui-plugin: SvelteKit/Vite dashboard, RBAC, audit log (port 843)\n  - asdf-security-plugin: CVE scanning, VirusTotal, provenance (port 844)\n  - asdf-metaiconic-plugin: Nickel aspect injection, config generation (port 845, CLI-style)\n  Each repo is a valid asdf plugin (bin/list-all, bin/install, etc.) and uses ReScript+Bun for logic.")
         (consequences
          . ("+ Clear separation of concerns (UI vs security vs config)"
             "+ Independent versioning and installation"
             "+ No port conflicts (843, 844, 845)"
             "+ Composable when all three installed (UI consumes Security APIs, Metaiconic generates configs)"
             "+ Easier testing and maintenance"
             "- Requires coordination for cross-plugin features"
             "- Three repos to manage instead of one"))))

       (adr-003
        ((status . "accepted")
         (date . "2026-02-01")
         (title . "ReScript + Bun for Plugin Logic")
         (context . "asdf plugins need fast, portable, type-safe logic. Shell scripts are brittle. Node.js has slow startup. TypeScript is banned per hyperpolymath policy.")
         (decision . "Use ReScript compiled to JS, executed with Bun. ReScript provides type safety and functional patterns. Bun provides fast startup and native TypeScript/JSX support. Shell wrappers in bin/ call into Bun-executed ReScript code.")
         (consequences
          . ("+ Type-safe plugin logic (vs shell scripts)"
             "+ Fast startup (Bun vs Node)"
             "+ Cross-platform compatibility"
             "+ Aligns with hyperpolymath language policy (no TypeScript)"
             "- Requires Bun installed on user system"
             "- Build step needed (rescript build)"))))

       (adr-004
        ((status . "accepted")
         (date . "2026-02-01")
         (title . "Cloudflare Pages as Default Deployment Target")
         (context . "Scaffoldia-generated projects need a zero-config, scalable deployment option. Self-hosting is complex for small teams. Traditional PaaS (Heroku) is expensive.")
         (decision . "Default to Cloudflare Pages for web apps with Node.js fallback and Podman Quadlet for persistent services. Generate wrangler.toml, _routes.json, and adapter configs automatically.")
         (consequences
          . ("+ Zero-config deployment (git push)"
             "+ Edge network performance"
             "+ Free tier for small projects"
             "+ Works with SvelteKit, static sites, APIs"
             "+ Node fallback for local development"
             "+ Podman Quadlet for services needing persistence"
             "- Cloudflare lock-in (mitigated by Node adapter)"
             "- Learning curve for Workers API"))))

       (adr-005
        ((status . "accepted")
         (date . "2026-02-01")
         (title . "asdf Plugin Standard Compliance")
         (context . "asdf plugins must follow specific conventions (bin/list-all, bin/install, etc.) to work correctly. Scaffoldia should generate compliant plugins that leverage all asdf features.")
         (decision . "Generate asdf plugins with:\n  - Required: bin/list-all, bin/install, bin/download\n  - Recommended: bin/latest-stable, bin/help.*, bin/list-bin-paths\n  - Optional: bin/exec-env, bin/uninstall, lib/commands/*\n  Use ASDF_INSTALL_PATH and other env vars correctly. Test with 'asdf plugin test'.")
         (consequences
          . ("+ Full asdf compatibility"
             "+ Users can leverage all asdf features (version management, shims, etc.)"
             "+ Proper error handling and debugging"
             "+ Extension commands for custom CLI"
             "- More boilerplate than simple scripts"
             "- Must maintain compatibility with asdf updates"))))

       (adr-006
        ((status . "accepted")
         (date . "2026-02-01")
         (title . "Nickel Production Profiles")
         (context . "Different deployment environments (production, staging, local) need different configs but share common structure. Hardcoding values leads to duplication and errors.")
         (decision . "Use Nickel profiles (production.ncl, hardened.ncl, etc.) to define environment-specific configs. Profiles include:\n  - Tags (ui, security, audit, licensing, provenance)\n  - Deploy targets (cloudflare_pages, node_fallback)\n  - Service config (port, persistent flag)\n  - RBAC roles (operator, viewer, admin)\n  - Security policies (CSP, allowed external connections)\n  - License metadata (primary, secondary, canonical URL)")
         (consequences
          . ("+ Single source of truth for environment configs"
             "+ Type-checked configuration (Nickel contracts)"
             "+ Easy to add new environments"
             "+ Prevents config drift between repos"
             "- Nickel eval needed at build time"
             "- Must export to JSON/TOML for runtime")))))))

    (development-practices
      ((code-style . "standard")
       (security . "openssf-scorecard")
       (versioning . "semver")
       (documentation . "asciidoc")
       (branching . "trunk-based")))

    (design-rationale
      ((why-nickel-over-jsonnet
        . "Nickel has better ergonomics, gradual typing, and stronger contract system than Jsonnet. It's designed for configuration, not templating.")
       (why-rescript-over-typescript
        . "ReScript has sound type system, no 'any' escape hatches, better inference, and aligns with hyperpolymath language policy (no TypeScript).")
       (why-three-repos-not-monorepo
        . "asdf plugins are independent units. Three repos allow independent installation, versioning, and maintenance. Users can install only what they need.")
       (why-bun-over-node
        . "Bun has ~4x faster startup, built-in TypeScript/JSX, better dev experience. Critical for CLI tools where startup time matters.")
       (why-aspect-oriented
        . "Cross-cutting concerns (security, licensing, deployment) shouldn't be scattered across files. Aspects let us inject them declaratively from a single config.")))))
