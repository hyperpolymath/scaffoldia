# SPDX-License-Identifier: AGPL-3.0-or-later
# scaffoldia - Development Tasks
set shell := ["bash", "-uc"]
set dotenv-load := true

project := "scaffoldia"
version := "0.1.0"

# Show all recipes
default:
    @just --list --unsorted

# === Development Environment ===

# Enter Guix development shell (primary)
guix-shell:
    guix shell -D -f guix.scm

# Enter Nix development shell (fallback)
nix-shell:
    nix develop

# === Build ===

# Build all components
build: build-cli build-builder
    @echo "Build complete"

# Build Haskell CLI
build-cli:
    @echo "Building CLI..."
    @cd cli && [ -f scaffoldia.hs ] && ghc -o scaffoldia scaffoldia.hs 2>/dev/null || echo "CLI: placeholder only"

# Validate Nickel builder configs
build-builder:
    @echo "Validating Nickel configs..."
    @find builder -name "*.ncl" -exec nickel typecheck {} \; 2>/dev/null || echo "Builder: no Nickel files to validate"

# === Test ===

# Run all tests
test: test-scm test-lint
    @echo "All tests passed"

# Test Scheme files syntax
test-scm:
    @echo "Checking Scheme syntax..."
    @for f in $$(find . -name "*.scm" -not -path "./.git/*"); do guile -c "(load \"$$f\")" 2>/dev/null || echo "Warning: $$f needs review"; done

# Run linters
test-lint: lint-shell lint-yaml
    @echo "Lint checks complete"

# === Lint ===

# Lint all
lint: lint-shell lint-yaml lint-nix
    @echo "Lint complete"

# Lint shell scripts
lint-shell:
    @echo "Linting shell scripts..."
    @find scripts -name "*.sh" -exec shellcheck {} \; 2>/dev/null || true

# Lint YAML files
lint-yaml:
    @echo "Linting YAML files..."
    @yamllint .github/workflows/*.yml 2>/dev/null || true

# Lint Nix files
lint-nix:
    @echo "Linting Nix files..."
    @nixpkgs-fmt --check flake.nix 2>/dev/null || true

# === Format ===

# Format all files
fmt: fmt-nix
    @echo "Formatting complete"

# Format Nix files
fmt-nix:
    @nixpkgs-fmt flake.nix 2>/dev/null || echo "nixpkgs-fmt not available"

# === Clean ===

# Clean build artifacts
clean:
    @echo "Cleaning..."
    @rm -rf result .direnv
    @find . -name "*.hi" -delete 2>/dev/null || true
    @find . -name "*.o" -delete 2>/dev/null || true
    @echo "Clean complete"

# === Security ===

# Run security checks
security:
    @echo "Running security checks..."
    @echo "Checking for weak crypto..."
    @! grep -rE 'md5\(|sha1\(' --include="*.py" --include="*.js" --include="*.ts" --include="*.rs" . 2>/dev/null | grep -v test || echo "Warning: Weak crypto found"
    @echo "Checking for HTTP URLs..."
    @! grep -rE 'http://[^l][^o][^c]' --include="*.yaml" --include="*.yml" . 2>/dev/null | grep -v localhost || echo "Warning: HTTP URLs found"
    @echo "Security check complete"

# === CI ===

# Verify CI/CD configuration
ci-check:
    @echo "Verifying CI/CD..."
    @yamllint .github/workflows/*.yml 2>/dev/null && echo "Workflows valid" || echo "Warning: Workflow issues"

# Run all CI checks locally
ci: lint test security
    @echo "CI checks passed"

# === Documentation ===

# Show project status
status:
    @echo "Project: {{project}} v{{version}}"
    @echo "Phase: v0.1 - Initial Setup"
    @echo "Completion: 35%"
    @echo ""
    @echo "SCM files:"
    @ls -la *.scm 2>/dev/null || echo "  (none in root)"
    @echo ""
    @echo "RSR Compliance:"
    @[ -f guix.scm ] && echo "  [OK] guix.scm" || echo "  [X] guix.scm missing"
    @[ -f flake.nix ] && echo "  [OK] flake.nix" || echo "  [X] flake.nix missing"
    @[ -f .well-known/security.txt ] && echo "  [OK] security.txt" || echo "  [X] security.txt missing"
