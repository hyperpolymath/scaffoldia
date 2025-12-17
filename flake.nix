# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 hyperpolymath
# flake.nix - Nix Flake (fallback to guix.scm)
# NOTE: Guix is the primary package manager. Use guix.scm when possible.
{
  description = "Scaffoldia - Developer-centred, modular, and community-driven repo scaffolding engine";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "scaffoldia-dev";

          buildInputs = with pkgs; [
            # Core tools
            just
            gnumake

            # Scheme/Guile for SCM files
            guile

            # Haskell for CLI (scaffoldia.hs)
            ghc
            cabal-install

            # Nickel for builder
            nickel

            # Node/Deno for UI (RSR prefers Deno)
            deno

            # Linting and formatting
            shellcheck
            yamllint
            nixpkgs-fmt
          ];

          shellHook = ''
            echo "Scaffoldia development shell (Nix fallback)"
            echo "NOTE: Guix is primary. Consider: guix shell -D -f guix.scm"
            echo ""
            echo "Available commands:"
            echo "  just --list    # Show available tasks"
            echo "  deno task      # Run Deno tasks"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "scaffoldia";
          version = "0.1.0";
          src = ./.;

          meta = with pkgs.lib; {
            description = "Developer-centred, modular, and community-driven repo scaffolding engine";
            homepage = "https://github.com/hyperpolymath/scaffoldia";
            license = licenses.agpl3Plus;
            maintainers = [ ];
            platforms = platforms.all;
          };
        };
      }
    );
}
