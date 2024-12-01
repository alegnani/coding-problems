{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
        with pkgs; {
          devShells.default = mkShell {
            buildInputs = [
              opam
              ocaml
              dune_3
              ocamlPackages.odoc
              ocamlPackages.ocamlformat
              ocamlPackages.ocaml-lsp
              ocamlPackages.utop
              ocamlPackages.core
              ocamlPackages.stdio
            ];
            shellHook = "eval $(opam env --switch=5.2.0)";
          };
        }
    );
}
