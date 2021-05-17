{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs.elmPackages; [ elm elm-format elm-test ];
}
