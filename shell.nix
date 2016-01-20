{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, exceptions, mtl, stdenv, transformers
      , unexceptionalio
      }:
      mkDerivation {
        pname = "algebraic-mtl";
        version = "1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base exceptions mtl transformers unexceptionalio
        ];
        homepage = "https://github.com/ocharles/algebraic-mtl";
        description = "A variation of the mtl using only algebraic effects in type classes";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
