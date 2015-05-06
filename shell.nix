with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, GLUT, hosc, OpenGL, random, safe
             , split, stdenv, stm, lens, ghc-mod
             }:
             mkDerivation {
               pname = "pfVisualizer";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 base containers GLUT hosc OpenGL random safe split stm lens
               ];
               buildTools =  [ghc-mod];
               license = stdenv.lib.licenses.gpl3;
             }) {};
in
  pkg.env
