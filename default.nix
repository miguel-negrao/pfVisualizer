{ mkDerivation, base, containers, GLUT, hosc, OpenGL, random, safe
 , split, stdenv, stm, lens
 }:
 mkDerivation {
   pname = "pfVisualizer";
   version = "0.1.0.1";
   src = ./.;
   isLibrary = false;
   isExecutable = true;
   buildDepends = [
     base containers GLUT hosc OpenGL random safe split stm lens
   ];
   buildTools =  [];
   license = stdenv.lib.licenses.gpl3;
 }
