{ mkDerivation, array, base, containers, stdenv, vector }:
mkDerivation {
  pname = "day23";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ array base containers vector ];
  license = stdenv.lib.licenses.bsd3;
}
