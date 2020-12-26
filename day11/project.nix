{ mkDerivation, adjunctions, base, comonad, distributive, parsec
, stdenv, vector
}:
mkDerivation {
  pname = "day11";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    adjunctions base comonad distributive parsec vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
