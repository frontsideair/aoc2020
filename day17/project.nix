{ mkDerivation, adjunctions, base, comonad, containers
, distributive, parsec, stdenv, total-maps, vector
}:
mkDerivation {
  pname = "day17";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    adjunctions base comonad containers distributive parsec total-maps
    vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
