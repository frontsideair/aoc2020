{ mkDerivation, adjunctions, attoparsec, base, comonad, containers
, stdenv, text, total-maps
}:
mkDerivation {
  pname = "day24";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    adjunctions attoparsec base comonad containers text total-maps
  ];
  license = stdenv.lib.licenses.bsd3;
}
