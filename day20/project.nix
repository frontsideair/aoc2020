{ mkDerivation, attoparsec, base, bytestring, containers, matrix
, relude, stdenv, vector
}:
mkDerivation {
  pname = "day20";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring containers matrix relude vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
