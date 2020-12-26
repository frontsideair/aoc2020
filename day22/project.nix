{ mkDerivation, attoparsec, base, containers, stdenv, text }:
mkDerivation {
  pname = "day22";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base containers text ];
  license = stdenv.lib.licenses.bsd3;
}
