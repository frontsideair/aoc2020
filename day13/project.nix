{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "day13";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}