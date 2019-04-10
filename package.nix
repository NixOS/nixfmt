{ mkDerivation, base, megaparsec, prettyprinter, stdenv, text }:
mkDerivation {
  pname = "nixfmt";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base megaparsec prettyprinter text ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
