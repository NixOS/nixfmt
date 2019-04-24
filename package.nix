{ mkDerivation, base, cmdargs, megaparsec, parallel-io
, parser-combinators, prettyprinter, stdenv, text
}:
mkDerivation {
  pname = "nixfmt";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base megaparsec parser-combinators prettyprinter text
  ];
  executableHaskellDepends = [ base cmdargs parallel-io text ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
