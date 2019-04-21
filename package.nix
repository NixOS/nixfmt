{ mkDerivation, base, megaparsec, parser-combinators, prettyprinter
, stdenv, text
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
  executableHaskellDepends = [
    base megaparsec parser-combinators prettyprinter text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
