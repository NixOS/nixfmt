{ mkDerivation, base, cmdargs, directory, megaparsec, parallel-io
, parser-combinators, stdenv, text, unix
}:
mkDerivation {
  pname = "nixfmt";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base megaparsec parser-combinators text
  ];
  executableHaskellDepends = [
    base cmdargs directory parallel-io text unix
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
