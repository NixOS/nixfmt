let 
  nixpkgs-mozilla = fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz;
in

with import <nixpkgs> {
  overlays = [ (import nixpkgs-mozilla) ];
};

stdenv.mkDerivation {
  name = "rnix";
  buildInputs = [ latest.rustChannels.nightly.rust ];
}
