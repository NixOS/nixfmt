{stdenv, lib, fetchFrom, ... }:

stdenv.mkDerivation rec {
  pname = "test";
  version = "0.0";
  src = fetchFrom {
    url = "example/${version}";
  };
  meta = with lib; {
    maintainers = with maintainers; [ someone ];
    description = "something";
  };
}
