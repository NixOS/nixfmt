let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  node = lock.nodes.${lock.nodes.${lock.root}.inputs.flake-compat};
  flake-compat = fetchTarball {
    url = node.locked.url or "https://github.com/NixOS/flake-compat/archive/${node.locked.rev}.tar.gz";
    sha256 = node.locked.narHash;
  };
in
(import flake-compat { src = ./.; }).outputs
