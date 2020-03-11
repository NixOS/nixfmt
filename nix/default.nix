let sources = import ./sources.nix; in
import sources.nixpkgs (import sources."haskell.nix")
