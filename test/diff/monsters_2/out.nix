{
  lib = {

    /* Concatenate two lists

       Type: concat :: [a] -> [a] -> [a]

       Example:
         concat [ 1 2 ] [ 3 4 ]
         => [ 1 2 3 4 ]
    */
    concat = x: y: x ++ y;
  };

  options = {

    boot.kernel.features = mkOption {
      default = { };
      example = literalExpression "{ debug = true; }";
      internal = true;
      description = ''
        This option allows to enable or disable certain kernel features.
        It's not API, because it's about kernel feature sets, that
        make sense for specific use cases. Mostly along with programs,
        which would have separate nixos options.
        `grep features pkgs/os-specific/linux/kernel/common-config.nix`
      '';
    };
  };
}
