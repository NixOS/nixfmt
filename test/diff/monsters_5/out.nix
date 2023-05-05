{

config,

lib,

pkgs,

...

}:

with

  lib;

let

  inherit

  (config.boot)

    kernelPatches;

  inherit

  (config.boot.kernel)

    features

    randstructSeed;

  inherit

  (config.boot.kernelPackages)

    kernel;

  kernelModulesConf

    =

    pkgs.writeText

    "nixos.conf"

    ''
      ${concatStringsSep "\n" config.boot.kernelModules}
    '';

in {

  ###### interface

  options

    =

    {

      boot.kernel.features

        =

        mkOption

        {

          default

            =

            { };

          example

            =

            literalExpression

            "{debug= true;}";

          internal

            =

            true;

          description

            =

            ''
              This option allows to enable or disable certain kernel features.
              It's not API, because it's about kernel feature sets, that
              make sense for specific use cases. Mostly along with programs,
              which would have separate nixos options.
              `grep features pkgs/os-specific/linux/kernel/common-config.nix`
            '';

        };

      boot.kernelPackages

        =

        mkOption

        {

          default

            =

            pkgs.linuxPackages;

          type

            =

            types.unspecified

            //

            {

              merge

                =

                mergeEqualOption;

            };

          apply

            =

            kernelPackages:

            kernelPackages.extend

            (self:

              super:

              {

                kernel

                  =

                  super.kernel.override

                  (originalArgs:

                    {

                      inherit

                        randstructSeed;

                      kernelPatches

                        =

                        (originalArgs.kernelPatches

                          or

                          [ ])

                        ++

                        kernelPatches;

                      features

                        =

                        lib.recursiveUpdate

                        super.kernel.features

                        features;

                    });

              });

          # We don't want to evaluate all of linuxPackages for the manual
          # - some of it might not even evaluate correctly.

          defaultText

            =

            literalExpression

            "pkgs.linuxPackages";

          example

            =

            literalExpression

            "pkgs.linuxKernel.packages.linux_5_10";

          description

            =

            ''
              This option allows you to override the Linux kernel used by
              NixOS.  Since things like external kernel module packages are
              tied to the kernel you're using, it also overrides those.
              This option is a function that takes Nixpkgs as an argument
              (as a convenience), and returns an attribute set containing at
              the very least an attribute <varname>kernel</varname>.
              Additional attributes may be needed depending on your
              configuration.  For instance, if you use the NVIDIA X driver,
              then it also needs to contain an attribute
              <varname>nvidia_x11</varname>.
            '';

        };

      boot.kernelPatches

        =

        mkOption

        {

          type

            =

            types.listOf

            types.attrs;

          default

            =

            [ ];

          example

            =

            literalExpression

            "[ pkgs.kernelPatches.ubuntu_fan_4_4 ]";
          description = "A list of additional patches to apply to the kernel.";
        };

    };
}
