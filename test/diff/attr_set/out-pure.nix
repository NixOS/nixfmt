[
  { }
  {
    # a
  }
  { a = 1; }
  { a = 1; }

  { }
  {

  }

  {
    a = {

    };
  }

  { b = 1; }
  {
    b = 1; # c
  }
  {
    # a
    b = 1;
  }
  {
    # a
    b = 1; # c
  }

  rec { c = 1; }
  rec {
    c = 1; # d
  }
  rec {
    # b
    c = 1;
  }
  rec {
    # b
    c = 1; # d
  }
  rec # a
  {
    c = 1;
  }
  rec # a
  {
    c = 1; # d
  }
  rec # a
  {
    # b
    c = 1;
  }
  rec # a
  {
    # b
    c = 1; # d
  }

  {
    a = rec {
      a = {
        a = rec {
          a = {
            a = rec {
              a = {
                a = rec {
                  a = {
                    a = rec {
                      a = { };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  }

  rec {

    c = 1;

    e = 1;

  }

  rec
  # a
  {

    # b

    c = 1;

    # d

    e = 1;

    # f

  }
  {
    x =
      {
        foo = 1;
        bar = 2;
        # multiline
      }
      .${x};
    y = # more multiline
      {
        foo = 1;
        bar = 2;
        # multiline
      }
      .${x};
    z =
      functionCall
        {
          # multi
          #line
        }
        [
          # several
          items
        ];
    a = [
      some
      flags # multiline
    ]
    ++ [ short ]
    ++ [
      more
      stuff # multiline
    ]
    ++ (if foo then [ bar ] else [ baz ])
    ++ [ ]
    ++ (optionals condition [
      more
      items
    ]);
    b = with pkgs; [
      a
      lot
      of
      packages
    ];
  }
  {
    systemd.initrdBi = lib.mkIf config.boot.initrd.services.lvm.enable [ pkgs.vdo ];
    systemd.initrdBin = lib.mkIf config.boot.initrd.services.lvm.enable [
      pkgs.vdo
    ];
    systemd.initrdBin_ = lib.mkIf config.boot.initrd.services.lvm.enable [
      pkgs.vdo
    ];
    systemd.initrdBin__ = lib.mkIf config.boot.initrd.services.lvm.enable [
      pkgs.vdo
    ];
    systemd.initrdBin___ = lib.mkIf config.boot.initrd.services.lvm.enable [
      pkgs.vdo
    ];
  }
  {
    patches = [
      (substituteAll {
        src = ./extensionOverridesPatches/vitals_at_corecoding.com.patch;
        gtop_path = "${libgtop}/lib/girepository-1.0";
      })
    ];
  }
  {
    programs.ssh.knownHosts =
      lib.mapAttrs (host_name: publicKey: {
        inherit publicKey;
        extraHostNames = [
          "${host_name}.m-0.eu"
          "${host_name}.vpn.m-0.eu"
          "${host_name}.lo.m-0.eu"
        ];
      }) secret-config.ssh-hosts
      // {
        foo = "bar";
      };
    programs.ssh.knownHosts2 =
      someStuff
      // lib.mapAttrs (host_name: publicKey: {
        inherit publicKey;
        extraHostNames = [
          "${host_name}.m-0.eu"
          "${host_name}.vpn.m-0.eu"
          "${host_name}.lo.m-0.eu"
        ];
      }) secret-config.ssh-hosts
      // {
        foo = "bar";
      };
    programs.ssh.knownHosts3 =
      lib.mapAttrs (
        host_name: publicKey: {
          inherit publicKey;
          extraHostNames = [
            "${host_name}.m-0.eu"
            "${host_name}.vpn.m-0.eu"
            "${host_name}.lo.m-0.eu"
          ];
        }
      )
      // {
        foo = "bar";
      };
    programs.ssh.knownHosts4 =
      someStuff
      // lib.mapAttrs (
        host_name: publicKey: {
          inherit publicKey;
          extraHostNames = [
            "${host_name}.m-0.eu"
            "${host_name}.vpn.m-0.eu"
            "${host_name}.lo.m-0.eu"
          ];
        }
      )
      // {
        foo = "bar";
      };
    programs.ssh.knownHosts5 =
      someStuff
      // lib.mapAttrs (
        host_name: publicKey: {
          inherit publicKey;
          extraHostNames = [
            "${host_name}.m-0.eu"
            "${host_name}.vpn.m-0.eu"
            "${host_name}.lo.m-0.eu"
          ];
        }
      );
    programs.ssh.knownHosts6 =
      someStuff
      // lib.mapAttrs (host_name: publicKey: {
        inherit publicKey;
        extraHostNames = [
          "${host_name}.m-0.eu"
          "${host_name}.vpn.m-0.eu"
          "${host_name}.lo.m-0.eu"
        ];
      }) secret-config.ssh-hosts;
    programs.ssh.knownHosts7 =
      someStuff # multiline
      // lib.mapAttrs (
        host_name: publicKey: {
          inherit publicKey;
          extraHostNames = [
            "${host_name}.m-0.eu"
            "${host_name}.vpn.m-0.eu"
            "${host_name}.lo.m-0.eu"
          ];
        }
      );
    programs.ssh.knownHosts8 =
      someStuff # multiline
      // lib.mapAttrs (host_name: publicKey: {
        inherit publicKey;
        extraHostNames = [
          "${host_name}.m-0.eu"
          "${host_name}.vpn.m-0.eu"
          "${host_name}.lo.m-0.eu"
        ];
      }) secret-config.ssh-hosts;
    programs.ssh.knownHosts9 = {
      multi = 1;
      line = 2;
    }
    // lib.mapAttrs (
      host_name: publicKey: {
        inherit publicKey;
        extraHostNames = [
          "${host_name}.m-0.eu"
          "${host_name}.vpn.m-0.eu"
          "${host_name}.lo.m-0.eu"
        ];
      }
    );
    programs.ssh.knownHosts10 = {
      multi = 1;
      line = 2;
    }
    // lib.mapAttrs (host_name: publicKey: {
      inherit publicKey;
      extraHostNames = [
        "${host_name}.m-0.eu"
        "${host_name}.vpn.m-0.eu"
        "${host_name}.lo.m-0.eu"
      ];
    }) secret-config.ssh-hosts;
  }

  # Parentheses
  {
    a = ({ });
    b = ([
      1
      2
      3
    ]);
    c = (if null then true else false);
    d = (
      let
      in
      [
        1
        2
        3
      ]
    );
    e = (
      if null then
        true
      else
        [
          1
          2
          3
        ]
    );
    # FIXME: This one exposes a really weird bug in the underlying
    # pretty printing engine.
    # (It's probably the same one that causes weird indentation in
    # functions with multiline function)
    # f = /* comment */ (if null then true else [ 1 2 3 ]);

    a = (with a; { });
    b = (
      with a;
      [
        1
        2
        3
      ]
    );
    c = (with a; if null then true else false);
    d = (
      with a;
      let
      in
      [
        1
        2
        3
      ]
    );
  }

  # Comments
  {
    fontsForXServer =
      config.fonts.fonts
      ++
      # We don't want these fonts in fonts.conf, because then modern,
      # fontconfig-based applications will get horrible bitmapped
      # Helvetica fonts.  It's better to get a substitution (like Nimbus
      # Sans) than that horror.  But we do need the Adobe fonts for some
      # old non-fontconfig applications.  (Possibly this could be done
      # better using a fontconfig rule.)
      [
        pkgs.xorg.fontadobe100dpi
        pkgs.xorg.fontadobe75dpi
      ];
  }
  # Regression https://github.com/NixOS/nixfmt/issues/253
  {
    foo1 = { };
    foo2 = bar { };
    foo3 = bar { } { };
    foo4 = [ ];
    foo5 = bar [ ];
    foo6 = bar [ ] [ ];
  }
  # https://github.com/NixOS/nixfmt/issues/228
  {
    foo =
      aaaaaaaaaaaaaaaaaaaaaaaaa
      + bbbbbbbbbbbbbbbbbbbbbbbbbbbbb
      + ccccccccccccccccccccccccccccc
      + ddddddddddddddddddddddddddddd;

    boot.kernelParams = [
      aaaaaaaaaaaaaa
    ]
    ++ optionals config.boot.vesa [
      "vga=0x317"
      "nomodeset"
    ];

    foo2 = [
      bar
    ]
    ++ baz # newline!
    ++ meow;

    foo3 =
      some function application that kinda is long # and
        multiline
      ++ [
        a
        list
      ];
    some.long.attribute # with a comment
      = [ stuff ] ++ more stuff;
    some.long.attribute1 # with a comment
      = [
        stuff
        a
        b
        c
      ]
      ++ more stuff;

    foo4 # nasty
      = # comments
      [
        bar
      ]
      ++ baz # newline!
      ++ meow;

    environment.systemPackages =
      # Include the PAM modules in the system path mostly for the manpages.
      [ package ]
      ++ lib.optional config.users.ldap.enable pam_ldap;

    environment.systemPackages2 =
      # Include the PAM modules in the system path mostly for the manpages.
      [ package ]
      ++ lib.optional config.users.ldap.enable pam_ldap
      ++ lib.optional config.services.kanidm.enablePam config.services.kanidm.package
      ++ lib.optional config.services.sssd.enable pkgs.sssd
      ++ lib.optionals config.security.pam.krb5.enable [
        pam_krb5
        pam_ccreds
      ];

    buildInputs1 = lib.optionals onePlatform [
      a
      b
      c
    ];

    buildInputs2 =
      lib.optionals onePlatform [
        a
        b
        c
      ]
      ++ lib.optionals anotherPlatform [
        d
        e
        f
      ];

    programs.ssh.knownHosts =
      someStuff functionArg
      // lib.mapAttrs (host_name: publicKey: {
        inherit publicKey;
        extraHostNames = [
          "${host_name}.m-0.eu"
          "${host_name}.vpn.m-0.eu"
          "${host_name}.lo.m-0.eu"
        ];
      }) secret-config.ssh-hosts
      // {
        foo = "bar";
      };

    programs.ssh.knownHosts2 =
      [ ] ++ (with expr; someStuff functionArg)
      // lib.mapAttrs (host_name: publicKey: {
        inherit publicKey;
        extraHostNames = [
          "${host_name}.m-0.eu"
          "${host_name}.vpn.m-0.eu"
          "${host_name}.lo.m-0.eu"
        ];
      }) secret-config.ssh-hosts
      // {
        foo = "bar";
      };
  }
]
