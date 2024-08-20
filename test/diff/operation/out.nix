[
  (
    # To find infinite recursion in NixOS option docs:
    # builtins.trace opt.loc
    [ docOption ] ++ optionals subOptionsVisible subOptions
  )
  (
    # Filter out git
    baseName == ".gitignore"
    || (type == "directory" && baseName == ".git")
    || (
      type == "directory"
      && (
        baseName == "target"
        || baseName == "_site"
        || baseName == ".sass-cache"
        || baseName == ".jekyll-metadata"
        || baseName == "build-artifacts"
      )
    )
    || (type == "symlink" && lib.hasPrefix "result" baseName)
    || (type == "directory" && (baseName == ".idea" || baseName == ".vscode"))
    || lib.hasSuffix ".iml" baseName
    # some other comment
    || baseName == "Cargo.nix"
    || lib.hasSuffix "~" baseName
    || builtins.match "^\\.sw[a-z]$$" baseName != null
    # a third comment
    || builtins.match "^\\..*\\.sw[a-z]$$" baseName != null
    || lib.hasSuffix ".tmp" baseName
    ||
      # fourth comment
      lib.hasSuffix ".bak" baseName
    ||
      # fifth comment
      baseName == "tests.nix"
    # comment on operator inside
    || baseName == "tests.nix"
    # comment absorbable term
    || { }
    # comment absorbable term 2
    || {
      foo = "bar"; # multiline
    }
    # comment on function application
    || foo bar baz
    # comment on function application 2
    || foo bar baz [
      1
      2
    ]
    # comment on other
    || foo ? bar
  )
  # Filter out nix-build result symlinks
  (type == "symlink" && lib.hasPrefix "result" baseName)
  (
    # Filter out nix-build result symlinks
    (type == "symlink" && lib.hasPrefix "result" baseName)
    # Filter out sockets and other types of files we can't have in the store.
    || (type == "unknown")
    ||
      # Filter out sockets and other types of files we can't have in the store.
      (type == "unknown")
    # Filter out sockets and other types of files we can't have in the store.
    || (type == "unknown")
  )
  (
    # Don't bother wrapping unless we actually have plugins, since the wrapper will stop automatic downloading
    # of plugins, which might be counterintuitive if someone just wants a vanilla Terraform.
    if actualPlugins == [ ] then
      terraform.overrideAttrs (orig: {
        passthru = orig.passthru // passthru;
      })
    else
      lib.appendToName "with-plugins" (
        stdenv.mkDerivation {
          inherit (terraform) meta pname version;
          nativeBuildInputs = [ makeWrapper ];
        }
      )
  )
  (
    if
      (cpu.family == "arm" && cpu.bits == 32)
      || (cpu.family == "sparc" && cpu.bits == 32)
      || (cpu.family == "m68k" && cpu.bits == 32)
      || (cpu.family == "x86" && cpu.bits == 32)
    then
      execFormats.aout
    else
      execFormats.elf
  )
  (
    [ aaaaaaaaaaaaa aaaaaaaaaaaaa ]
    + [ bbbbbbbbbbbbbb bbbbbbbbbbbbbbb ] * [ ccccccccccccccc ccccccccccccccccccc ]
  )
  (
    [ aaaaaaaaaaaaa aaaaaaaaaaaaa ] * [ bbbbbbbbbbbbbb bbbbbbbbbbbbbbb ]
    + [ ccccccccccccccc ccccccccccccccccccc ]
  )

  ([ 1 2 3 ] / [ 4 5 6 ] / [ 7 8 9 ])
  ([ 1 2 3 ] ++ [ 4 5 6 ] ++ [ 7 8 9 ])

  (
    [
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
    ++ (optionals condition [ more items ])
  )

  # Test precedence
  (
    aaaaaaaaaaaaaaa
    + bbbbbbbbbbbbbbbbbbbb
    + ccccccccccccccccccccccccccc
    + ddddddddddddddddddddddd * eeeeeeeeeeeeeeeeeeeeeeee
    +
      ffffffffffffffffffffffffff
      *
        gggggggggggggggggggggggg
        ++ hhhhhhhhhhhhhhhhhhhhhhhhhhh
        ++ iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      * jjjjjjjjjjjjjjjjjjjjj
  )

  # Logical precedence
  (
    assert pipewireSupport -> !waylandSupport || !webrtcSupport -> pipewireSupport;
    if
      aaaaaaaaaaaaaa && bbbbbbbbbbbb
      || cccccccccccccccccccc && ddddddddddddddddd
      || eeeeeeeeeeeeeeeeeeee && fffffffffffffffffffffffffff
    then
      [ ]
    else if
      aaaaaaaaaaaaaaaaaaaaa
      || bbbbbbbbbbbbbbbbbbb && cccccccccccccccccccccccccccccccc
      || ddddddddddddddddd && eeeeeeeeeeeeeeeeeeee
      || fffffffffffffffffffffffffff
    then
      [ ]
    else if
      aaaaaaaaaaaaaa && bbbbbbbbbbbb && aaaaaaaaaaaaaa && bbbbbbbbbbbb
      ||
        cccccccccccccccccccc
        && ddddddddddddddddd
        && cccccccccccccccccccc
        && ddddddddddddddddd
      ||
        eeeeeeeeeeeeeeeeeeee
        && fffffffffffffffffffffffffff
        && eeeeeeeeeeeeeeeeeeee
        && fffffffffffffffffffffffffff
    then
      [ ]
    else
      { }
  )

  # Indentation
  (
    [
      #multiline
      zip
      zlib
    ]
    ++ [
      (if (lib.versionAtLeast version "103") then nss_latest else nss_esr)
    ]
  )

  # Indentation with parenthesized multiline function call
  ([ 1 2 3 ] ++ (isOneOf item [ 1 2 3 4 ]) ++ isOneOf item [ 1 2 3 4 ])
  # Interaction with function calls
  (
    g {
      # multiline
      y = 20;
    }
    * f {
      # multiline
      x = 10;
    }
    +
      g {
        # multiline
        y = 20;
      }
      * h {
        # multiline
        z = 30;
      }
  )

  # Experimental pipe operators
  (
    a // b
    |> f "very long argument should justify splitting this over multiple lines"
    |> g { }
  )

  (
    g { }
    <| f "very long argument should justify splitting this over multiple lines"
    <| a // b
  )
]
