[
  (
    # Filter out git
    baseName == ".gitignore"
    || (type == "directory" && baseName == ".git")
    || (type == "directory"
      && (baseName == "target"
        || baseName == "_site"
        || baseName == ".sass-cache"
        || baseName == ".jekyll-metadata"
        || baseName == "build-artifacts"))
    || (type == "symlink" && lib.hasPrefix "result" baseName)
    || (type == "directory" && (baseName == ".idea" || baseName == ".vscode"))
    || lib.hasSuffix ".iml" baseName
    # some other comment
    || baseName == "Cargo.nix"
    || lib.hasSuffix "~" baseName
    || builtins.match "^\\.sw[a-z]$$" baseName != null
    || # a third comment
      builtins.match "^\\..*\\.sw[a-z]$$" baseName != null
    || lib.hasSuffix ".tmp" baseName
    ||
      # fourth comment
      lib.hasSuffix ".bak" baseName
    ||
      # fifth comment
      baseName == "tests.nix")
  (
    # Don't bother wrapping unless we actually have plugins, since the wrapper will stop automatic downloading
    # of plugins, which might be counterintuitive if someone just wants a vanilla Terraform.
    if actualPlugins == [ ] then
      terraform.overrideAttrs (orig: { passthru = orig.passthru // passthru; })
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
    [
      aaaaaaaaaaaaa
      aaaaaaaaaaaaa
    ]
    + [
      bbbbbbbbbbbbbb
      bbbbbbbbbbbbbbb
    ]
      * [
        ccccccccccccccc
        ccccccccccccccccccc
      ]
  )
  (
    [
      aaaaaaaaaaaaa
      aaaaaaaaaaaaa
    ]
    * [
      bbbbbbbbbbbbbb
      bbbbbbbbbbbbbbb
    ]
    + [
      ccccccccccccccc
      ccccccccccccccccccc
    ]
  )

  (
    [
      1
      2
      3
    ]
    / [
      4
      5
      6
    ]
    / [
      7
      8
      9
    ]
  )
  (
    [
      1
      2
      3
    ]
    ++ [
      4
      5
      6
    ]
    ++ [
      7
      8
      9
    ]
  )

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
    ++ (
      if foo then
        [ bar ]
      else
        [ baz ]
    )
    ++ [ ]
    ++ (optionals condition [
      more
      items
    ])
  )

  # Test precedence
  (aaaaaaaaaaaaaaa
    + bbbbbbbbbbbbbbbbbbbb
    + ccccccccccccccccccccccccccc
    + ddddddddddddddddddddddd * eeeeeeeeeeeeeeeeeeeeeeee
    + ffffffffffffffffffffffffff
      * gggggggggggggggggggggggg
        ++ hhhhhhhhhhhhhhhhhhhhhhhhhhh
        ++ iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      * jjjjjjjjjjjjjjjjjjjjj)

  # Logical precedence
  (
    assert pipewireSupport
      -> !waylandSupport || !webrtcSupport
      -> pipewireSupport;
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
      (
        if (lib.versionAtLeast version "103") then
          nss_latest
        else
          nss_esr
      )
    ]
  )

  # Indentation with parenthesized multiline function call
  (
    [
      1
      2
      3
    ]
    ++ (isOneOf item [
      1
      2
      3
      4
    ])
    ++ isOneOf item [
      1
      2
      3
      4
    ]
  )
]
