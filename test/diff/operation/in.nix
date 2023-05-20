[
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
  ([ aaaaaaaaaaaaa aaaaaaaaaaaaa ] + [ bbbbbbbbbbbbbb bbbbbbbbbbbbbbb ] * [ ccccccccccccccc ccccccccccccccccccc ])
  ([ aaaaaaaaaaaaa aaaaaaaaaaaaa ] * [ bbbbbbbbbbbbbb bbbbbbbbbbbbbbb ] + [ ccccccccccccccc ccccccccccccccccccc ])

  ([ 1 2 3] / [4 5 6] / [7 8 9])
  ([ 1 2 3] ++ [4 5 6] ++ [7 8 9])

  ([
      some flags # multiline
    ] ++ [ short ] ++ [
      more stuff # multiline
    ] ++ (if foo then [ bar ] else [baz ]) ++ [] ++
    (optionals condition [more items])
  )

  # Test precedence
  (aaaaaaaaaaaaaaa + bbbbbbbbbbbbbbbbbbbb
  + ccccccccccccccccccccccccccc + ddddddddddddddddddddddd
  * eeeeeeeeeeeeeeeeeeeeeeee + ffffffffffffffffffffffffff
  * gggggggggggggggggggggggg ++ hhhhhhhhhhhhhhhhhhhhhhhhhhh
  ++ iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii * jjjjjjjjjjjjjjjjjjjjj)

  # Logical precedence
  (assert pipewireSupport -> !waylandSupport || !webrtcSupport -> pipewireSupport;
  if aaaaaaaaaaaaaa && bbbbbbbbbbbb
  || cccccccccccccccccccc && ddddddddddddddddd
  || eeeeeeeeeeeeeeeeeeee && fffffffffffffffffffffffffff
  then [] else
  if aaaaaaaaaaaaaaaaaaaaa || bbbbbbbbbbbbbbbbbbb
  && cccccccccccccccccccccccccccccccc || ddddddddddddddddd
  && eeeeeeeeeeeeeeeeeeee || fffffffffffffffffffffffffff
  then [] else
  {}
  )

  # Indentation
  (
    [
      #multiline
      zip
      zlib
    ]
    ++ [
      (if (lib.versionAtLeast version "103") then
        nss_latest
      else
        nss_esr)
    ]
  )

]
