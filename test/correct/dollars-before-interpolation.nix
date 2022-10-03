let bar = "bar";

in [
  "$$\${bar}"
  "$\$${bar}"
  ''
    $$''${bar}
    $''$${bar}
  ''
]
