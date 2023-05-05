[
  ''
  foo
 bar
''
  ""
###
  "
  "
###
  "a
   ${x}
   b
  "
###
  ''''
###
  ''a''
###
  ''${""}''
###
  ''${""}

  ''
###
  ''a
      ''
###
  ''a

      ''
###
  ''  a
      ''
###

    ''a
  ''
###
              ''
        a
      ${""}
         b
        ${""}
         c ${""} d
         e
            ''
###
  ''
  ''
###
          ''
              declare -a makefiles=(./*.mak)
              sed -i -f ${makefile-sed} "''${makefiles[@]}"
              ### assign Makefile variables eagerly & change backticks to `$(shell …)`
              sed -i -e 's/ = `\([^`]\+\)`/ := $(shell \1)/' \
                -e 's/`\([^`]\+\)`/$(shell \1)/' \
                "''${makefiles[@]}"
            ''
###
        ''
                [${ mkSectionName sectName }]
        ''
###
''-couch_ini ${ cfg.package }/etc/default.ini ${ configFile } ${ pkgs.writeText "couchdb-extra.ini" cfg.extraConfig } ${ cfg.configFile }''
###
              ''exec i3-input -F "mark %s" -l 1 -P 'Mark: ' ''
###
              ''exec i3-input -F '[con_mark="%s"] focus' -l 1 -P 'Go to: ' ''
###
              ''"${ pkgs.name or "<unknown-name>" }";''
###
                  ''
      ${pkgs.replace-secret}/bin/replace-secret '${placeholder}' '${secretFile}' '${targetFile}' ''
###
''
    mkdir -p "$out/lib/modules/${ kernel.modDirVersion }/kernel/net/wireless/"
    ''
###
        ''        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
        <plist version="1.0">
        ${ expr "" v }
        </plist>''

  ''
    --${
    "test"
  }
  ''

  "--${
    "test"
  }"
]
