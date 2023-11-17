[
  "${
  # a
  "${
  # b
  "${c}"}" # d
  }"
  {
    ExecStart = "${pkgs.openarena}/bin/oa_ded +set fs_basepath ${pkgs.openarena}/openarena-0.8.8 +set fs_homepath /var/lib/openarena ${
        concatMapStringsSep (x: x) " " cfg.extraFlags
      }";
    description = "${
        optionDescriptionPhrase (class: class == "noun" || class == "conjunction") t1
      } or ${
        optionDescriptionPhrase
          (class: class == "noun" || class == "conjunction" || class == "composite")
          t2
      }";
    ruleset = ''
      table ip nat {
        chain port_redirects {
          type nat hook prerouting priority dstnat
          policy accept

          ${
            builtins.concatStringsSep "\n" (
              map
                (
                  e:
                  ''
                    iifname "${cfg.upstreamIface}" tcp dport ${builtins.toString e.sourcePort} dnat to ${e.destination}''
                )
                tcpPortMap
            )
          }

          ${
            builtins.concatStringsSep "\n" (
              map
                (
                  e:
                  ''
                    ifname "${cfg.upstreamIface}" udp dport ${builtins.toString e.sourcePort} dnat to ${e.destination}''
                )
                udpPortMap
            )
          }
        }
    '';
  }
]
