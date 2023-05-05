{
# foo
stdenv
# foo
,
# foo
lib
# foo
,
# foo
fetchFromGitLab
# foo
,
# foo
cairo
# foo
,
# foo
desktop-file-utils
# foo
,
# foo
gettext
# foo
,
# foo
glib
# foo
,
# foo
gtk4
# foo
,
# foo
libadwaita
# foo
,
# foo
meson
# foo
,
# foo
ninja
# foo
,
# foo
pango
# foo
,
# foo
pkg-config
# foo
,
# foo
python3
# foo
,
# foo
rustPlatform
# foo
,
# foo
wrapGAppsHook4
# foo
}:
# foo
stdenv.mkDerivation
# foo
rec
# foo
{
  # foo
  pname
  # foo
    =
    # foo
    "contrast";
  # foo
  version
  # foo
    =
    # foo
    "0.0.5";
  # foo
  src
  # foo
    =
    # foo
    fetchFromGitLab
    # foo
    {
      # foo
      domain
      # foo
        =
        # foo
        "gitlab.gnome.org";
      # foo
      group
      # foo
        =
        # foo
        "World";
      # foo
      owner
      # foo
        =
        # foo
        "design";
      # foo
      repo
      # foo
        =
        # foo
        "contrast";
      # foo
      rev
      # foo
        =
        # foo
        version;
      # foo
      sha256
      # foo
        =
        # foo
        "cypSbqLwSmauOoWOuppWpF3hvrxiqmkLspxAWzvlUC0=";
      # foo
    };
  # foo
  cargoDeps
  # foo
    =
    # foo
    rustPlatform.fetchCargoTarball
    # foo
    {
      # foo
      inherit
      # foo
        src;
      # foo
      name
      # foo
        =
        # foo
        "${pname}-${version}";
      # foo
      hash
      # foo
        =
        # foo
        "sha256-W4FyqwJpimf0isQRCq9TegpTQPQfsumx40AFQCFG5VQ=";
      # foo
    };
  # foo
  nativeBuildInputs
  # foo
    =
    # foo
    [
      # foo
      desktop-file-utils
      # foo
      gettext
      # foo
      meson
      # foo
      ninja
      # foo
      pkg-config
      # foo
      python3
      # foo
      rustPlatform.rust.cargo
      # foo
      rustPlatform.cargoSetupHook
      # foo
      rustPlatform.rust.rustc
      # foo
      wrapGAppsHook4
      # foo
      glib
      # foo
      # for glib-compile-resources

      # foo
    ];
  # foo
  buildInputs
  # foo
    =
    # foo
    [
      # foo
      cairo
      # foo
      glib
      # foo
      gtk4
      # foo
      libadwaita
      # foo
      pango
      # foo
    ];
  # foo
  postPatch
  # foo
    =
    # foo
    ''
      patchShebangs build-aux/meson_post_install.py
      # https://gitlab.gnome.org/World/design/contrast/-/merge_requests/23
      substituteInPlace build-aux/meson_post_install.py \
        --replace "gtk-update-icon-cache" "gtk4-update-icon-cache"
    '';
  # foo
  meta
  # foo
    =
    # foo
    with
    # foo
      lib;
    # foo
    {
      # foo
      description
      # foo
        =
        # foo
        "Checks whether the contrast between two colors meet the WCAG requirements";
      # foo
      homepage
      # foo
        =
        # foo
        "https://gitlab.gnome.org/World/design/contrast";
      # foo
      license
      # foo
        =
        # foo
        licenses.gpl3Plus;
      # foo
      maintainers
      # foo
        =
        # foo
        with
        # foo
          maintainers;
        # foo
        [
          # foo
          jtojnar
          # foo
        ];
      # foo
      platforms
      # foo
        =
        # foo
        platforms.unix;
      # foo
    };
  # foo
}
