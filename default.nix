let
  # ./updater versions.json reflex-platform
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  reflex-platform-src = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).reflex-platform;
  reflex-platform = import reflex-platform-src { system = builtins.currentSystem; };
  overrides = self: super: {
    tapl = reflex-platform.nixpkgs.haskell.lib.dontHaddock super.tapl;
  };
in reflex-platform.project ({ pkgs, ... }: {
  inherit overrides;
  useWarp = true;
  withHoogle = false;
  packages = {
    tapl = ./.;
  };
  shells = {
    ghc = ["tapl"];
    ghcjs = ["tapl"];
  };
})
