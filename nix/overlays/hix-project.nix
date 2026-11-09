{ compiler-nix-name
, gitignore
, system
}:

final: prev: {
  hixProject = compiler-nix-name:
    final.haskell-nix.hix.project {
      src = gitignore.lib.gitignoreSource ../../.;
      evalSystem = system;
      inherit compiler-nix-name;

      modules = [{
        packages.unix.components.library.configureFlags = [''-f os-string''];
        packages.directory.components.library.configureFlags = [''-f os-string''];
        packages.file-io.components.library.configureFlags = [''-f os-string''];
        packages.process.components.library.configureFlags = [''-f os-string''];

        packages.render-server.components.exes.render-server.dontStrip = false;
      } (
        prev.lib.optionalAttrs prev.stdenv.isDarwin (import ../macos-modules.nix { pkgs = prev; })
      )];
    };
}
