{ pkgs }:

{
  packages.render-server.components.exes.render-server.postInstall = ''
    ${builtins.readFile ./fix-dylib.sh}

    fix_dylib "$out/bin/render-server" libiconv.2.dylib libiconv.dylib
    fix_dylib "$out/bin/render-server" libffi.8.dylib libffi.dylib
    fix_dylib "$out/bin/render-server" libpcre.1.dylib libpcre.dylib
    fix_dylib "$out/bin/render-server" libc++.1.0.dylib libc++.dylib
    fix_dylib "$out/bin/render-server" libc++abi.1.0.dylib libc++abi.dylib
    check_no_nix_refs "$out/bin/render-server"

    strip "$out/bin/render-server"
  '';

  packages.render-server.components.exes.render-server.configureFlags = let
    # Nixpkgs can't currently give us a cross-compiled x86_64-darwin libffi.a when we're building on aarch64-darwin.
    # So, we bundle one in the repo.
    # Tried to also detect if we're on aarch64-darwin, so it can work normally if the build machine is x86_64-darwin,
    # but that is deliberately difficult here (builtins.currentSystem is considered an "impure builtin".)
    libffi = if pkgs.stdenv.targetPlatform.system == "x86_64-darwin"
             then "${../assets/libffi.a}"
             else "${pkgs.pkgsStatic.libffi}/lib/libffi.a";
  in
    [
      ''--ghc-options="-optl-Wl,-dead_strip -optl-Wl,-dead_strip_dylibs -optl-Wl,-force_load,${libffi}"''
    ];
}
