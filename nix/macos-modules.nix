{ pkgs }:

{
  packages.lsp-export-server.components.exes.lsp-export-server.postInstall = ''
    ${builtins.readFile ./fix-dylib.sh}

    fix_dylib "$out/bin/lsp-export-server" libiconv.2.dylib libiconv.dylib
    fix_dylib "$out/bin/lsp-export-server" libffi.8.dylib libffi.dylib
    fix_dylib "$out/bin/lsp-export-server" libpcre.1.dylib libpcre.dylib
    fix_dylib "$out/bin/lsp-export-server" libc++.1.0.dylib libc++.dylib
    fix_dylib "$out/bin/lsp-export-server" libc++abi.1.0.dylib libc++abi.dylib
    fix_dylib "$out/bin/lsp-export-server" libz.dylib libz.1.dylib
    check_no_nix_refs "$out/bin/lsp-export-server"

    strip "$out/bin/lsp-export-server"
  '';

  packages.lsp-export-server.components.exes.lsp-export-server.configureFlags = let
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
