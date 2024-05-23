{
  pkgs ? import <nixpkgs> { },
}:

let
  inherit (pkgs) callPackage;
  document-templates = callPackage ./document-templates.nix { };
  sbcl' = pkgs.sbcl.withPackages (ps: with ps; [ document-templates ]);
  app = pkgs.stdenv.mkDerivation {
    pname = "document-templates-app";
    version = "0.0.17";
    src =
      let
        patterns = ''
          *
          !.sbcl-disable-debugger.lisp
        '';
      in
      pkgs.nix-gitignore.gitignoreSourcePure patterns ./.;

    buildInputs = [ sbcl' ];

    dontStrip = true;

    buildPhase = ''
      ${sbcl'}/bin/sbcl --no-userinit --non-interactive \
        --load .sbcl-disable-debugger.lisp \
        --eval '(load (sb-ext:posix-getenv "ASDF"))' \
        --eval '(push :standalone *features*)' \
        --eval '(asdf:load-system :document-templates)' \
        --eval '(document-templates::dump)'
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp document-templates $out/bin
    '';
  };
in
{
  inherit app;
}
