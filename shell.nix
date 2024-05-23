{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    (import ./default.nix { inherit pkgs; }).app.buildInputs
    ripgrep
    tree
    nixfmt-rfc-style
    nurl
    emacs-nox
    emacsPackages.magit
    emacsPackages.whitespace-cleanup-mode
    emacsPackages.nix-mode
    emacsPackages.modus-themes
    emacsPackages.smartparens
    emacsPackages.paredit
    emacsPackages.multiple-cursors
    emacsPackages.evil
    emacsPackages.dockerfile-mode
    emacsPackages.slime
    emacsPackages.crux
  ];
  shellHook = ''
    local dir="$(pwd)/.emacs.d"
    alias emacs="emacs --init-directory=$dir"
  '';
}
