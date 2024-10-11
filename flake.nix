{
  description = "PQ-Lang development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfreePredicate = pkg: builtins.elem (pkg.pname or (builtins.parseDrvName pkg.name).name)
              [ "vscode" "vscode-with-extensions" "vscode-extension-github-copilot" "vscode-extension-github-copilot-chat" ];
          };
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # VSCode + extensions
            (vscode-with-extensions.override {
              vscodeExtensions = with vscode-extensions; [
                ocamllabs.ocaml-platform
                vscodevim.vim
                github.vscode-pull-request-github
                github.vscode-github-actions
                github.copilot
                github.copilot-chat
                eamodio.gitlens
              ];
            })

            # Required packages
            git
            ocaml
            opam
            ocamlPackages.dune_3
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
            ocamlPackages.ounit
            ocamlPackages.qcheck
            ocamlPackages.qcheck-ounit
            
            # Development tools
            neovim
          ];

          # Shell hook for additional environment setup
          shellHook = ''
            export PS1="\[\033[1;36m\](pq-lang)\[\033[0m\] \[\033[1;34m\]\w\[\033[0m\]$ "
            echo "PQ-Lang development environment activated!"
            echo "opam version: $(opam --version)"
          '';
        };
      });
}
