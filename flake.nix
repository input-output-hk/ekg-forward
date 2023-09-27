{
  description = "ekg-forward";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    CHaP.url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;

    # cicero
    tullia.url = "github:input-output-hk/tullia";

    # non-flake nix compatibility
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      #"x86_64-darwin"
      #"aarch64-linux"
      #"aarch64-darwin"
    ];
  in
    inputs.flake-utils.lib.eachSystem supportedSystems (
      system: let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = import inputs.nixpkgs {
          overlays =
            [inputs.haskellNix.overlay]
            ++ builtins.attrValues inputs.iohkNix.overlays;
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        inherit (nixpkgs) lib;

        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        cabalProject = nixpkgs.haskell-nix.cabalProject' {
          src = ./.;
          name = "ekg-forward";
          compiler-nix-name = "ghc8107";

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
          };

          # tools we want in our shell
          # TODO: removed tools to get building properly
          shell.tools = {
            cabal = "3.10.1.0";
            #ghcid = "0.8.8";
            #haskell-language-server = "latest";
            #hlint = {};
            #fourmolu = "0.10.1.0";
          };
          # Now we use pkgsBuildBuild, to make sure that even in the cross
          # compilation setting, we don't run into issues where we pick tools
          # for the target.
          #shell.nativeBuildInputs = with nixpkgs.pkgsBuildBuild; [
          #  (pkgs.python3.withPackages (ps: with ps; [sphinx sphinx_rtd_theme recommonmark sphinx-markdown-tables sphinxemoji]))
          #  haskellPackages.implicit-hie
          #];
          shell.withHoogle = false;

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
              # Use our forked libsodium from iohk-nix crypto overlay.
              packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [[pkgs.libsodium-vrf pkgs.secp256k1]];
              packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [[pkgs.libsodium-vrf]];
              packages.ekg-forward = {
                configureFlags = ["--ghc-option=-Werror"];
                components = {
                  tests.ekg-forward-golden = {
                    preCheck = ''
                      export CREATE_GOLDEN_FILES=1
                    '';
                  };
                };
              };
            })
          ];
        };
        # ... and construct a flake from the cabal project
        flake =
          cabalProject.flake (
            # we also want cross compilation to windows.
            lib.optionalAttrs (system == "x86_64-linux") {
              #crossPlatforms = p: [p.mingwW64];
            }
          )
          # add cicero logic.
          // (let
            actionCiInputName = "GitHub event";
          in
            inputs.tullia.fromSimple system {
              tasks = {
                ci = {
                  config,
                  lib,
                  ...
                }: {
                  preset = {
                    nix.enable = true;
                    github.ci = {
                      # Tullia tasks can run locally or on Cicero.
                      # When no facts are present we know that we are running locally and vice versa.
                      # When running locally, the current directory is already bind-mounted into the container,
                      # so we don't need to fetch the source from GitHub and we don't want to report a GitHub status.
                      enable = config.actionRun.facts != {};
                      repository = "input-output-hk/ekg-forward";
                      remote = config.preset.github.lib.readRepository actionCiInputName null;
                      revision = config.preset.github.lib.readRevision actionCiInputName null;
                    };
                  };

                  command.text = config.preset.github.status.lib.reportBulk {
                    bulk.text = ''
                      nix eval .#hydraJobs --apply __attrNames --json |
                      nix-systems -i |
                      jq 'with_entries(select(.value))' # filter out systems that we cannot build for
                    '';
                    each.text = ''nix build -L .#hydraJobs."$1".required'';
                    skippedDescription = lib.escapeShellArg "No nix builder for this system";
                  };

                  memory = 1024 * 8;
                  nomad.driver = "exec";
                  nomad.resources.cpu = 10000;
                };
              };

              actions = {
                "ekg-forward/ci" = {
                  task = "ci";
                  io = ''
                    // This is a CUE expression that defines what events trigger a new run of this action.
                    // There is no documentation for this yet. Ask SRE if you have trouble changing this.
                    let github = {
                      #input: "${actionCiInputName}"
                      #repo: "input-output-hk/ekg-forward"
                    }

                    #lib.merge
                    #ios: [
                      {#lib.io.github_push, github, #default_branch: true},
                      {#lib.io.github_pr,   github},
                    ]
                  '';
                };
              };
            });
      in
        nixpkgs.lib.recursiveUpdate flake rec {
          # add a required job, that's basically all hydraJobs.
          hydraJobs =
            nixpkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs = flake.hydraJobs;
            };
          legacyPackages = rec {
            inherit cabalProject;
            # also provide hydraJobs through legacyPackages to allow building without system prefix:
            inherit hydraJobs;
          };
          # `nix develop .#profiling`: a shell with profiling enabled
          devShells.profiling = (cabalProject.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
          # formatter used by nix fmt
          formatter = nixpkgs.alejandra;
        }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
