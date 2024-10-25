{
  description = ''
    Cardano-node-pparams-api for an easy api interface to cardano network
    protocol parameters.
  '';

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    incl.url = "github:divnix/incl";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"

      # Disabling to reduce CI time initially; uncomment later.
      # "x86_64-darwin"
      # "aarch64-linux"
      # "aarch64-darwin"
    ];
  in
    {inherit (inputs) incl;}
    // inputs.flake-utils.lib.eachSystem supportedSystems (
      system: let
        # Setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays.
        nixpkgs = import inputs.nixpkgs {
          overlays = [
            # iohkNix.overlays.crypto provide libsodium-vrf, libblst and libsecp256k1.
            inputs.iohkNix.overlays.crypto

            # HaskellNix.overlay can be configured by later overlays, so need to come before them.
            inputs.haskellNix.overlay

            # Configure haskell.nix to use iohk-nix crypto libraries.
            inputs.iohkNix.overlays.haskell-nix-crypto
          ];
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        inherit (nixpkgs) lib;

        # see flake `variants` below for alternative compilers
        defaultCompiler = "ghc966";

        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        cabalProject = nixpkgs.haskell-nix.cabalProject' ({config, ...}: {
          src = ./.;
          name = "cardano-node-pparams-api";
          compiler-nix-name = lib.mkDefault defaultCompiler;

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with:
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };

          # Tools we want in our shell; from hackage.
          shell.tools =
            {
              cabal = "3.10.3.0";
              cabal-gild = "1.5.0.1";
              ghcid = "0.8.9";
            }
            // lib.optionalAttrs (config.compiler-nix-name == defaultCompiler) {
              # Tools that work only with default compiler
              fourmolu = "0.14.0.0";
              haskell-language-server.src = nixpkgs.haskell-nix.sources."hls-2.9";
              hlint = "3.8";
              stylish-haskell = "0.14.6.0";
            };
          # And from nixpkgs or other inputs.
          shell.nativeBuildInputs = with nixpkgs; [gh jq yq-go];

          # Disable Hoogle until someone requests it.
          shell.withHoogle = false;

          # Skip cross compilers for the shell.
          shell.crossPlatforms = _: [];

          # Package customizations as needed, ex: where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
              packages.cardano-node-pparams-api.configureFlags = ["--ghc-option=-Werror"];
              # packages.cardano-node-pparams-api.components.tests.cardano-node-pparams-api-test.build-tools = with pkgs.buildPackages; [jq coreutils shellcheck];
              # packages.cardano-node-pparams-api.components.tests.cardano-node-pparams-api-golden.build-tools = with pkgs.buildPackages; [jq coreutils shellcheck];
            })
            # ({
            #   pkgs,
            #   config,
            #   ...
            # }: let
            #   exportCliPath = "export CARDANO_CLI=${config.hsPkgs.cardano-node-pparams-api.components.exes.cardano-node-pparams-api}/bin/cardano-node-pparams-api${pkgs.stdenv.hostPlatform.extensions.executable}";
            #   mainnetConfigFiles = [
            #     "configuration/cardano/mainnet-config.yaml"
            #     "configuration/cardano/mainnet-config.json"
            #     "configuration/cardano/mainnet-byron-genesis.json"
            #     "configuration/cardano/mainnet-shelley-genesis.json"
            #     "configuration/cardano/mainnet-alonzo-genesis.json"
            #     "configuration/cardano/mainnet-conway-genesis.json"
            #   ];
            # in {
            #   # cardano-node-pparams-api tests depend on cardano-node-pparams-api and some config files:
            #   packages.cardano-node-pparams-api.components.tests.cardano-node-pparams-api-golden.preCheck = let
            #     # This defines files included in the directory that will be passed to `H.getProjectBase` for this test:
            #     filteredProjectBase = inputs.incl ./. [
            #       "cabal.project"
            #       "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"
            #     ];
            #   in ''
            #     ${exportCliPath}
            #     cp -r ${filteredProjectBase}/* ..
            #   '';
            #   packages.cardano-node-pparams-api.components.tests.cardano-node-pparams-api-test.preCheck = let
            #     # This defines files included in the directory that will be passed to `H.getProjectBase` for this test:
            #     filteredProjectBase = inputs.incl ./. mainnetConfigFiles;
            #   in ''
            #     ${exportCliPath}
            #     cp -r ${filteredProjectBase}/* ..
            #   '';
            # })
            # {
            #   packages.crypton-x509-system.postPatch = ''
            #     substituteInPlace crypton-x509-system.cabal --replace 'Crypt32' 'crypt32'
            #   '';
            # }
          ];
        });

        # Construct a flake from the cabal project
        flake = cabalProject.flake (
          lib.optionalAttrs (system == "x86_64-linux") {
            # On linux, build/test other supported compilers
            # variants = lib.genAttrs ["ghc8107"] (compiler-nix-name: {
            #   inherit compiler-nix-name;
            # });
          }
        );
      in
        lib.recursiveUpdate flake rec {
          project = cabalProject;

          # Add a required job that's basically all hydraJobs.
          hydraJobs =
            nixpkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs =
                flake.hydraJobs
                // {
                  # This ensure hydra send a status for the required job even
                  # if there is no change other than commit hash.
                  revision = nixpkgs.writeText "revision" (inputs.self.rev or "dirty");
                };
            };

          legacyPackages = {
            inherit cabalProject nixpkgs;

            # Also provide hydraJobs through legacyPackages to allow building without a system prefix.
            inherit hydraJobs;
          };

          packages = rec {
            inherit (cabalProject.hsPkgs.cardano-node-pparams-api.components.exes) cardano-node-pparams-api;
            default = cardano-node-pparams-api;
          };

          devShells = let
            profillingShell = p: {
              # `nix develop .#profiling` or `.#ghc927.profiling` yields a shell with profiling enabled
              profiling = (p.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
            };
          in
            profillingShell cabalProject;

          # The nix formatter used by nix fmt
          formatter = nixpkgs.alejandra;
        }
    );

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = true;
  };
}
