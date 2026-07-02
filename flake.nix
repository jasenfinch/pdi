{
  description = "R development environment with tidyverse";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};

    R-packages = with pkgs.rPackages; [
      # add R packages here
      covr
      devtools
      DT
      tidyverse
      knitr
      randomForest
      readxl
      rmarkdown
      usethis
    ];

    r = pkgs.rWrapper.override {
      packages = R-packages;
    };

    radian = pkgs.radianWrapper.override {
      packages = R-packages;
    };
  in {
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        r
        radian
        pandoc
      ];
    };
  };
}
