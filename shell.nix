with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "sql-server";
  ghc = haskell.packages.ghc7103.ghc;
  buildInputs = [ ncurses # For intero
                  git # To enable git packages in stack.yaml
                  cabal-install
	          zlib  ]; 
} 
