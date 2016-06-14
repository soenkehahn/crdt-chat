../assets/runmain.js: ../src/Api.hs src/Main.hs package.yaml stack.yaml
	stack build
	cp -rv .stack-work/install/x86_64-linux/lts-5.12/ghcjs-0.2.0_ghc-7.10.3/bin/patches-client.jsexe/*.js ../assets/
