../assets/runmain.js: ../src/Api.hs src/exe/Main.hs src/Client.hs src/SameOrigin.hs package.yaml stack.yaml src/Client.hs src/exe/Main.hs src/Primitive.hs src/Primitive/Types.hs src/SameOrigin.hs test/ClientSpec.hs test/ClientTests.hs test/exe/Main.hs
	stack build --fast
	cp -rv .stack-work/install/x86_64-linux/lts-5.12/ghcjs-0.2.0_ghc-7.10.3/bin/crdt-chat-client.jsexe/*.js ../assets/
