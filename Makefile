%.ast:
	agda2lambox -o build test/$*.agda

%.wasm: %.ast
	lbox wasm -o demo/$*.wasm build/$*.ast

%.v:
	agda2lambox -o build --rocq test/$*.agda

%.typed:
	agda2lambox -o build --typed test/$*.agda
