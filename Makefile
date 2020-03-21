all:
	ghc src/dka-2-mka.hs -o dka-2-mka

clean:
	rm dka-2-mka src/*.hi src/*.o