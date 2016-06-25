all:
	ghc Parser.hs Evaluator.hs Main.hs -o run

clean:
	rm *.o *.hi run
