SOURCES = Scheme/Data.hs \
	  Scheme/Eval.hs \
	  Scheme/Parser.hs \
	  Main.hs

all:
	ghc -Wall -package parsec -o main $(SOURCES)

clean:
	rm *.o *.hi Scheme/*.o Scheme/*.hi main
