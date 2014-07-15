PROG = SnysServer

compile:
	ghc -O2 --make $(PROG) -threaded

clean:
	rm -f *.hi *.o *~ *# *.dyn_hi *.dyn_o
