P=net/might/matt/languages

.PHONY: all
all: test

bin:
	mkdir bin

bin/$(P)/sexp/SExp.class: bin src/SExp.scala
	scalac -d bin src/SExp.scala

bin/$(P)/reg/RegProg.class: src/RegProg.scala bin/$(P)/sexp/SExp.class
	scalac -cp bin -d bin src/RegProg.scala

bin/$(P)/cps/CPSProg.class: src/CPSProg.scala bin/$(P)/sexp/SExp.class
	scalac -cp bin -d bin src/CPSProg.scala

bin/$(P)/exp/Exp.class: src/Exp.scala bin/$(P)/sexp/SExp.class
	scalac -cp bin -d bin src/Exp.scala

bin/Test.class: src/Test.scala bin/$(P)/sexp/SExp.class bin/$(P)/reg/RegProg.class bin/$(P)/cps/CPSProg.class
	scalac -feature -cp bin -d bin src/Test.scala

.PHONY: test
test: bin/Test.class 
	scala -cp bin Test


.PHONY: clean
clean:
	rm -rf bin 
