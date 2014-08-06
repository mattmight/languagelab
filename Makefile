
.PHONY: all
all: bin
	scalac -d bin src/*.scala

bin:
	mkdir bin

.PHONY: test
test: bin/Test.class 
	scala -cp bin Test


.PHONY: clean
clean:
	rm -rf bin 
