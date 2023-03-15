all: clean minic

minic:
	dune build src/minic.exe
	mv src/minic.exe minic

clean:
	dune clean
	rm -f minic

.PHONY: all clean minic