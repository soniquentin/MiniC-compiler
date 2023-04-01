all: clean minic

minic:
	dune build minic.exe
	mv minic.exe minic

clean:
	dune clean
	rm -f minic

.PHONY: all clean minic