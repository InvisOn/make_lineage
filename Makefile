ALL := output.dot output.pdf

all: output.pdf

output.pdf: output.dot
	cat $< | dot -Tpdf -o $@

output.dot: Makefile ./src/main.rs
	LANG=C make -p | makevizzy o> output.dot

clean:
	rm -f $(ALL)
