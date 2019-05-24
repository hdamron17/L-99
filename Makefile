BIN=bin

all: $(BIN)/main

run: main.lisp
	sbcl --quit --load $< --eval "(main)"

$(BIN)/%: %.lisp | $(BIN)
	sbcl --non-interactive --load $< --eval "(sb-ext:save-lisp-and-die \"$@\" :toplevel #'$(basename $<) :executable t)"

$(BIN):
	mkdir $@

clean:
	$(RM) -r $(BIN)
