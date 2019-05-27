BIN=bin

all: $(BIN)/main
.PHONY: all run clean

run: main.lisp
	ros --load $< --eval "(main)"

$(BIN)/%: %.lisp | $(BIN)
	ros --load $< --eval "(sb-ext:save-lisp-and-die \"$@\" :toplevel #'$(basename $<) :executable t)"  # Assumes SBCL

$(BIN):
	mkdir $@

clean:
	$(RM) -r $(BIN)
