all: dfa.pdf
	latexmk -xelatex

dfa.pdf: dfa.odg
	echo "Update dfa.pdf from dfa.odg using libreoffice"
	exit 1

clean:
	latexmk -c

.PHONY: all clean
