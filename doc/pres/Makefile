all: main.fr.pdf main.en.pdf

dfa.%.pdf: dfa.%.odg
	echo Update $@ from $^ using libreoffice
	exit 1

main.fr.pdf: main.fr.tex dfa.fr.pdf
	tectonic -k $<

main.en.pdf: main.en.tex dfa.en.pdf
	tectonic -k $<

clean:
	rm -f main.*.{aux,nav,out,toc,vrb}

distclean:
	$(MAKE) clean
	rm -f main.fr.pdf main.en.pdf

#  TeXlive / latexmk workflow
# main.fr.pdf: main.fr.tex dfa.fr.pdf
# 	#latexmk -xelatex main.fr.tex
# 
# main.en.pdf: main.en.tex dfa.en.pdf
# 	#latexmk -xelatex main.en.tex
# 
# clean:
# 	latexmk -c
# 
# distclean:
# 	latexmk -C
# 	rm -f *.snm *.vrb *.nav

.PHONY: all clean distclean
