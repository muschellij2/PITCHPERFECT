# date=2014Nov18
name=CT_ICH_Segmentation
${name}.pdf: ${name}.Rnw ${name}.tex \
	Reseg_Dice_Comparison.png \
	Reseg_Volume_Comparison.png \
	CT_Skull_Stripping_Bib.bib \
	extra_bibs_addon.bib \
	CT_ICH_Segmentation.bib
	if [ -e ${name}.aux ]; \
	then \
	rm ${name}.aux; \
	fi;
	Rscript -e "library(knitr); knit('${name}.Rnw'); purl('${name}.Rnw')"
	pdflatex ${name}
	bibtex ${name}
	bibtex ${name}
	if [ -e ${name}1-blx.bib ]; \
	then \
	bibtex ${name}1-blx; \
	fi;
	if [ -e ${name}2-blx.bib ]; \
	then \
	bibtex ${name}2-blx; \
	fi;		
	pdflatex ${name}
	pdflatex ${name}
	open ${name}.pdf

clean: 
	rm ${name}.pdf
open: 
	open ${name}.pdf
adobe:
	adobe ${name}.pdf
#http://texblog.org/2012/10/22/multiple-bibliographies-with-biblatex/
#http://tex.stackexchange.com/questions/128196/problem-with-refsection

# code.tex: bsc-code.tex
#	sed -e '/\(Schunk\|Sinput\)/d' \
#		-e 's/\\\(begin\|end\){Soutput}/~~~~/; s/^> /    /' \
#		 bsc-code.tex | pandoc -w context > code.tex
	
