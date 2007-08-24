# Makefile for creating vars.pdf
#
TARGET = vars
SRC = vars.Rnw
EXT = ${TARGET}.aux ${TARGET}.bbl ${TARGET}.blg ${TARGET}.dvi ${TARGET}.log ${TARGET}.toc ${TARGET}.tex ${TARGET}.lof ${TARGET}.lot graph*.eps graph*.pdf Rplots.ps
#
# Rules
#
pdf: ${TARGET}.pdf
dvi: ${TARGET}.dvi
tex: ${TARGET}.tex
#
${TARGET}.pdf: ${TARGET}.dvi
	dvips ${TARGET}.dvi
	ps2pdf ${TARGET}.ps
	rm ${EXT}
#
${TARGET}.dvi: ${TARGET}.tex
	latex ${TARGET}
	latex ${TARGET}
	bibtex ${TARGET}
	latex ${TARGET}
	latex ${TARGET}
#
${TARGET}.tex: ${SRC}
	echo "Sweave(\"${SRC}\")" | R --no-save


