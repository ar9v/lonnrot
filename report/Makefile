latexcmd = latexmk -pdf -pdflatex="pdflatex -file-line-error --synctex=1 -interaction=nonstopmode -use-make report.tex"

report.pdf: report.tex chapters/**/*
	$(latexcmd)
	rail -h report
	$(latexcmd)

clean:
	latexmk -CA
