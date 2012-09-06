function knitpdf () {
    R -e "library(knitr);knit('${1}.tex')"
    pdflatex ${1}-out.tex
    rm -rf cache/ figure/
    rm -f *~ *.log *.aux
}
