alias "ema"="emacs -nw"
alias "ls"="ls -l"

function knitpdf () {
    # knit file in R
    R -e "library(knitr);knit('${1}.tex')"

    # convert to pdf
    pdflatex --enable-pipes --shell-escape ${1}-out.tex
#    rm -rf cache/ figure/
#    rm -f *~ *.log *.aux
#    rm -f ${1}-out.tex
}
