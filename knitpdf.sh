alias "ema"="emacs -nw"


function knitpdf () {
    # knit file in R
    R -e "library(knitr);knit('${1}.tex')"
    # create shell output
    echo "\begin{lstlisting}" > ${2}.tex
    ./${2}.sh >> ${2}.tex
    echo "\end{lstlisting}" >> ${2}.tex
    echo "\begin{lstlisting}" > ${3}.tex
    ./${3}.sh >> ${3}.tex
    echo "\end{lstlisting}" >> ${3}.tex
    # convert to pdf
    pdflatex --enable-pipes --shell-escape ${1}-out.tex
    rm -rf cache/ figure/
    rm -f *~ *.log *.aux
    rm -f ${1}-out.tex ${2}.tex ${3}.tex
}
