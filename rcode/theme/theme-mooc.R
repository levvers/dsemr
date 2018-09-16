# Get knitr syntax highlighting to use black background and University colours
#thm <- list(highlight="\\definecolor{fgcolor}{rgb}{1,1,1}\n\\newcommand{\\hlnum}[1]{\\textcolor{white}{#1}}%\n\\newcommand{\\hlstr}[1]{\\textcolor{uogpumpkin}{#1}}%\n\\newcommand{\\hlcom}[1]{\\textcolor{uogleaf!75}{#1}}%\n\\newcommand{\\hlopt}[1]{\\textcolor[rgb]{1,1,1}{#1}}%\n\\newcommand{\\hlstd}[1]{\\textcolor{white}{#1}}%\n\\newcommand{\\hlkwa}[1]{\\textcolor{uogsky!75}{#1}}%\n\\newcommand{\\hlkwb}[1]{\\textcolor{uogthistle!75}{#1}}%\n\\newcommand{\\hlkwc}[1]{\\textcolor{uogcobalt!75}{#1}}%\n\\newcommand{\\hlkwd}[1]{\\textcolor{uogcobalt!75}{#1}}%\n\\let\\hlipl\\hlkwb", background="#000000", foreground="#ffffff")
#knitr::knit_theme$set(thm)
#library(extrafont)

knitr::opts_chunk$set(out.width=".45\\textwidth",dev.args=list(pointsize=12))
flcolours <- list(
    flpink = "#fe40ed",
    flyellow = "#fee658",
    flpurple= "#850acc",
    flgrapefruit = "#fd956f",
    flblue = "#626aff"
)

flcolours$sequence <- with(flcolours, 
      c(flpink, flyellow, flblue, flgrapefruit, flpurple)
)

# Set up standard R plots to use FutureLearn colours
knitr::knit_hooks$set(flcolours = function(before, options, envir) {
    if (before) {
        palette(c('#000000', flcolours$sequence))
        NULL
    }
})


