# output to knitr
wd = ""

library(knitr)
setwd(wd)
knit2html("PA1_template.Rmd")
browseURL("PA1_template.html")