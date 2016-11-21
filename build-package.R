library(devtools)
library(roxygen2)
setwd("C:/Users/zhuob01/Documents/cdashQC/cdashQC-master")
# devtools::use_data(CODES, internal = T)
document()
check()
Sys.getenv("PATH")
Sys.setenv(PATH = "C:/texlive/2016/bin/win32")
build(manual = T)
install.packages("~/cdashQC/cdashQC_0.1.2.tar.gz", repos = NULL, type = "source")


## generate the help manual.
pack <- "cdashQC"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))