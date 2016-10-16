
##########################################################################################
#                                     PACKAGES USED
#
##########################################################################################


library(haven)  ## Read SAS data, a package from Hadley
library(dplyr)  # data manipulation tools
library(reshape2) # data manipulation tools
library(knitr)    # output
library(rmarkdown)  # output
library(lubridate)  # date, time and time span
library(ggplot2)    # plot
library(tidyr)      # tidy data
library(lazyeval)  
library(cdashQC)    # package
library(QCdata)     # example data sets of CA19379


##########################################################################################
#                                   FUNCTIONS LOADED
#
##########################################################################################
# source("Y:/development/users/Zhuob01/R-template/cdash/cdashQC/R/cdash_dem.R")
# source("Y:/development/users/Zhuob01/R-template/cdash/R/cdash_ae.R")
# source("Y:/development/users/Zhuob01/R-template/cdash/R/cdash_eg.R")
# source("Y:/development/users/Zhuob01/R-template/cdash/R/cdash_vs.R")
# source("Y:/development/users/Zhuob01/R-template/cdash/R/cdash_con.R")
# source("Y:/development/users/Zhuob01/R-template/cdash/R/cdash_lab_shift.R")
# source("Y:/development/users/Zhuob01/R-template/cdash/R/cdash_lab_oor.R")

###
library(R.utils)

current_path <- "C:/Users/zhuob01/Documents/cdashQC/cdashQC-master/R/"
scan_files <- listDirectory(current_path)
number <- c()
for (i in 1: length(scan_files)){
  number[i] <- countLines(paste(current_path, scan_files[i], sep = ""))[1]
}

print(sum(number))
