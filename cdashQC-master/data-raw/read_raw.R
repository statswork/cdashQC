library(dplyr)
# read raw data
codes <- read.csv("Y:/development/users/Zhuob01/R-template/data/CODES.csv",stringsAsFactors=F) %>% filter(lb_testc != "")
names(codes) <- toupper(names(codes))
codes$LB_TESTC <- as.character(codes$LB_TESTC)
save(codes, file = "Y:/development/users/Zhuob01/R-template/cdash/cdashQC/data/codes.RData")
