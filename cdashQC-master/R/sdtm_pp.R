
#' create the pc, pd and pp data 
#' 
#' @title prepare PK/PD data
#' @param data the SDTM format data of either \code{pc}, \code{pd} or \code{pp}.
#' @param ex the SDTM data \code{ex} where treatment information is stored.
#' @return the data set with necessary variables selected for analysis
#' @export

sdtm_pkpd <- function(data, ex){
  
  ex1 <- ex %>% select(USUBJID, EXSEQ, EXGRPID, EXCAT, EXDOSE) %>% 
                mutate(period = substr(trimws(EXGRPID), 1, 2))
  
  # detect the domain
  domain <- trimws(unique(data$DOMAIN))
  
  select_var <- c("USUBJID", "DOMAIN",
                  paste(domain, c("GRPID", "TESTCD", "TEST", "ORRES", "ORRESU", "STRESN", 
                                "STRESU", "STAT", "SPEC", "LLOQ", "DTC", "TPT"), sep = ""), 
                  "VISITDY")  # the variables to be selected
  
  coln <- match(select_var, names(data))
  grpid <- data %>% select(ends_with("GRPID"))
  names(grpid) <- "gpid"
  
  data1 <- data %>% select(coln[!is.na(coln)]) %>% 
                      mutate(period = substr(trimws(grpid$gpid), 1, 2))
  
  data2 <- right_join(ex1 %>% arrange(USUBJID, period) %>% select(USUBJID, period, EXCAT, EXDOSE),
                    data1 %>% arrange(USUBJID, period), 
                    by = c("USUBJID", "period"))
  
  return(data2)
  
}



#' Calculate geometric CV 
#' 
#' @title calculate geometric CV
#' @param v1  a vector
#' @return the geometric CV
#' 
#' 
 
gcv <- function(v1, log = FALSE){
  
  if(!log) {x <- log(v1)}
  else {x <- v1}
  result <- sqrt(exp(var(x))-1)
  return(result)
}



#' Summary statistics for the  PK/PD data. Celerion standard: When calculating Geom Mean and GCV, SAS code takes 
#' the concentration values of 0.00 that would go into the calculation and sets them to missing and then 
#' calculates the geom mean and gcv with those values removed. Those 0.00 values are used when calculating all 
#' other summary statistics.  
#' 
#' @title calculate the 10-point statistics for PK/PD
#' @param data the object returned from \code{\link{sdtm_pkpd}}
#' @param na_rm should \code{NA} be removed? Default \code{TRUE}
#' @return the statistics
#' \item{n}{number of observations}
#' \item{mean}{the mean}
#' \item{sd}{standard deviation}
#' \item{cv}{coefficient of variation}
#' \item{sem}{standard error of the mean}
#' \item{min}{minimum}
#' \item{median}{the median}
#' \item{max}{the maximum}
#' \item{geom}{geometric mean}
#' \item{gcv}{geometric standard deviation \code{GCV  = sqrt(exp(var(log(x)))-1 )}}
#' @export


summary_pkpd <- function(data, na_rm = TRUE){
  
  # detect the domain
  domain <- trimws(unique(data$DOMAIN))
  
  select_var <- c("USUBJID", "period", "EXCAT", 
                  paste(domain, c("TPT", "SPEC", "TESTCD", "STRESN"), sep = ""))
  
  coln <- match(select_var, names(data))
  
  data1 <- data %>% select(coln[!is.na(coln)])
  
  # detect the name of grouping variables
  name1 <- names(data1)[grepl("SPEC", names(data1))]
  name2 <- names(data1)[grepl("TPT", names(data1))]
  name3 <- names(data1)[grepl("TESTCD", names(data1))]
  name50 <- grepl("STRESN", names(data1))
  names(data1)[name50] <- "num_value"
 
  if(domain == "PP"){
    data1 <- data1 %>% group_by_("EXCAT", "period", name1, name3) 
  }
  else{
    data1 <- data1 %>% group_by_("EXCAT", "period", name1, name2, name3) 
  }
      s1 <- data1 %>% filter(!is.na(num_value)) %>%
                  summarize(q1n = n(), 
                  q2mean = mean(num_value, na.rm = na_rm),
                  q3sd = sd(num_value, na.rm = na_rm), 
                  q4cv = q3sd/q2mean, 
                  q5sem = q3sd/sqrt(q1n), 
                  q6min = min(num_value, na.rm = na_rm), 
                  q7median = median(num_value, na.rm = na_rm), 
                  q8max = max(num_value, na.rm = na_rm))
      
      # for geometric means and geomcv, we set 0.00 to be missing
      s2 <- data1 %>% filter(num_value > 0) %>% 
                      summarise(q9geom = exp(mean(log(num_value))), 
                                q10geomcv = gcv(num_value))
  if (domain == "PP")  
  {
    s3 <- left_join(s1 %>% arrange_("EXCAT", "period", name1, name3), 
                    s2 %>% arrange_("EXCAT", "period", name1, name3), 
                    by = c("EXCAT", "period", name1, name3))
    
  }
    
  else {
    s3 <- left_join(s1 %>% arrange_("EXCAT", "period", name1, name2, name3), 
                    s2 %>% arrange_("EXCAT", "period", name1, name2, name3), 
                    by = c("EXCAT", "period", name1, name2, name3))
  }    
      
  return(s3)
  
}



