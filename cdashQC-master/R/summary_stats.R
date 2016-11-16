
#' Summary statistics for vital signs or eg
#' 
#' @title summary statistics
#' @param data_clean the clean data set returned by \code{\link{replicate_clean}}.
#' @param included the included data set created by \code{\link{create_included}}
#' @param inter_digit  if rounding happens for the intermediate averages, what digits should be kept.
#' @param final_digit what is the digit for final summary?
#' @param na_rm  should missing values be excluede? Default set to be \code{TRUE}.
#' @return a data frame with summary statistics by test, time point and treatment.
#' @export

summary_vs_eg <- function(data_clean, included, inter_digit = NULL, final_digits = 3, na_rm=TRUE){
  
  # guess what the data set is 
  d1 <- guess_test(data_clean)
  
  clean4 <- replicate_average(data_clean, included, digits = inter_digit, na_rm = na_rm)
  
  # statistical summaries
  clean5 <- clean4 %>%  group_by_(d1, "PHOUR", "SEQ") %>% 
    summarise(COUNT = n(), 
              mean = mean(outcome),
              sd = sd(outcome), 
              mininum = min(outcome),
              median = median(outcome), 
              maximu = max(outcome)) %>%
    mutate(cv = sd/mean)
  
  # transpose
  if (d1 == "VS_TEST"){
    clean6 <- gather(clean5, statistic, value, - VS_TEST, -PHOUR, -SEQ) %>%
      spread(SEQ, value) 
  } else {
    clean6 <- gather(clean5, statistic, value, -EG_TEST, -PHOUR, -SEQ) %>%
      spread(SEQ, value)
  }
  
  
  # the unique time points, used to sort the result by time point
  timepoint <- data_clean %>% ungroup() %>% 
    select(PHOUR, DAY, HOUR) %>% 
    distinct(PHOUR, DAY, .keep_all = T)
  
  clean7 <- left_join(clean6 %>% arrange(PHOUR), 
                      timepoint %>% arrange(PHOUR),
                      by = "PHOUR")
  
  result <- round_df(clean7, final_digits) %>% arrange_(d1, "DAY", "HOUR") %>% 
    select(-DAY, -HOUR)   # round_df() is included in the useful.R file.
  
  return(result)
}



#' Get the summary statistics for change from baseline for \code{vs} and \code{eg}
#' 
#' @title calculate the change from baselines
#' @param data_clean  An object returned from \code{replicate_clean}
#' @param included the included data set created by \code{\link{create_included}}
#' @param inter_digit  if rounding happens for the intermediate averages, what digits should be kept.
#' @param final_digit what is the digit for final summary?
#' @param na_rm  should missing values be excluede? Default set to be \code{TRUE}.
#' @return a data frame with summary statistics by test, time point and treatment.
#' @export
#' 
summary_cfb <- function(data_clean, included, inter_digit = NULL, final_digits = 3, na_rm=TRUE){
  
  # guess what the data set is 
  d1 <- guess_test(data_clean)
  
  # get the replicate averages 
  c1 <- replicate_average(data_clean, included, digits = inter_digit, na_rm = na_rm)
  
  baseline <- c1 %>% filter(status == "BASELINE") %>% 
                     mutate(base = outcome) %>% ungroup() %>%
                     select(-outcome, -status, -PHOUR)

  postdose <- c1 %>% filter(status == "POSTDOSE") %>% 
                     mutate(postdose = outcome) %>% ungroup()
                     select(-outcome, -status)
  
  
  c2 <- left_join(postdose %>% arrange_("CLIENTID", d1, "SEQ"), 
                  baseline %>% arrange_("CLIENTID", d1, "SEQ"), 
                  by = c("CLIENTID", d1, "SEQ"))
  
}



#' Get the averages of the replicates
#' 
#' @title get the averanges of the replicates for \code{vs} or \code{eg} data.
#' @param data_clean  an object returned from \code{replicate_clean}
#' @param included the included data set created by \code{\link{create_included}}
#' @param digits  should the averages be rounded? Default NO.
#' @param na_rm  should missing values be excluede? Default \code{TRUE}.
#' @return the averages
#' @export
#' @examples
#' eg2 <- replicate_data(eg) # find the triplicates
#' @seealso \code{\link{replicate_clean}}

replicate_average <- function(data_clean, included, digits = NULL, na_rm = TRUE){

  d1 <- guess_test(data_clean, var_identifier = "_TEST")
  # get the treatment information
  clean1 <- left_join(data_clean %>% arrange(CLIENTID),         
                      included %>% arrange(CLIENTID) %>% select(CLIENTID, SEQ, RSEQ), 
                      by = "CLIENTID")
  
  clean2 <- clean1
  
  # average by time points wherever applicable.
  if (d1 == "VS_TEST") { 
    clean3 <- clean2 %>% filter(VS_TEST != "OVERALL COMMENT") %>% 
      mutate(outcome = as.numeric(VS_RES_R))
   
    # average the replicates by position wherever applicable 
    clean4 <- clean3 %>%  group_by(CLIENTID, VS_TEST, status, PHOUR, SEQ, DAY, HOUR, VS_POS) %>% 
      summarise(outcome = mean(outcome, na.rm = na_rm))
    
  } else {  # if it's eg data
    clean3 <- clean2 %>% filter(EG_TEST != "OVERALL INTERPRETATION") %>%
      mutate(outcome = as.numeric(EG_ORRES))
    
    clean4 <- clean3 %>% group_by(CLIENTID, EG_TEST, status, PHOUR, SEQ) %>%
      summarise(outcome = mean(outcome, na.rm = na_rm))
  }
  
  if (!is.null(digits)){
    clean4 <- round_df(clean4, digits = digits)
  }
  
  return(clean4)
  
}
