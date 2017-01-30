#' guess the name of the test code: it should end with var_identifier. 
#' 
#' @title guess the name of the test
#' @param  data  currently support \code{eg} and \code{vs}
#' @param var_identifier, which columns is the variable for test code names. 
#' @return the test code
#' @export
#' @examples 
#' guess_test(eg)

guess_test <- function(data, var_identifier = "_TEST"){
  
  short_code <- c("EG", "VS", "LB")
  
  if(any(names(data)== var_identifier)){            # exact match
    complete_identifier <- var_identifier
    return(complete_identifier)
  }
  
  else if (any(grepl(var_identifier, names(data)))){ # partial match
    index <- rep(F, length(short_code))
    for (i in 1:length(short_code)) {
      index[i] <- any(grepl(paste(short_code[i], "_", sep = ""), names(data)))
    }
    if (sum(index)>1) stop("Name conflict!!")
    # keep track of the original name
    complete_identifier <- paste(short_code[index], var_identifier, sep = "")
    return(complete_identifier)
  }
  else {stop(paste("There is no variable whose name partially matches", var_identifier))}
}




###  guess the number of reps by SEQ, PERIOD, PHOUR and test
## 
#' guess the number of replicates for each SEQ/PERIOD/PHOUR/TESTCODE
#' 
#' @title guess the number of replicates
#' @param  data  data of \code{eg} (created by \code{\link{create_eg}}) or \code{vs} (created by \code{\link{create_vs}})
#' @param var_identifier, which columns is the variable for test code names. 
#' @return a data frame containing number of reps per PERIOD/PHOUR/TESTCODE
#' @export
#' @examples 
#' eg0 <- create(eg, included)
#' d1 <- guess_reps(eg0)
guess_reps <- function(data, var_identifier = "_TEST"){
  
  d1 <- guess_test(data, var_identifier = var_identifier)     # get the variable name  of the test code
  
  r0 <- data %>% select(CLIENTID, SEQ, PERIOD, PHOUR, matches(d1))
  names(r0)[names(r0)==d1] <- "TEST_CODE"              # change the name for easy manipulation.
  
  r1 <- r0 %>% filter(!grepl("EARLY TERMINA", toupper(PERIOD))) %>%
               filter(!grepl("UNSCHEDULED", toupper(PERIOD))) %>%
               unite_("seq_prd", c("SEQ", "PERIOD"), remove = FALSE) 
    
  seq_prds <- unique(r1$seq_prd)
  result <- data.frame()
  for ( i in 1:length(seq_prds)){
    r2 <- r1 %>% filter(seq_prd == seq_prds[i])
    r3 <- as.data.frame(ftable(r2))                       # get the cross-table
    r4 <- r3 %>% group_by(TEST_CODE, SEQ, PERIOD, PHOUR) %>% 
                 summarize(shouldHave = calc_mode(Freq) ) %>%  # use the mode to decide the number of reps
                 ungroup() %>% mutate_if(is.factor, as.character) 
    
    result <- bind_rows(result, r4) 
    
  }
  
  result <- result %>% arrange(SEQ, PERIOD, PHOUR, TEST_CODE) %>% 
                       select(SEQ, PERIOD, PHOUR, TEST_CODE, shouldHave) %>%
                       filter(shouldHave > 0)
  
  names(result)[names(result)=="TEST_CODE"] <- d1
  return(result)
}


## check the replicates and possible RECHECK or MISSING VALUES
#' check data qualities, reporting missing and RECHECKS
#' 
#' @title check the replicates and issue a message if there's issue
#' @param  data data of \code{eg} (created by \code{\link{create_eg}}) or \code{vs} (created by \code{\link{create_vs}})
#' @param reps   the number of replicates planned. (should be tabled by TESTCODE, SEQ, PERIOD and PHOUR). If \code{NULL}, \code{guess_reps} will be invoked to guess the number of replicates.
#' @param printed  whether to print the observations having issues at the console.
#' @return a data frame containing subject and issues at given period and protocol hours
#' @export
#' @seealso \code{guess_reps}.
#' @examples 
#' eg0 <- create_eg(eg, incldued)
#' d1 <- replicate_check(eg1)


replicate_check <- function(data, reps = NULL, printed = TRUE){
  
  if( is.null(reps)) {    # if there's no reps specification
    reps <-  guess_reps(data, var_identifier = "_TEST")
  }  
  
  d1 <- guess_test(data)
  
  # check whether each time point has correct replicate numbers
  obs_hour <- data %>% select_("SEQ", "PERIOD", "PHOUR", d1, "CLIENTID") %>%
                        unite_("seq_prd", c("SEQ", "PERIOD"), remove = FALSE)
  periods <- unique(obs_hour$seq_prd)
  data_tab <- data.frame()  # count the actual reps
  
  for ( i in 1:length(periods)) {
    r1 <- obs_hour %>% filter(seq_prd == periods[i])
    r2 <- as.data.frame(ftable(r1)) %>% mutate_if(is.factor, as.character)
    data_tab <- bind_rows(data_tab, r2)
  }
  
  ## combine the actual frequency with the planned frequency
  t1 <- right_join(data_tab %>% arrange_("SEQ", "PERIOD", "PHOUR", d1), 
                   reps %>% arrange_("SEQ", "PERIOD", "PHOUR", d1), 
                   by = c("SEQ", "PERIOD", "PHOUR", d1)) 
  
  ## find those having issue with reps
  t2 <- t1 %>% mutate(exact_reps = ifelse(Freq==shouldHave, TRUE, FALSE)) %>% 
               filter(exact_reps== FALSE) %>% 
               select(-seq_prd)
  
  # if t2 is not empty, then the corresponding subject has measurement issues
  if(nrow(t2) > 0 ){
    ids <- which(names(t2) == d1)
    t2 <- t2 %>% mutate(messages = paste("SUBJECT = ", trimws(CLIENTID), " PERIOD = ", 
                                         PERIOD, " PHOUR = ", PHOUR, " ", d1, " = ",  t2[, ids], 
                                         " should have ", shouldHave, " replicates, but observed ",
                                         Freq,sep = "")) %>% arrange(messages)
    
    if (printed){   # do you want to issue the messages?
      for (i in 1:nrow(t2)){ message(t2$messages[i]) }
    }
    
    return(t2)  
    
  }
  else {message("NO REPLICATE ISSUE with the data")}
}




#' get the replicates for each protocol hour (PHOUR)
#' 
#' @title get the replicates for eg or vs data.
#' @param data the data set created by \code{\link{create_eg}} (for ecg data) or by  \code{\link{create_vs}} (for vital signs data).
#' @param reps specifies the structures of reps. If not specified, \code{guess_resp} will be invoked. Default set to be \code{NULL}. 
#' @return  a list 
#'  \item{data_clean}{the subjects containing correct number of resplicates}
#'  \item{data_dirty}{the subjects that have different number of replicates than desired}
#' @export
#' @seealso \code{\link{create_eg}}, \code{\link{guess_resp}} and  \code{\link{replicate_check}}
#' 

replicate_data <- function(data, reps= NULL){
  
  d1 <- guess_test(data)   # guess if the data is eg or vs

  t0 <- replicate_check(data, reps = reps, printed = FALSE)
  t1 <- t0 %>% select_("CLIENTID", "PERIOD", "PHOUR",d1) %>%   # those are data having reps issues
              mutate(dirty_obs = TRUE)
  
  data2 <- right_join(t1 %>% arrange_("CLIENTID",  "PERIOD", "PHOUR",d1), 
                      data %>% arrange_("CLIENTID",   "PERIOD", "PHOUR",d1), 
                      by = c("CLIENTID", "PERIOD", "PHOUR", d1))

  
  data_clean <- data2 %>% filter(is.na(dirty_obs)) %>% select(-dirty_obs)
  data_dirty <- data2 %>% filter(dirty_obs == TRUE) %>% select(-dirty_obs)
  
  result <- list(data_clean = data_clean, data_dirty=data_dirty)
  return(result)
  
}



## after checking the data_dirty data, need to decide which rows to be used as replicates
#' get cleaned replicates 
#' 
#' @title get the cleaned replicates by combining the "clean" data and "dirty" data
#' @param data  an object returned from \code{replicate_data} (works for vs and eg)
#' @param rm_row a vector of integers specifying which rows should be removed from the dirty data.
#' @return the cleaned replicates
#' @export
#' @examples
#' eg2 <- replicate_data(eg) # step 1: find the triplicates
#' eg_prob <- eg2$data_dirty         # need manual check
#' # the following rows should be removed
#' rows_removed <- c(2, 4, 7, 13, 14, 15, 19, 25, 28, 32, 37, 40, 44, 52, 57, 59, 64, 65)
#' eg3 <- replicate_clean(eg2, rows_removed)
#' @seealso \code{\link{replicate_data}}


replicate_clean <- function(data, rm_row = NULL){
  
  data_clean <- data$data_clean
  data_dirty <- data$data_dirty
  
  if(nrow(data_dirty) == 0){return(data_clean)}
  else {
    if(is.null(rm_row)){  # if you don't specify which replicates should be removed
      message("there are issues with replicates. If no 'rm_row' is specified, the observations \n are kept only when they are at last time point of predose or at first time point postdose.")
  
      # guess whether it's eg or vs
      original_name <- guess_test(data_dirty, var_identifier = "_TEST")
      
      if(original_name == "VS_TEST"){  # if the data is vs
        data_keep <- data_dirty %>% group_by(CLIENTID, SEQ, PERIOD, PHOUR, DAY, HOUR, VS_TEST, VS_POS, VS_LOC, VS_MIN) %>% 
          filter( (row_number() == 1 & status == "POSTDOSE" )  |   # postdose: choose the first obs
                    ( status != "POSTDOSE" &  row_number() == n() ) )  #predose: choose the last obs
        }
      else if (original_name == "EG_TEST") {
            data_keep <- data_dirty %>% group_by(CLIENTID, SEQ, PERIOD, PHOUR, DAY, HOUR, EG_TEST) %>%
                          filter((status == "POSTDOSE" & row_number() == 1 )  |   # postdose: choose the first obs
                             (status != "POSTDOSE" &  row_number() == n()) ) #predose: choose the last obs
        }
      }
    
    else { # if you tell it what rows should be removed.
      data_keep <- data_dirty[-rm_row, ]
    }
    
    data_clean_new <- bind_rows(data_clean, data_keep) %>%
      arrange(CLIENTID, PERIOD, HOUR) %>% ungroup()
  }
  
}





