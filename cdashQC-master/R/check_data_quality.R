
### guess the name of the test code: it should end with var_identifier. 

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



###  guess the number of reps by PERIOD, PHOUR and test
## 
#' guess the number of replicates for each PERIOD/PHOUR/TESTCODE
#' 
#' @title guess the number of replicates
#' @param  data  currently support \code{eg} and \code{vs}
#' @param var_identifier, which columns is the variable for test code names. 
#' @return a data frame containing number of reps per PERIOD/PHOUR/TESTCODE
#' @export
#' @examples 
#' d1 <- guess_reps(eg)
guess_reps <- function(data, var_identifier = "_TEST"){
 
  d1 <- guess_test(data, var_identifier = var_identifier)     # get the variable name  of the test code
  r0 <- data %>% create_phour() %>%                    # if no PHOUR available, create one
                select(CLIENTID, PERIOD, PHOUR, matches(d1))
  names(r0)[names(r0)==d1] <- "TEST_CODE"              # change the name for easy manipulation.
  
  periods <- unique(r0$PERIOD)
  result <- data.frame()
  for ( i in 1:length(periods)){
    r1 <- r0 %>% filter(PERIOD == periods[i])
    r2 <- as.data.frame(ftable(r1))                       # get the cross-table
    r3 <- r2 %>% group_by(TEST_CODE, PERIOD, PHOUR) %>% 
                  summarize(shouldHave = calc_mode(Freq) ) %>%  # use the mode to decide the number of reps
                  ungroup() %>% mutate_if(is.factor, as.character) 
    
    result <- bind_rows(result, r3) 
    
  }
  
  result <- result %>% arrange(PERIOD, PHOUR, TEST_CODE) %>% 
                select(PERIOD, PHOUR, TEST_CODE, shouldHave) %>%
                filter(shouldHave > 0)
 
  names(result)[names(result)=="TEST_CODE"] <- d1
  return(result)
}


## check the replicates and possible RECHECK or MISSING VALUES
#' check data qualities, reporting missing and RECHECKS
#' 
#' @title check the replicates and issue a message if there's issue
#' @param  data  currently support \code{eg} and \code{vs}
#' @param reps   the number of replicates planned. (should be tabled by TESTCODE, PERIOD and PHOUR). If \code{NULL}, \code{guess_reps} will be invoked to guess the number of replicates.
#' @param printed  whether to print the observations having issues at the console.
#' @return a data frame containing subject and issues at given period and protocol hours
#' @export
#' @seealso \code{guess_reps}.
#' @examples 
#' d1 <- check_data(eg)


check_data <- function(data, reps = NULL, printed = TRUE){
 
  if( is.null(reps)) {reps <-  guess_reps(data, var_identifier = "_TEST")}   # if there's no reps specification
  
  
  # change the test name to TEST_CODE for easy data manipulation.
  original_name <- guess_test(data, var_identifier = "_TEST")
  temp_name <- "TEST_CODE"
  names(data)[names(data) == original_name] <- temp_name
  names(reps)[names(reps) == original_name] <- temp_name
  
  # check whether each time point has correct replicate numbers
  obs_hour <- data %>% select(PERIOD, PHOUR, TEST_CODE, CLIENTID)
  periods <- unique(obs_hour$PERIOD)
  data_tab <- data.frame()  # count the actual reps
  for ( i in 1:length(periods))
  {
    r1 <- obs_hour %>% filter(PERIOD == periods[i])
    r2 <- as.data.frame(ftable(r1)) %>% mutate_if(is.factor, as.character)
    data_tab <- bind_rows(data_tab, r2)
  }
  
  ## combine the actual frequency with the planned frequency
  t1 <- right_join(data_tab %>% arrange(PERIOD, PHOUR, TEST_CODE), 
                   reps %>% arrange(PERIOD, PHOUR, TEST_CODE), 
                   by = c("PERIOD", "PHOUR", "TEST_CODE")) 
  
  ## find those having issue with reps
  t2 <- t1 %>% mutate(exact_reps = ifelse(Freq==shouldHave, TRUE, FALSE)) %>% 
    filter(exact_reps== FALSE)
  
  # if t2 is not empty, then the corresponding subject has measurement issues
  if(nrow(t2) > 0 ){
    if (original_name == "EG_TEST"){
      t2 <- spread(t2, TEST_CODE,exact_reps)
      t2 <- t2 %>% mutate(messages = paste("SUBJECT = ", trimws(CLIENTID), " PERIOD = ",
                          PERIOD, " PHOUR = ", PHOUR, " should have ", 
                          shouldHave, " replicates, but observed ",  Freq,sep = ""))
    }
    
    else if (original_name == "VS_TEST"){
      
      t2 <- t2 %>% mutate(messages = paste("SUBJECT = ", trimws(CLIENTID), " PERIOD = ", 
                           PERIOD, " PHOUR = ", PHOUR, " TEST = ", TEST_CODE, 
                         " should have ", shouldHave, " replicates, but observed ",
                         Freq,sep = ""))
    }
      if (printed){
        for (i in 1:nrow(t2)){ message(t2$messages[i]) }
    }
      
      names(t2)[names(t2)==temp_name] <- original_name
      return(t2)  
    
  }
  else {message("NO REPLICATE ISSUE with the data")}
}
