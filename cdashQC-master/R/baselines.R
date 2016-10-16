

# find which variables have negative hours, note if "negativeHour == F" that means we should use screen or other (PHOUR) as baseline

negative_hour <- function(temp1){
  if (all(names(temp1) %in% "TEST_CODE")){
    stop("YOU must change the variable name to 'TEST_CODE'. Usually, this variable contains all the test categories!")
  }
  
  testc<- temp1 %>% select(TEST_CODE) %>% distinct()  # all the test codes
  testc_Negative <- rep(F, nrow(testc)) # create an indicator to specify whether this code has negative hours as baseline
  for( i in 1: nrow(testc)){
    ids <- temp1$TEST_CODE == as.character(testc[i, 1])
    hours <- unique(temp1$HOUR[ids])
    if (any(hours<0, na.rm = T)) {testc_Negative[i] <- T }
    else {testc_Negative[i] <- F}
  }
  testc$HasNegativeHour <- testc_Negative
  data.frame(testc)
}



# find base line for the test
# create an extra column indicator

#' find the baseline hour for each test category
#' 
#' @title find the baseline hour for each test
#' @param data currently support \code{vs}, \code{eg} and \code{lb_cq}.
#' @param var_identifier a string that can be used to identify the variable name e.g.(\code{VS_TEST, EG_TEST, LB_TEST})
#' @return a data frame where by-subject baseline hour is determined. 
#' @export
#' @examples 
#' # the following two will give you exactly the same result
#' r1 <- guess_base_phour(vs)
#' r2 <- guess_base_phour(vs, var_identifier = "VS_TEST")
#' 
#' # If you want to find baseline hours for eg, the following two methods are equivalent
#' r3 <- guess_base_phour(eg)
#' r4 <- guess_base_phour(eg, var_identifier = "EG_TEST")
#' 

guess_base_phour <- function(data, var_identifier = "_TEST"){
  
  # find the corresponding variable name of the test
 complete_identifier <- guess_test(data, var_identifier)

  
  sort_via <- which(names(data) == complete_identifier)
  names(data)[sort_via] <- "TEST_CODE"      # give it a new name for easy data manipulation.
  
  hour_ind <- negative_hour(data)           # find which test has negative hours 
  
  data2 <- inner_join(data %>% arrange(TEST_CODE), 
                      hour_ind %>% arrange(TEST_CODE),
                      by = "TEST_CODE") 
  
 
  # subset to have negative hours as baseline
  baseline1 <-  data2 %>% filter(HasNegativeHour==T) %>%
          filter(HOUR < 0  & !is.na(HOUR)) %>%                 # has negative hour as baseline
          arrange(CLIENTID, PERIOD, TEST_CODE, DAY, HOUR) %>%  # sort the related variables
          group_by(CLIENTID, PERIOD, TEST_CODE) %>%                
          slice(n())   %>%                                     # the hour of last observation will correspond to baseline hour
          select(CLIENTID, PERIOD, TEST_CODE, PHOUR)  %>%
          mutate(status= "BASELINE")
         
  # find test categories that have NA (PERIOD = SCREEN) as baseline
  baseline2 <- data2 %>% filter(HasNegativeHour==F) %>%
          filter(is.na(HOUR)) %>%
          arrange(CLIENTID, PERIOD, TEST_CODE, DAY, HOUR) %>%  # sort the related variables
          group_by(CLIENTID, PERIOD, TEST_CODE) %>%                
          slice(n())   %>%                                     # the hour of last observation will correspond to baseline hour
          select(CLIENTID, PERIOD, TEST_CODE, PHOUR)  %>%
          mutate(status= "BASELINE")
        
  baseline_hour <- bind_rows(baseline1, baseline2)
  
  baseline_hour <- baseline_hour %>% ungroup() %>% arrange(CLIENTID, TEST_CODE, PERIOD, PHOUR)
  temp_name <- which(names(baseline_hour) == "TEST_CODE")
  names(baseline_hour)[temp_name] <- complete_identifier  # change the code name to its original name
  
  dat <- baseline_hour 
  return(dat)
}


