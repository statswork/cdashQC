

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

### find the name of the test code: it should end with var_identifier. 

find_test <- function(data, var_identifier = "_TEST"){
  # this piece of code detects which variable name should be the variable name of tests
  short_code <- c("EG", "VS", "LB")
  index <- rep(F, length(short_code))
  for (i in 1:length(short_code)){
    index[i] <- any(grepl(paste(short_code[i], "_", sep = ""), names(data)))
  }
  if (sum(index)>1) stop("Name conflict!!")
  # keep track of the original name
  complete_identifier <- paste(short_code[index], var_identifier, sep = "")
  return(complete_identifier)
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

find_base_phour <- function(data, var_identifier = "_TEST"){
  
  # find the corresponding variable name of the test
 complete_identifier <- find_test(data, var_identifier)

  
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
          select(CLIENTID, PERIOD, TEST_CODE, PHOUR, DAY, HOUR)  %>%
          mutate(status= "BASELINE")
         
  # find test categories that have NA (PERIOD = SCREEN) as baseline
  baseline2 <- data2 %>% filter(HasNegativeHour==F) %>%
          filter(is.na(HOUR)) %>%
          arrange(CLIENTID, PERIOD, TEST_CODE, DAY, HOUR) %>%  # sort the related variables
          group_by(CLIENTID, PERIOD, TEST_CODE) %>%                
          slice(n())   %>%                                     # the hour of last observation will correspond to baseline hour
          select(CLIENTID, PERIOD, TEST_CODE, PHOUR, DAY, HOUR)  %>%
          mutate(status= "BASELINE")
        
  baseline_hour <- bind_rows(baseline1, baseline2)
  temp_name <- which(names(baseline_hour) == "TEST_CODE")
  names(baseline_hour)[temp_name] <- complete_identifier  # change the code name to its original name
  
  dat <- baseline_hour %>% ungroup() %>% arrange(CLIENTID, PERIOD, DAY, HOUR)
  return(dat)
}




#' create an extra column inidcating whether the current row should be considered as baseline. 
#' 
#' @title create baseline indicator.
#' @param data currently support \code{vs}, \code{eg} and \code{lb_cq}.
#' @param var_identifier a string that can be used to identify the variable name e.g.(\code{VS_TEST, EG_TEST, LB_TEST})
#' @return a data frame with an extra column \code{status} whose value could be one of \code{BASELINE}, \code{POSTDOSE} and \code{PREDOSE (NOT BASELINE)} 
#' @export


create_baseline <- function(data, var_identifier = "_TEST"){
  
  # find the baseline hours for each test category
  basehour <- find_base_phour(data, var_identifier)
  var_sort <- names(basehour)[!names(basehour) %in% c("status")]
  
  # merge to the original data to create an extra column showing whether it's baseline or not.
  data1 <- left_join(data %>% arrange_(.dots = var_sort), 
              basehour %>% arrange_(.dots= var_sort), 
              by = var_sort)
  
  id1 <- is.na(data1$status) & (is.na(data1$HOUR) | data1$HOUR < 0)
  id2 <- is.na(data1$status) & ( data1$HOUR >= 0)
  
  data1$status[id1] <- "PREDOSE (NOT BASELINE)"
  data1$status[id2] <- "POSTDOSE"
  
  return(data1)
}








