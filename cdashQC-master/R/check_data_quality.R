#

# get the mode of a vector
calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}




#' Check the possible issues with ECG data 
#'
#' @title check for possile missing or recheck measurements.
#' @param eg  the dataset eg.
#' @return a data frame containing observations with possible issues. 
#' @export
#'
check_eg <- function(eg){
  
  eg0 <- eg %>% select(PERIOD, PHOUR, CLIENTID)
     
  eg_table <- as.data.frame(ftable(eg0)) %>% arrange(PERIOD, PHOUR, CLIENTID)
  find_mode <- eg_table %>% 
            group_by(PERIOD, PHOUR) %>% 
            summarise_each(funs(calc_mode), Freq) %>% 
            mutate(mode = Freq) %>% select(-Freq)
  
  eg_table1 <- inner_join(eg_table, 
                          find_mode %>% ungroup() %>% arrange(PERIOD, PHOUR), 
                          by = c("PERIOD", "PHOUR"))
  
  eg_problem <- eg_table1 %>% filter(Freq != mode) %>%
                mutate(status = ifelse(Freq < mode, "Missing Values", "May contain Rechecks or Measurement Errors")) %>%
                mutate_if(is.factor, as.character)        # if the variable is a factor, change it to character
  # get the actual DAY and HOUR information
  
  useful_info <- eg %>% select(PERIOD, PHOUR, CLIENTID, EG_DAT,EG_TIM, DAY, HOUR, EG_TEST, EG_ORRES, EG_SPEC) 

  eg_problem2 <- inner_join(eg_problem %>% arrange(PERIOD, PHOUR, CLIENTID),
                            useful_info %>% arrange(PERIOD, PHOUR, CLIENTID), 
                            by = c("PERIOD", "PHOUR", "CLIENTID"))

  return(eg_problem2)
}


#' Check the possible issues with vital signs data 
#'
#' @title check for possile missing or recheck measurements.
#' @param vs The dataset vs.
#' @return a data frame containing observations with possible issues. 
#' @export
#'

check_vs <- function(vs){
  vs0 <- vs %>% select(PERIOD, PHOUR, CLIENTID)
  
  vs_table <- as.data.frame(ftable(vs0)) %>% arrange(PERIOD, PHOUR, CLIENTID)
  find_mode <- vs_table %>% group_by(PERIOD, PHOUR) %>%
                summarize_each(funs(calc_mode), Freq) %>% 
                mutate(mode = Freq) %>% select(-Freq)
  
  vs_table1 <- inner_join(vs_table, 
                          find_mode %>% ungroup() %>% arrange(PERIOD, PHOUR), 
                          by = c("PERIOD", "PHOUR"))
                
  vs_problem <- vs_table1 %>% filter(Freq != mode) %>% 
                  mutate(status = ifelse(Freq < mode, "Missing Values", "May contain Rechecks  or Measurement Errors")) %>%
                  mutate_if(is.factor, as.character)        # if the variable is a factor, change it to character
  
  useful_info <- vs %>% select(PERIOD, PHOUR, CLIENTID, VS_DAT,VS_TIM, DAY, HOUR, VS_TEST, VS_ORRES, VS_COM, VS_RCK) 
  
  vs_problem2 <- inner_join(vs_problem %>% arrange(PERIOD, PHOUR, CLIENTID),
                            useful_info %>% arrange(PERIOD, PHOUR, CLIENTID), 
                            by = c("PERIOD", "PHOUR", "CLIENTID"))
  return(vs_problem2)
}

