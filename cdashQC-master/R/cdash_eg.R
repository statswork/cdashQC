################################################################################################
#                                               ECGs
#
################################################################################################


#' dcast the ecg variables
#' 
#' @title create variable columns for the ECGs parameters.
#' @param eg  the dataset eg read from sas
#' @return eg1 data with necessary variables kept for further summarization. 
#' @export
#' 

create_eg <- function(eg){
  
  #  if there's no PHOUR variable then create it 
  eg <- create_phour(eg) %>% create_baseline()
  
   # select the variables
  eg_value <- eg %>% select(CLIENTID, PERIOD, PHOUR, DAY, HOUR, EG_TEST, EG_ORRES, 
                            EG_DAT, EG_TIM, EG_ORR_D, EG_SPEC, status) %>% # choose columns
                      arrange(CLIENTID, PHOUR, DAY, HOUR, EG_DAT, EG_TIM)  # sort them 
   
  
  eg1 <- eg_value %>% mutate(egdate = parse_date_time(paste(ymd(EG_DAT), seconds_to_period(EG_TIM)), "Ymd HMS", truncated= 3),
                                HOUR = round(HOUR, 2)) %>%  
                        mutate(EG_TIM = format(egdate, "%H:%M:%S")) %>% 
                        select(-egdate)
   
    return(eg1)
}

