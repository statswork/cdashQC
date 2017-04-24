################################################################################################
#                                               ECGs
#
################################################################################################


#' dcast the ecg variables
#' 
#' @title create variable columns for the ECGs parameters.
#' @param eg  the dataset eg read from sas
#' @param included the included data set created by \code{\link{create_included}}
#' @return eg1 data with necessary variables kept for further summarization. 
#' @export
#' 

create_eg <- function(eg, included){
  
  #  if there's no PHOUR variable then create it 
  eg1 <- eg %>% create_phour() %>% 
        create_seq(included) %>%
        create_baseline()  
                            
   # select the variables
  eg_value <- eg1 %>% select(CLIENTID, SEQ, PERIOD, PHOUR, DAY, HOUR, EG_TEST, EG_ORRES, 
                            EG_DAT, EG_TIM, EG_ORR_D, EG_SPEC, status) %>% # choose columns
                      arrange(CLIENTID, SEQ, PHOUR, DAY, HOUR, EG_DAT, EG_TIM)  # sort them 
   
  
  eg2 <- eg_value %>% format_time(date = "EG_DAT", time = "EG_TIM", newname = "egdate") %>%
                        mutate(HOUR = round(HOUR, 2)) %>%  
                        select(-egdate)
   
    return(eg2)
}