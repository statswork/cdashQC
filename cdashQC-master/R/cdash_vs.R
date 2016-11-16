################################################################################################
#                                       Vital Signs
#
################################################################################################


#' dcast the vs variables
#' 
#' @title create the vital sign data set.
#' @param vs  the dataset dm read from sas
#' @return vs data with necessary variables kept for further summarization. 
#' @export
#' 

create_vs <- function(vs){

 vs <- create_phour(vs) %>% create_baseline()
 
  vs1 <- vs %>% select(CLIENTID, PHOUR, PERIOD, DAY, HOUR, VS_TEST,
                      VS_RES_R, VS_DAT, VS_TIM, VS_POS, VS_LOC, VS_MIN, VS_RCK, status) %>%
                arrange(CLIENTID, VS_DAT, VS_TIM, VS_POS, VS_MIN, VS_LOC) %>%
                mutate(vsdate = parse_date_time(paste(ymd(VS_DAT), seconds_to_period(VS_TIM)), "Ymd HMS", truncated= 3))
  
  vs1 <- vs1 %>% mutate(VS_TIM = format(vsdate, "%H:%M:%S")) %>% select(-vsdate)
  
  return(vs1)
}



