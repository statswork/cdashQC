################################################################################################
#                                       Vital Signs
#
################################################################################################


#' dcast the vs variables
#' 
#' @title create the vital sign data set.
#' @param vs  the dataset dm read from sas
#' @return eg1 the transposed data 
#' @export
#' 




create_vs <- function(vs){
  
  #Change names of variables
  vs$VS_TEST <- trimws(vs$VS_TEST)
  vs$VS_TEST[vs$VS_TEST == "BODY MASS INDEX"] <- "BMI"
  vs$VS_TEST[vs$VS_TEST == "HEART RATE"] <- "HR"
  vs$VS_TEST[vs$VS_TEST == "RESPIRATORY RATE"] <- "RR"
  
  attention <- vs %>% filter(VS_TEST=="OVERALL COMMENT") 
  if (nrow(attention != 0))
    message("There is unusual measurements, check VS_TEST that have value OVERALL COMMENT")
  
  vs1 <- vs %>% filter(VS_TEST !="OVERALL COMMENT") %>% 
    mutate(ptno = as.numeric(CLIENTID), 
           VS_RES_R = as.numeric(VS_RES_R), 
           HOUR = round(HOUR, 2)) %>%
    select(ptno, PHOUR, PERIOD, DAY, HOUR, VS_TEST,
           VS_RES_R, VS_DAT, VS_TIM, VS_POS, VS_LOC, VS_MIN, VS_RCK) %>%
      dcast(ptno + PERIOD + PHOUR + DAY + HOUR + VS_DAT + VS_TIM + VS_POS
            +VS_MIN + VS_LOC + VS_RCK ~ VS_TEST, value.var = "VS_RES_R" ) %>%
      arrange(ptno, VS_DAT, VS_TIM, VS_POS, VS_MIN, VS_LOC) %>%
      mutate(vsdate = parse_date_time(paste(ymd(VS_DAT), seconds_to_period(VS_TIM)), "Ymd HMS", truncated= 3))
  
  vs1 <- vs1 %>% mutate(VS_TIM = format(vsdate, "%H:%M:%S")) %>% select(-vsdate)
  
  return(vs1)
}