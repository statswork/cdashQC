################################################################################################
#                                       Vital Signs
#
################################################################################################


#' create the vs data
#' 
#' @title create the vital sign data set.
#' @param vs  the dataset dm read from sas
#' @param included the included data set created by \code{\link{create_included}}
#' @return vs data with necessary variables kept for further summarization. 
#' @export
#' 

create_vs <- function(vs, included){

 vs1 <- vs %>% create_phour() %>%         # add PHOUR
               create_seq(included) %>%   # add SEQ
               create_baseline()  
 
  vs2 <- vs1 %>% select(CLIENTID, SEQ, PHOUR, PERIOD, DAY, HOUR, VS_TEST,
                      VS_RES_R, VS_DAT, VS_TIM, VS_POS, VS_LOC, VS_MIN, VS_RCK, status) %>%
                arrange(CLIENTID, VS_DAT, VS_TIM, VS_POS, VS_MIN, VS_LOC) %>%
                format_time(date = "VS_DAT", time = "VS_TIM", newname = "vsdate")
  
  vs3 <- vs2 %>% mutate(VS_TEST = replace(VS_TEST, VS_TEST == "BODY MASS INDEX", "BMI"), 
                        VS_TEST = replace(VS_TEST, VS_TEST == "HEART RATE", "HR"), 
                        VS_TEST = replace(VS_TEST, VS_TEST == "RESPIRATORY RATE", "RESPIRATION")
                        )
  
  return(vs3)
}



