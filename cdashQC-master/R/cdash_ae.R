#' createaet.
#'
#' @title do what createaet does.
#' @param ae  the dataset ae read from sas
#' @param ex  the dataset ex read from sas
#' @param included  the dataset included from sas, can be created using \code{new_create_included()}
#' @param improv the same argument as the sas macro \code{createaet()}
#' @return aet the data
#' @export
#'


create_aet <- function(ae, ex, included, improv = 99){
  
  # pay attention to | (elementwise use of "or") and || (overall evaluation)
  ae <- ae %>% mutate( ptno = as.numeric(CLIENTID),
              AE_TERM = replace(AE_TERM, AE_TERM == "" | AE_YN == "NO", "NONE" ),      
              ondate= parse_date_time(paste(ymd(AE_STDT), seconds_to_period(AE_STTM)), "Ymd HMS", truncated = 3), 
              redate= parse_date_time(paste(ymd(AE_ENDT), seconds_to_period(AE_ENTM)), "Ymd HMS", truncated = 3),      # recovery date and time of ae
              proceduretime1 = parse_date_time(paste(ymd(AE_PDT1), seconds_to_period(AE_PTM1)), "Ymd HMS", truncated=3)
              ) %>% arrange(ptno)   # sort by ptno


  #  start date and time of treatement
  
  med <- ex %>% mutate(ptno= as.numeric(CLIENTID), 
                      meddt= parse_date_time(paste(ymd(EX_STDAT), seconds_to_period(EX_STTIM)), "Ymd HMS", truncated = 3)) %>%
              select(ptno, meddt, PERIOD, EX_TRT_C) %>%
              filter(!is.na(meddt))  %>%
              arrange(ptno, meddt) %>% mutate(medper = PERIOD) %>% select(-PERIOD) # remove NAs
  firstmed <- med  %>% group_by(ptno) %>%  # group by ptno
              filter(row_number(ptno)==1)    # select distinct values  (first.obs)


  # prior: ae prior dosing
  prior <- inner_join(firstmed, ae, by = "ptno") %>%
    dplyr::filter(ondate < meddt & (!is.na(ondate)))   # select those AEs occurred before dosing

  # after (no distinct value, because there might be multiple period)
  # potential problem when running CA18700 
  after <- inner_join(med, ae, by = "ptno") %>%
    dplyr::filter(ondate >= meddt | is.na(ondate) ) %>%
               mutate(timediff = ondate-meddt)  # create timediff variable

  # separate "after" to three types
  # final1a = on study
  # final1b = unknown times
  # final1c = No AEs

  new <- after %>% arrange(ptno, AE_NO, AE_TERM, ondate) %>% # order the variables
         group_by(ptno, AE_NO, AE_TERM, ondate)
  # select the first obs where ondate is not missing
  final1a <- new %>% filter(row_number(ondate)==1 & !(is.na(ondate)))
  finalb <- new %>% filter(is.na(ondate)) %>% arrange(ptno, AE_NO, AE_TERM) %>% 
                    group_by(ptno, AE_NO, AE_TERM) %>% 
                    filter(row_number(AE_TERM)==1)  # select distinct values
  
  final1b <- finalb %>% filter(AE_TERM != "NONE")  
  final1c <- finalb %>% filter(  AE_TERM == "NONE")

  final <- bind_rows(prior, final1a, final1b, final1c) %>% # efficient way to rbind
          arrange(ptno)
  if ( nrow(final) != nrow(ae)) stop(paste("final has ", nrow(final), "observations while ae has",
                           nrow(ae), "You are losing observations (Duplicates Maybe?)", sep = " " ))
 # final <- final %>% mutate(duration = as.period(redate - ondate))
 # final <- final %>% mutate(duration = as.period(interval(as.POSIXct(ondate), as.POSIXct(redate))))
  final$duration = as.period(interval(as.POSIXct(final$ondate), as.POSIXct(final$redate)))
  
  included1 <- included %>% mutate(ptno = as.numeric(PTNO), seq = SEQ) %>% select(ptno, seq)
  final <- left_join(final, included1, by = "ptno")
  ids <- toupper(final$medper) %in% c('PREDOSE','CC','CREATININE CLEARANCE','SCREENING','ALL','UNKNOWN','UNK','SCREEN')
  final <- final[!ids, ] %>% mutate(pernew = as.numeric(substr(medper, 1, 1)),
                                      treat = substr(seq, pernew, pernew) ) %>%
                              arrange(ptno, ondate, AE_TERM)  #sort
  
  final <- final %>% mutate(t_e =  ifelse(is.na(duration), "NO", "YES") )
  

  improve <- final %>% filter(AE_OUT==improv)
  if (nrow(improve) > 0){ # if the improve data is not empty
     improve <- improve %>%
        mutate(ondate = parse_date_time(paste(ymd(AE_ENDT), seconds_to_period(AE_ENTM)), "Ymd HMS", truncated = 3)) %>%
        select(ptno, ondate, AE_TERM)  %>% arrange(ptno, ondate, AE_TERM)

      final <- full_join(final, improve, by = c("ptno", "ondate", "AE_TERM"))
    }

   aet <- final %>% mutate(pern = as.numeric(pernew)) %>% select(-pernew, -seq, -timediff)
   aet$treat[aet$t_e == "NO" | toupper(ae$AE_TERM) == "NONE"] <- " "

   return(aet)
  }




#' list the ae
#'
#' @title list Adverse Envent
#' @param aet  created by \code{create_aet}
#' @param type an indicator. Should I list \code{ae1}, \code{ae2} or \code{ae3}?
#' @return a data frame
#' @export
#' @seealso \code{\link{create_aet}}
#' 


ae_listing <- function(aet, type = 1){
  
  
  
  if(type == 1){result <- ae1(aet)}
  else if (type == 2) {result <- ae2(aet)}
  else if (type == 3) {result <- ae3(aet)}
  return(result)
}




# #' ae1
# #' 
# #' @title Create Adverse event list.
# #' @param aet  the dataset created by \code{create_aet()}
# #' @return a data frame
# #' @export
# #' @seealso \code{\link{create_aet}}

ae1 <- function(aet){

  aet_subset <-  aet %>% select(ptno, pern, t_e, AE_TERM, AE_STDT, AE_STTM, AE_ENDT, 
                       AE_ENTM, duration, EX_TRT_C, ondate, redate)
  
    result <- aet_subset %>% mutate(AE_STTM = format(ondate, "%H:%M:%S"),
                                  AE_ENTM = format(redate, "%H:%M:%S")) %>%
                            select(-ondate, -redate)
    result <- as.data.frame(result)
    return(result)

}



# #' ae2
# #'
# #' @title list Adverse Envent 2.
# #' @param aet the dataset created by \code{create_aet()}
# #' @return a data frame 
# #' @export
# #' @seealso \code{\link{create_aet}}
# #' 
ae2 <- function(aet){

  aet_subset <- aet %>% select(ptno, pern, EX_TRT_C, AE_TERM, AE_STDT, AE_STTM,
                               AE_FRQ_D, AE_SEV_D, AE_SER_D, AE_OUT_D, 
                               AE_REL_D, AE_ACT_D, AE_AN1_D, ondate, redate)
  


    result <- aet_subset %>% mutate(AE_STTM = format(ondate, "%H:%M:%S")) %>%
                        select(-ondate, -redate)
    result <- as.data.frame(result)
  
    return(result)
}






# #' ae3
# #'
# #' @title list Adverse Envent Non Drug Therapy.
# #' @param aet  created by \code{create_aet()}
# #' @return a data frame
# #' @export
# #' @seealso \code{\link{create_aet}}
# #' 

ae3 <- function(aet){

  aet_s <- aet %>% filter(trimws(AE_P1) != "") %>%
           select(ptno, pern, EX_TRT_C, AE_TERM, AE_STDT, AE_STTM, 
                   AE_PDT1, AE_PTM1, AE_P1, ondate, proceduretime1) 
  
    result <- aet_s %>% mutate(AE_STTM = format(ondate, "%H:%M:%S"),
                               AE_PTM1 = format(proceduretime1, "%H:%M:%S")) %>%
                        select(-ondate, -proceduretime1)
    
    return(result)

}



