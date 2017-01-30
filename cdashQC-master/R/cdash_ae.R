
ae_assign1 <- function(ae_ex){  # if there's only one treatment
  idx <- ae_ex$total_trt == 1 
  
  sub_1 <- ae_ex[idx, ]
  
  # those experiencing AEs, but ondate is before treatment
  id1 <- sub_1$ondate < sub_1$treat_1_sttm & sub_1$AE_TERM != "NONE"
  sub_1$ASSIGNED_TRT[id1] <- "PRIOR TO TREATMENT"  

  ## postdose AE
  id2 <- sub_1$ondate >= sub_1$treat_1_sttm & sub_1$AE_TERM != "NONE"
  sub_1$ASSIGNED_TRT[id2] <- sub_1$treat_1[id2] 
  
  ae_ex[idx, ] <- sub_1
  
  return(ae_ex)
}


ae_assign2 <- function(ae_ex){  # if there's only one treatment
  
  idx <- ae_ex$total_trt == 2 
  
  sub_1 <- ae_ex[idx, ]
  
  # those experiencing AEs, but ondate is before treatment

  # POSTDOSE AEs
  ## how to assign an AE to a treatment
  # prior         | assign to A          |  Assign to B          
  # ------------- |Trt A --------------- |Trt B-----------------
  
  id1 <- sub_1$ondate < sub_1$treat_1_sttm & sub_1$AE_TERM != "NONE"
  sub_1$ASSIGNED_TRT[id1] <- "PRIOR TO TREATMENT"  
  
  id3 <- sub_1$ondate >= sub_1$treat_1_sttm & sub_1$ondate < sub_1$treat_2_sttm & sub_1$AE_TERM != "NONE"
  sub_1$ASSIGNED_TRT[id3] <- sub_1$treat_1[id3] 
  
  id4 <- sub_1$ondate >= sub_1$treat_2_sttm & sub_1$AE_TERM != "NONE"
  sub_1$ASSIGNED_TRT[id4] <- sub_1$treat_2[id4] 
  
  ae_ex[idx, ] <- sub_1
  
  return(ae_ex)
  
}




ae_assign3 <- function(ae_ex){  # if there's only one treatment
  
  idx <- ae_ex$total_trt == 3 
  
  sub_1 <- ae_ex[idx, ]

  # POSTDOSE AEs
  ## how to assign an AE to a treatment
  # prior         | assign to A          |  Assign to B     | Assign to C     
  # ------------- |Trt A --------------- |Trt B-------------| Trt C----------
  
  id1 <- sub_1$ondate < sub_1$treat_1_sttm & sub_1$AE_TERM != "NONE"
  sub_1$ASSIGNED_TRT[id1] <- "PRIOR TO TREATMENT"  
  
  id3 <- sub_1$ondate >= sub_1$treat_1_sttm & sub_1$ondate < sub_1$treat_2_sttm & sub_1$AE_TERM != "NONE"
  sub_1$ASSIGNED_TRT[id3] <- sub_1$treat_1[id3] 
  
  id4 <- sub_1$ondate >= sub_1$treat_2_sttm & sub_1$ondate < sub_1$treat_3_sttm & sub_1$AE_TERM != "NONE"
  sub_1$ASSIGNED_TRT[id4] <- sub_1$treat_2[id4] 
  
  id4 <- sub_1$ondate >= sub_1$treat_3_sttm & sub_1$AE_TERM != "NONE"
  sub_1$ASSIGNED_TRT[id5] <- sub_1$treat_3[id5] 
  
  ae_ex[idx, ] <- sub_1
  
  return(ae_ex)
  
}




#' Assign ae to a treatment.
#'
#' @title Assing AE to treatment
#' @param ae  the dataset ae read from sas
#' @param ex  the dataset ex read from sas
#' @return a data frame with one more column specifying which treatment the AE should be in.
#' @export
#'

assign_ae_trt <- function(ae, ex){

  # pay attention to | (elementwise use of "or") and || (overall evaluation)
  ae1 <- ae %>% mutate(               
    AE_TERM = replace(AE_TERM, AE_TERM == "" | AE_YN == "NO", "NONE" ),      
    ondate= parse_date_time(paste(ymd(AE_STDT), seconds_to_period(AE_STTM)), "Ymd HMS", truncated = 3), 
    redate= parse_date_time(paste(ymd(AE_ENDT), seconds_to_period(AE_ENTM)), "Ymd HMS", truncated = 3),      # recovery date and time of ae
    proceduretime1 = parse_date_time(paste(ymd(AE_PDT1), seconds_to_period(AE_PTM1)), "Ymd HMS", truncated=3)
  ) %>%
    arrange(CLIENTID)   # sort by ptno
  
  ex_st <- ex_start(ex)
  
  ae_ex <- left_join(ae1 %>% arrange(CLIENTID), ex_st %>% arrange(CLIENTID), 
                     by = "CLIENTID") %>% mutate(ASSIGNED_TRT = "")
  
  n_trt <- max(ae_ex$total_trt)
  if (n_trt == 1){
    ae_ex <- ae_assign1(ae_ex) 
  }
  if(n_trt == 2)
  {
    ae_ex <- ae_ex %>% ae_assign1()  %>% ae_assign2()
    
  }
  
  if(n_trt == 3)
  {
    ae_ex <- ae_ex %>% ae_assign1()  %>% ae_assign2() %>% ae_assign3()
    
  }
  
  return(ae_ex)
}






#' createaet.
#'
#' @title do what createaet does.
#' @param ae  the dataset ae read from sas
#' @param ex  the dataset ex read from sas
#' @param included  the dataset included from sas, can be created using \code{create_included()}
#' @param improv the same argument as the sas macro \code{createaet}
#' @return aet the data
#' @export
#'


create_aet <- function(ae, ex, improv = 99){
  
  final <- assign_ae_trt(ae, ex)
  

  
  if ( nrow(final) != nrow(ae)) stop(paste("final has ", nrow(final), "observations while ae has",
                           nrow(ae), "You are losing observations (Duplicates Maybe?)", sep = " " ))
 # final <- final %>% mutate(duration = as.period(redate - ondate))
 # final <- final %>% mutate(duration = as.period(interval(as.POSIXct(ondate), as.POSIXct(redate))))
  final$duration = as.period(interval(as.POSIXct(final$ondate), as.POSIXct(final$redate)))
  

  improve <- final %>% filter(AE_OUT==improv)
  if (nrow(improve) > 0){ # if the improve data is not empty
     improve <- improve %>%
        mutate(ondate = parse_date_time(paste(ymd(AE_ENDT), seconds_to_period(AE_ENTM)), "Ymd HMS", truncated = 3)) %>%
        select(CLIENTID, ondate, AE_TERM)  %>% arrange(CLIENTID, ondate, AE_TERM)

      final <- full_join(final, improve, by = c("CLIENTID", "ondate", "AE_TERM"))
    }

   return(final)
  }




#' list the ae
#'
#' @title list Adverse Envent
#' @param aet  created by \code{create_aet}
#' @param type an indicator. Should I list \code{ae1 (type = 1)}, \code{ae2 (type = 2)} or \code{ae3 (type = 3)}?
#' @return a data frame
#' @export
#' @seealso \code{\link{create_aet}}
#' 


listing_ae <- function(aet, type = 1){
  
  
  
  if(type == 1){result <- ae1(aet)}
  else if (type == 2) {result <- ae2(aet)}
  else if (type == 3) {result <- ae3(aet)}
  return(result)
}




# #' ae1
# #' 
# #' @title Create Adverse event list.
# #' @param aet  the dataset created by \code{create_aet}
# #' @return a data frame
# #' @export
# #' @seealso \code{\link{create_aet}}

ae1 <- function(aet){

  aet_subset <-  aet %>% select(CLIENTID, AE_TERM, AE_STDT, AE_STTM, AE_ENDT, 
                       AE_ENTM, ASSIGNED_TRT, ondate, redate, duration)
  
    result <- aet_subset %>% mutate(AE_STTM = format(ondate, "%H:%M:%S"),
                                  AE_ENTM = format(redate, "%H:%M:%S")) %>%
                            select(-ondate, -redate)
    result <- as.data.frame(result)
    return(result)

}



# #' ae2
# #'
# #' @title list Adverse Envent 2.
# #' @param aet the dataset created by \code{create_aet}
# #' @return a data frame 
# #' @export
# #' @seealso \code{\link{create_aet}}
# #' 
ae2 <- function(aet){

  aet_subset <- aet %>% select(CLIENTID, ASSIGNED_TRT, AE_TERM, AE_STDT, AE_STTM,
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
           select(CLIENTID, ASSIGNED_TRT, AE_TERM, AE_STDT, AE_STTM, 
                   AE_PDT1, AE_PTM1, AE_P1, ondate, proceduretime1) 
  
    result <- aet_s %>% mutate(AE_STTM = format(ondate, "%H:%M:%S"),
                               AE_PTM1 = format(proceduretime1, "%H:%M:%S")) %>%
                        select(-ondate, -proceduretime1)
    
    return(result)

}



#' summarize treatment-emergent adverse event
#' 
#' @title teae by number of subjects or by number of events
#' @param data the data set created by \code{\link{create_aet}}
#' @param group choose a varialbe that has all values identical (e.g., "STUDYNO").
#' @param var the treatment variable name, "EX_TRT_C" for example
#' @param by  has two options, either "\code{event}" or "\code{subject}"
#' @return a list
#' \item{ae}{frequency table for teae}
#' \item{soc}{summarization by system organ class and by preferred term}
#' @export 

summary_teae <- function(data, group = "STUDYNO", var = "EX_TRT_C", by = "subject"){
  stopifnot(by %in% c("subject", "event"))
 
   if (by == "subject"){
     
       # total number of subjects doesd
       dosed <- data %>% distinct(CLIENTID, .keep_all = TRUE) %>%
                         ungroup() %>% 
                         get_summary_stats(group = group, var = var)
       
       # total number of subjects with teae
       teae <- data %>% filter(teae == "YES") %>%
                        distinct(CLIENTID, .keep_all = TRUE) %>% 
                        ungroup %>%
                        get_summary_stats(group = group, var= var)
       
       temp <- left_join(dosed %>% select(-trait) %>% arrange(type),
                         teae %>% select(-trait) %>% arrange(type), 
                         by = "type")
       names(temp) <- c("treat", "dosed", "with_teae")
       temp$without_teae <- temp$dosed-temp$with_teae
       
       temp2 <- temp %>% gather(ae, number,  -treat) %>% 
                         spread(treat, number) 
       
       temp2$overall <- rowSums(temp2[, -1])
       
       t1 <- data %>% filter(teae == "YES") %>%
                      group_by(CLIENTID, AEP_PT) %>%
                      filter(row_number()==1) %>%      # remove duplicated obs for each subject
                      ungroup() 
    
     } 
  if (by == "event"){
    
        N_teae <- data %>% filter(teae == "YES") %>%
                           ungroup() %>%
                           get_summary_stats(group = group, var = var)
        names(N_teae)[3] <- "col3"
        
        temp2 <- N_teae %>% mutate(trait = "N_TEAEs") %>% 
                              spread(type, col3) 
        temp2$Overall <- rowSums(temp2[, -1])
        
        # summarize by soc and pt
        t1 <- data %>% filter(teae == "YES") %>% ungroup() 
    }  
  
  
  # teae table for system organ class 
  soc <- t1 %>% get_summary_stats(group = var, var = "AEP_SOC")
  # teae table for preferred term 
  pt <- t1 %>% get_summary_stats(group = var, var = "AEP_PT")
  # match SOC and PT
  soc_pt <- data %>% ungroup %>% select(AEP_SOC, AEP_PT) %>% distinct()
  pt2 <- right_join(soc_pt %>% arrange(AEP_PT), 
                    pt %>% mutate(AEP_PT = type) %>% select(-trait, -type) %>% arrange(AEP_PT), 
                    by = "AEP_PT")
  
  soc2 <- soc %>% mutate(AEP_SOC = type, AEP_PT = "--Total--") %>% select(-trait, -type)
  soc_pt2 <- bind_rows(pt2, soc2, .id = "from") %>% arrange(AEP_SOC, desc(from), AEP_PT) %>% select(-from)
  soc_pt2$Overall <- rowSums(soc_pt2[, -(1:2)])
  
  result <- list(ae=temp2, soc = soc_pt2)
  
  return(result)
}

#' summarize adverse events by severity
#' 
#' @title Adverse event by severity
#' @param data the data set created by \code{\link{create_aet}}
#' @param group choose a varialbe that has all values identical (e.g., "STUDYNO").
#' @param var the treatment variable name, "EX_TRT_C" for example
#' @param by  has two options, either "\code{event}" or "\code{subject}". If \code{by = "subject"}, the subject will be counted under the highest severity level.
#' @return a list
#' \item{pt}{frequency table by preferred term}
#' \item{total}{frequency table by treatment}
#' @export 
#' 
summary_ae_sev <- function(data, group = "STUDYNO", var = "EX_TRT_C", by = "subject"){
    
  stopifnot(by %in% c("subject", "event"))
  # summarize overall
 if (by == "subject"){
     o1 <- data %>% filter(!is.na(AE_STDT)) %>%                    # if it has a start date, then it's AE
                   group_by(CLIENTID, AEP_PT, AE_SEV) %>%          # keep the most severe case 
                   filter(row_number()==n()) %>%                    # make sure this is by-subject.
                   ungroup()
   
   } 
  if (by == "event") {
   
     o1 <- data %>% filter(!is.na(AE_STDT)) %>% 
                    group_by(CLIENTID, AEP_PT, AE_SEV) %>%
                    ungroup() 
   
  }
  
  pt1 <- o1 %>% group_by_(var, "AEP_PT", "AE_SEV_D") %>%
                summarize(count = n()) 
  names(pt1)[1] <- "col1"
  pt2 <- pt1 %>% dcast(AEP_PT + AE_SEV_D ~ col1, fill = 0, value.var = "count")
  pt2$Overall <- rowSums(pt2[, -(1:2)])
  
  t1 <- o1 %>% group_by_(var, "AE_SEV_D") %>% summarize(count = n())
  names(t1)[1] <- "col1"
  t2 <- t1 %>% dcast(AE_SEV_D ~ col1, fill = 0, value.var = "count")
  t2$Overall <- rowSums(t2[, -1])
  
  result <- list(pt = pt2, total = t2)
  return(result)
}




#' summarize adverse events by relationship to study drug
#' 
#' @title Adverse event by relationship to study drug
#' @param data the data set created by \code{\link{create_aet}}
#' @param group choose a varialbe that has all values identical (e.g., "STUDYNO").
#' @param var the treatment variable name, "EX_TRT_C" for example
#' @param by  has two options, either "\code{event}" or "\code{subject}". If \code{by = "subject"}, that means When a subject experienced the same AE at more than one level of drug relationship during a treatment period, each AE was counted separately
#' @return a list
#' \item{pt}{frequency table by preferred term}
#' \item{total}{frequency table by treatment}
#' @export 
#' 
#' 

summary_ae_rel <- function(data, group = "STUDYNO", var = "EX_TRT_C", by = "subject"){
  stopifnot(by %in% c("subject", "event"))
  # summarize overall
  if (by == "subject"){
    o1 <- data %>% filter(!is.na(AE_STDT)) %>%                    # if it has a start date, then it's AE
      group_by(CLIENTID, AEP_PT, AE_REL_D) %>%          
      filter(row_number()==1) %>%                    # make sure this is by-subject and each relativeness is counted once for a subject who has more than 2 levels 
      ungroup()
    
  } 
  if (by == "event") {
    
    o1 <- data %>% filter(!is.na(AE_STDT)) %>% 
      group_by(CLIENTID, AEP_PT, AE_REL_D) %>%
      ungroup() 
    
  }
  
  
  pt1 <- o1 %>% group_by_(var, "AEP_PT", "AE_REL_D") %>%
                summarize(count = n()) 
  names(pt1)[1] <- "col1"
  pt2 <- pt1 %>% dcast(AEP_PT + AE_REL_D ~ col1, fill = 0, value.var = "count")
  pt2$Overall <- rowSums(pt2[, -(1:2)])
  
  t1 <- o1 %>% group_by_(var, "AE_REL_D") %>% summarize(count = n())
  names(t1)[1] <- "col1"
  t2 <- t1 %>% dcast(AE_REL_D ~ col1, fill = 0, value.var = "count")
  t2$Overall <- rowSums(t2[, -1])
  
  result <- list(pt = pt2, total = t2)
  return(result)
  
}
