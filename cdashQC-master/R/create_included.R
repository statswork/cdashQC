
# create chkseq data
# the returned data set should have all sequences and an indicator showing if subject checked in at least one sequence

create_chkseq <- function(cr){
  
  # additional code to determine if a subject checked in in each period
  cr1 <- cr %>% mutate(PERIOD = gsub("A|B|C|D|E|F|G|H|I", "", PERIOD)) 
  
  cr_ck_in <- cr1 %>% filter(toupper(trimws(CR_CAT)) == "CHECK-IN") %>%    #check in date
                      select(CLIENTID, SCRID, PERIOD, CR_DAT, CR_TIM) %>%
                      group_by(CLIENTID, PERIOD) %>%
                      filter(row_number()==1) %>% ungroup() %>% 
                      mutate(CHK_IN_DAT = CR_DAT, CHK_IN_TIM = CR_TIM) %>% 
                      select(-CR_DAT, -CR_TIM)
  
  cr_return<- cr1 %>% filter(toupper(trimws(CR_CAT)) == "RETURN") %>%    # return date
                      select(CLIENTID, PERIOD, CR_DAT, CR_TIM) %>%
                      group_by(CLIENTID, PERIOD) %>%
                      filter(row_number()==n()) %>% ungroup() %>% 
                      mutate(RETURN_DAT = CR_DAT, RETURN_TIM = CR_TIM) %>% 
                      select(-CR_DAT, -CR_TIM)
  
 # label(cr_return$RETURN_DAT) <- "Date of Scheduled Return"
#  label(cr_return$RETURN_TIM) <- "Time of Scheduled Return"
  
  chk <- left_join(cr_ck_in %>% arrange(CLIENTID, PERIOD), 
                   cr_return %>% arrange(CLIENTID, PERIOD),
                   by = c("CLIENTID", "PERIOD"))
                    
  return(chk)
  
}



## create disposition table
create_dis <- function(ds){
  
  #  DS_TRM_D: primary reason for discontinuation
  
  dis <- ds %>% mutate(DS_TRM_D = trimws(toupper(DS_TRM_D))) %>% 
                select(CLIENTID, SCRID, DS_TRM_D, DS_SPEC, DS_STDAT) %>%
                mutate(DS_TRM_D = replace(DS_TRM_D, trimws(DS_TRM_D)=="", "COMPLETED"))
  
   return(dis)  
}


#' create_included.
#'
#' @title an integrated data set.
#' @param ex  the dataset ex read from sas
#' @param cr  the dataset cr read from sas
#' @param ds  the dataset ds read from sas
#' @return the included data set which contains the following information
#' \item{CLIENTID}{the patient ID}
#' \item{SCRID}{the corresponding screening ID}
#' \item{SEQ}{the treatment sequence, from \code{ex} data}
#' \item{PERIOD}{CRF study period, from \code{ex} data}
#' \item{EX_ROUTE}{Route}
#' \item{EX_FORM}{Formulation}
#' \item{EX_STDAT}{Date of first dosing}
#' \item{EX_STTIM}{Time of first dosing}
#' \item{EX_DOSE}{Dose Amount}
#' \item{EX_LASTDOSEDAT}{Date of last dosing (Created in this function)}
#' \item{EX_LASTDOSETIM}{Time of last dosing}
#' \item{CHK_IN_DAT}{date of check-in for each period}
#' \item{CHK_IN_TIM}{time of check in for each period}
#' \item{RETURN_DAT}{date of return for each period}
#' \item{RETURN_TIM}{time of return for each period}
#' \item{DS_TRM_D}{Primary reason for discontinuation}
#' \item{DS_SPEC}{specify}
#' \item{DS_STDAT}{Date of completion or discontinuation per subject}
#' \item{DS_LSDAT}{Date of last contact per subject}
#' @export
#'
#'

create_included <- function(ex, cr, ds){
  
  seqtest <- create_seqtest(ex)
  chk_in <- create_chkseq(cr)
  dis <- create_dis(ds)
  
  c1 <- right_join(seqtest %>% arrange(CLIENTID, SCRID, PERIOD), 
                  chk_in %>% arrange(CLIENTID, SCRID, PERIOD), 
                  by = c("CLIENTID", "SCRID", "PERIOD"))
  
  c2 <- left_join(c1, dis %>% arrange(CLIENTID, SCRID), 
                  by = c("CLIENTID", "SCRID"))
  

  ## what is the key word of completed
  status <- unique(c2$DS_TRM_D)
  complete_id <- grepl( "COMPLET",toupper(status))
  comple <- status[complete_id]
  
  ## if a subject is not DS_TRM_D "COMPLETED" but have a return date and time, then it actually completed.
  ids <- c2$DS_TRM_D !=  comple & !is.na(c2$RETURN_DAT) & !is.na(c2$RETURN_TIM)
  
  c2$DS_TRM_D[ids] <-  comple
  c2$DS_SPEC[ids] <- ""
 
  return(c2)
}





############################### OTHER CREATED FUNCTIONS ##############################################

## 
# create protocol hour (PHOUR) if it's not already in the data set. 
#' Create protocol hour variable if it's not already in the data set.
#' @title Create protocol hour
#' @param data either \code{eg}, \code{vs} or \code{lb_cq}
#' @param digit round the hour by digit, default set to be 2.
#' @return the same data set with one more column if Phour is created, otherwise returns the input data.
#' @export


create_phour <- function(data, digit = 2){
  
  if (!any(names(data) == "PHOUR")) {
    message("Variable 'PHOUR' not detected, creating it by concatnating variables 'DAY' and 'HOUR'")
    
    data <- data %>% mutate(PHOUR = paste("Day", DAY, "Hour", round(HOUR, digit)))  
  }
  
  return(data)
  
}

##########


cluster_hour <- function(hour, ngroup1, ngroup2){
  
  na_hour <- which(is.na(hour))
  grp <- rep(9999, length(hour))
  
  x <- scale(hour[-na_hour])

  fit1 <- kmeans(x, 17)  
  grp[-na_hour] <- fit1$cluster
  

  data <- data.frame(hour, grp = grp)

  
}





## 
# create SEQ if it's not already in the data set. 
#' Create SEQ variable (treatment) if it's not already in the data set.
#' @title Create SEQ
#' @param data either \code{eg}, \code{vs} or \code{lb_cq}
#' @param included the included data set created by \code{\link{create_included}}
#' @return the same data set with one more column if SEQ is created, otherwise returns the input data.
#' @export


create_seq <- function(data, included){
  
  
  if (!any(names(data) == "SEQ")) {
  
        
        trt1 <- included %>% select(CLIENTID, SEQ, PERIOD, EX_STDAT, EX_STTIM)
        trt2 <- included %>% select(CLIENTID, SEQ, PERIOD) %>% distinct() 
        
        pr <- unique(data$PERIOD)
        unusual <- which(!(trimws(toupper(pr)) %in% c("1", "2", "3")))
      
      for (i in 1:length(unusual)){      # get "SCREEN", "EARLY TERMINATION", "UNSCHEDULED" ect.
        sr <- pr[unusual[i]]                      
        trt2_1 <- trt2 %>% mutate(PERIOD = sr)
        trt1 <- bind_rows(trt1, trt2_1) 
      }
    
          trt <- trt1 %>% group_by(CLIENTID, PERIOD) %>% # if a seq has two periods, remove duplicate "screen"
                          filter(row_number() == 1) %>% 
                          ungroup()
    
      
      # Note: Early Termination will be removed.
      r0 <-  right_join(trt %>% arrange(CLIENTID, PERIOD), 
                        data %>% arrange(CLIENTID, PERIOD),
                        by = c("CLIENTID", "PERIOD"))
      
      r1 <- r0 %>%  filter(!is.na(SEQ))       # those records does not match PERIOD
  }
  
  
  else {
    r1 <- data
  }
  
  return(r1)
}



