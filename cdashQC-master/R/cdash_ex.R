

# create seqtest data
# the returned data set should have all sequences and an indicator showing if subject completed all sequences

create_seqtest <- function(ex){  # NOTE: THIS FUNCTION IS CALLED IN AE TABLE AS WELL
  
  ex1 <- ex %>% mutate(SEQ = substr(EX_TRT_C, 4, 4),   # get the treatment lable
                       PERIOD = gsub("A|B|C|D|E|F|G|H|I", "", PERIOD) # remove A or B or C, ect.  equivalent to compress() in SAS
                        ) %>% 
                select(CLIENTID, SCRID, SEQ, PERIOD, EX_TRT, EX_DOSE, EX_DOSU, EX_ROUTE, EX_FORM, 
                       EX_STDAT, EX_STTIM )
  
  ex_start <- ex1 %>% group_by(CLIENTID, SEQ, PERIOD) %>% # the first dosing time
             filter(row_number()==1) %>% ungroup()  
  
  ex_end <- ex1 %>% group_by(CLIENTID, SEQ, PERIOD) %>% # the last dosing time
    filter(row_number() == n() ) %>% ungroup() %>% 
    mutate(EX_LASTDOSEDAT = EX_STDAT, EX_LASTDOSETIM = EX_STTIM) %>% 
    select(CLIENTID, SEQ, PERIOD, EX_LASTDOSEDAT, EX_LASTDOSETIM)
  #label(ex_end$EX_LASTDOSEDAT) <- "End Date of Treatment"
  #label(ex_end$EX_LASTDOSETIM) <- "End Time of Treatment"
  
  medseq <- left_join(ex_start %>% arrange(CLIENTID, SEQ, PERIOD), 
                      ex_end %>% arrange(CLIENTID, SEQ, PERIOD), 
                      by = c("CLIENTID", "SEQ", "PERIOD"))
  
  return(medseq)
  
}





#' Get the start time of each treatment
#'
#' @title Get the start time of each treatment
#' @param ex  the dataset ex read from sas
#' @return a data frame specifying the start time of each treatment
#' @export
#'
#'

ex_start <- function(ex){
  
  ex1 <- create_seqtest(ex) %>% 
              mutate(treat_st = parse_date_time(paste(ymd(EX_STDAT), seconds_to_period(EX_STTIM)), "Ymd HMS", truncated = 3)) %>%
              select(CLIENTID, SEQ, treat_st) 
            
  n_trt_tab <- ex1 %>% select(CLIENTID) %>% mutate(ind = TRUE) %>%
                       group_by(CLIENTID) %>% mutate(Freq = cumsum(ind)) %>% 
                       filter(row_number() == n()) %>% 
                       ungroup() %>% select(-ind)
        
  n_trt <- max(n_trt_tab$Freq)
  
  ex2 <- ex1 %>% mutate(trt0 = T) %>% group_by(CLIENTID) %>% 
                mutate(trt = cumsum(trt0)) %>% ungroup() %>%
                select(CLIENTID, trt, treat_st) %>%
                arrange(CLIENTID,  treat_st, trt) %>%
                spread(trt, treat_st) 
  
  trt <- ex1 %>% mutate(trt0 = T) %>% group_by(CLIENTID) %>% 
                mutate(trt = cumsum(trt0)) %>% ungroup() %>%  # how to get a running sum
                select(CLIENTID, trt, SEQ) %>%
                arrange(CLIENTID, SEQ, trt) %>%
                spread(trt, SEQ) 
  
  ex3 <- left_join(ex2 %>% arrange(CLIENTID), 
                   trt %>% arrange(CLIENTID), 
                   by = "CLIENTID")
  ex4 <- left_join(ex3, n_trt_tab %>% arrange(CLIENTID), 
                   by = "CLIENTID")            
  
  
  names(ex4)[-1] <-  c(paste("treat", 1:n_trt, "sttm", sep = "_"), 
                       paste("treat", 1:n_trt, sep = "_"), 
                       "total_trt")
  ## decide non-empty columns
  
  return(ex4)
  
}

