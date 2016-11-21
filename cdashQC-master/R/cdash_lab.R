
#' create the lab data
#' 
#' @title create the lab test data set.
#' @param lb_cq  the dataset lb_cq read from sas
#' @return lab data with necessary variables kept for further summarization. 
#' @export
#' 

create_lab <- function(lb_cq){
  
  lab1 <- create_phour(lb_cq) %>% create_baseline()
  
  
  lab2 <- lab1 %>% select(CLIENTID, PHOUR, PERIOD, DAY, HOUR, LB_SEX_C, LB_CAT, LB_TEST, 
                          LB_TESTC, LB_DAT, LB_TIM, LB_ORRES, LB_ORREU, LB_ORLO, LB_ORHI, LB_STNRC, LB_UNSCH,
                          LB_CMTST, LB_NRIND, LB_CSGFG, LB_PISIG, status) %>%
                    mutate(LB_CAT = replace(LB_CAT, !(trimws(LB_CAT) %in% c('CHEM','HEME','UA','UDS','COAG','VIROL')), "OTH"), 
                           LB_ORRES = replace(LB_ORRES,trimws(toupper(LB_CMTST)) == "CLOTTED UNABLE TO ANALYZE", "CUTA"), 
                           LB_ORRES = replace(LB_ORRES,trimws(toupper(LB_CMTST)) == "TOO NUMEROUS TO COUNT", "TNTC"),
                           labdate = parse_date_time(paste(ymd(LB_DAT), seconds_to_period(LB_TIM)), "Ymd HMS", truncated = 3)
                           ) %>%
                    mutate(LB_TIM = format(labdate, "%H:%M:%S")) %>% 
                    select(-labdate) %>%
                    arrange(CLIENTID, PHOUR, PERIOD, DAY, HOUR, LB_CAT, LB_TEST)

  id1 <- trimws(toupper(lab2$LB_TESTC)) == "CK-MB"
  id2 <- trimws(toupper(lab2$LB_TESTC)) == "A/G"
  id3 <- trimws(toupper(lab2$LB_TESTC)) == "B/C"
  
  
  lab3 <- lab2 %>% mutate(LB_TESTC = replace(LB_TESTC, id1, "CKMBB"), 
                         LB_TESTC = replace(LB_TESTC, id2, "AG"), 
                         LB_TESTC = replace(LB_TESTC, id3, "BC"), 
                         LB_ORREU = replace(LB_ORREU, id2, "RATIO"),
                         LB_ORREU = replace(LB_ORREU, id3, "RATIO")) %>%
                  unite(col = "signal", LB_NRIND, LB_CSGFG, LB_PISIG, sep = "", remove = FALSE)
  
  
  return(lab3)
}



## Step 1:  first build the data and clean it ----------------------------------------------


#' clean the lab data, handling rechecks/unscheduled included.
#' 
#' @title Clean the lab test data set.
#' @param lab  the dataset returned by \code{\link{create_lab}}
#' @param ex  the ex data set.
#' @param included the included data set. 
#' @return lab data with necessary variables kept for further summarization. 
#' @export
#' @seealso \code{\link{create_lab}} and \code{\link{create_included}}


# for lab shift, we need to take care of rechecks/unscheduled/..

clean_lab <- function(lab, ex, included){
  
  # decide the obs that have problems
  temp0 <- lab %>% filter( !grepl("EARLY TERM", toupper(PERIOD))) %>% # early terminations
    filter( !(HOUR > 0 & trimws(LB_UNSCH) != "")) %>%  # remove the postdose recheck values
    filter( !( (HOUR <= 0 | is.na(HOUR) )  &           # remove pre-dose recheck having empty values
                 (trimws(LB_UNSCH) != "") & (trimws(LB_ORRES) == "")) )
  
  
  # combine all necessary variables
  included1 <- included %>% select(CLIENTID, SEQ)
  temp1 <- inner_join(temp0 %>% arrange(CLIENTID), 
                      included1 %>% arrange(CLIENTID), by= "CLIENTID")

  temp1$treat <- ""
  id4 <- !(trimws(toupper(temp1$PERIOD)) %in% c("SCR", "SCREEN", "POST"))
  temp1$treat[id4] <- substr(trimws(temp1$SEQ[id4]), as.numeric(temp1$PERIOD[id4]),as.numeric(temp1$PERIOD[id4]))
  
  
  med <- ex %>% mutate(drugtype = EX_TRT, 
                       exdate = parse_date_time(paste(ymd(EX_STDAT), seconds_to_period(EX_STTIM)), "Ymd HMS", truncated = 3)
                       ) %>%
                mutate(EX_STTIM=format(exdate, "%H:%M:%S")) %>%
                filter( !is.na(EX_STDAT) ) %>%
                select(CLIENTID, EX_STDAT, EX_STTIM, EX_TRT, PERIOD, drugtype) %>%
                arrange(PERIOD, CLIENTID) %>% 
                group_by(PERIOD, CLIENTID) %>%  
                filter(row_number()==1)  
  
  temp3 <- left_join(temp1 %>% arrange(CLIENTID, PERIOD), 
                     med %>% arrange(CLIENTID, PERIOD),
                     by = c("CLIENTID","PERIOD")) %>%
            arrange(CLIENTID, PERIOD, DAY, HOUR)
  
  

  id_base <- !(toupper(trimws(temp3$PERIOD)) %in% c("SCREEN", "SCREENING")) &                              # not in the screening
    (temp3$HOUR < 0 | is.na(temp3$HOUR)) &                                                      # predose hours
    ( temp3$LB_DAT > temp3$EX_STDAT | (temp3$LB_DAT == temp3$EX_STDAT & temp3$LB_TIM > temp3$EX_STTIM) )  # LB test is done after dose
  
  baseprob <- temp3[id_base, ]
  
  if(nrow(baseprob) > 0 & !all(is.na(baseprob)) ) warning("Check your BASEPROB data")
  temp4 <- temp3[!id_base, ]            # the "good" data
  
  # By default, the missing values of LB_NRIND are reset to be "Normal"
  temp5 <- temp4 %>% mutate(LB_NRIND = replace(LB_NRIND, trimws(LB_NRIND) == "", "N"),
                            LB_NRIND = replace(LB_NRIND, trimws(LB_NRIND) != "N" & trimws(LB_CAT) == "UA", "H")) # UA has NORMAL and HIGH

  return(temp5)  
}



# Step 2: Get the baselines  ----------------------------------------------

create_lab_baseline <- function(lb_clean){
  
  baselines <- lb_clean %>%  filter(status == "BASELINE") %>%
                            mutate(bl = trimws(LB_NRIND), baseperiod = PERIOD, baseday = DAY, basehour = HOUR) %>%
                            select(LB_CAT, LB_TESTC, CLIENTID, baseperiod, baseday, basehour, bl) %>%
                            arrange(LB_CAT, LB_TESTC, CLIENTID, baseperiod, baseday, basehour)
                          
  return(baselines)
}


# Step 3: Get the postdoes  ----------------------------------------------

# #' Construct postdose data
# #' @title create a postdose from lb_cq.
# #' @param lb_clean  the dataset returned by \code{clean_lab}.
# #' @return the baseline data set.


create_lab_postdose <- function(lb_clean){
  ## post dose
  temp1 <- lb_clean %>% filter(status == "POSTDOSE")
  postdose <- temp1 %>% filter(HOUR > 0 & trimws(LB_ORRES)!= "")
  
  return(postdose)
}




# Step 4: create the lab shift table  ----------------------------------------------

#' Create the lab shift table.
#'
#' @title create the lab shift table.
#' @param lb_clean  the dataset returned by \code{clean_lab}.
#' @return the shift table
#' @export
#' @seealso \code{\link{clean_lab}}
#' @examples 
#' included <- create_included(ex, dm, cr, ds)
#' lab <- create_lab(lb_cq)
#' lb_clean <- clean_lab(lab, included, ex)
#' lbshift <- summary_labshift(lb_clean)  
#'

summary_labshift <- function(lb_clean){
  
  # get baseline
  baselines <- create_lab_baseline(lb_clean)
  # get postdose
  postdose <- create_lab_postdose(lb_clean)
  
  shift1 <- inner_join(postdose %>% arrange(LB_CAT, LB_TESTC, CLIENTID),
                          baselines %>% arrange(LB_CAT, LB_TESTC, CLIENTID),
                          by = c("LB_CAT", "LB_TESTC", "CLIENTID")) %>%
                   mutate(change = paste(trimws(bl), trimws(LB_NRIND), sep = ""))
  

  shift2 <- shift1 %>% select(LB_CAT, LB_TESTC, LB_TEST, SEQ, PHOUR, change)  %>%
                       ftable %>% as.data.frame()
  
  shift3 <- shift2 %>% spread(change, Freq) 
  
  # add unpopulated columns and make those columns to be 0
  header <- c("LL", "LN", "LH", "NL", "NN", "NH", "HL", "HN","HH")
  t1 <- !(header %in%  names(shift3))    # which columns did not show up 
  
  zeros <- data.frame(matrix(0, nrow = nrow(shift3), ncol =sum(t1)))  
  names(zeros) <- header[t1]

  shift4 <- bind_cols(shift3, zeros) %>% mutate_if(is.factor, as.character)   
  
  shift5 <- shift4 %>% select(LB_CAT, LB_TESTC, LB_TEST, SEQ, PHOUR, LL, LN, LH, NL, NN, NH, HL, HN, HH) %>%
                       filter( LL+ LN+ LH+ NL+ NN+ NH+ HL+ HN+ HH  != 0)
  
  ## sort the results by day and hours
  timepoint <- lb_clean %>% select(PHOUR, DAY) %>% 
                distinct()
  
  shift6 <- left_join(shift5 %>% arrange(PHOUR), 
                      timepoint %>% arrange(PHOUR), 
                      by = "PHOUR") %>%
            arrange(LB_CAT, LB_TEST, SEQ, DAY) %>% select(-DAY)
  
  return(shift6)
}  




#' Summarize the lab statistics
#'
#' @title summary statistics for lab shift.
#' @param lb_clean  the dataset returned by \code{clean_lab}.
#' @param digits  how many digits should be kept for the final data.
#' @return the summary statistics by test code, time point and by treatment.
#' @export
#' @seealso \code{\link{clean_lab}}
#' @examples 
#' included <- create_included(ex, dm, cr, ds)
#' lab <- create_lab(lb_cq)
#' lb_clean <- clean_lab(lab, included= included, ex= ex)
#' s1 <- summary_lab(clean_lab)  #not urinalysis
#'

summary_lab <- function(lb_clean, digits = 3){
  
  # filtering
  lab1 <- lb_clean  %>% filter(LB_ORLO != "" | LB_ORHI != "") %>% # remove the obs that are categorical (e.g., "NEGATIVE") 
                  mutate(outcome = as.numeric(LB_ORRES)) %>%
                  filter(!is.na(outcome))  # 
    
  
  # get the normal range values
  nr <- normal_range(lb_clean) %>% ungroup() %>% select(-lgthrge)
  
  lab2 <- left_join(lab1 %>% arrange(LB_CAT, LB_TESTC), 
                    nr  %>% arrange(LB_CAT, LB_TESTC), 
                    by = c("LB_CAT", "LB_TESTC"))
  
  # keep only baseline hour and postdose hours
  lab2_1 <- lab2 %>% filter(status %in% c("BASELINE", "POSTDOSE"))
  
  # statistical summaries
  lab3 <- lab2_1 %>%  group_by(LB_CAT, LB_TEST, range, PHOUR, DAY, HOUR, SEQ) %>% 
    summarise(COUNT = n(), 
              mean = mean(outcome),
              sd = sd(outcome),
              mininum = min(outcome),
              q1 = quantile(outcome, probs = 0.25),
              median = median(outcome), 
              q3 = quantile(outcome, probs = 0.75),
              maximu = max(outcome)) %>%
    mutate(cv = sd/mean)
  
  r0 <- gather(lab3, statistic, outcome, -LB_CAT, -LB_TEST, -range, -PHOUR, -DAY, -HOUR, -SEQ) 
  r1 <- r0 %>% spread(SEQ, outcome) %>% arrange(LB_CAT, LB_TEST, DAY, HOUR)
  
  result <- round_df(r1, digits) %>% 
            ungroup()  # this function is included in the useful.R file.
  
  return(result)
}






# ------------------------------------------------------ LAB OOR ------------------------------------------------------

#' Find the out-of-range values
#'
#' @title Find OOR
#' @param lab  the data returned by \code{\link{create_lab}}.
#' @return a list containg OOR.
#' @export

lab_oor <- function(lab){
   
  lab1 <- lab %>% filter(signal != "" |  
                        trimws(toupper(PERIOD)) == "EARLY TERMINATION" |
                        trimws(toupper(PERIOD)) == "UNSCHEDULED")
  # normal ranges
  nr <- normal_range(lab) %>% ungroup() %>% select(LB_CAT, LB_TESTC, range)
  
  oor <- left_join(lab1 %>% arrange(LB_CAT, LB_TESTC), 
                   nr %>% arrange(LB_CAT, LB_TESTC), 
                   by = c("LB_CAT", "LB_TESTC"))
  
  labcat <- unique(oor$LB_CAT) 
  
  oor1 <- list()
    for ( i in 1:length(labcat)) {
      r1 <- filter(oor, LB_CAT == labcat[i]) %>% mutate(outcome = paste(LB_ORRES, signal))
      if(nrow(r1) >0 ) {  # not empty
        r2 <- r1 %>% select(CLIENTID, PERIOD, PHOUR, DAY, HOUR, LB_DAT, range, LB_TEST, outcome) %>% 
          spread(LB_TEST, outcome)
        oor1[[i]] <- r2 %>% arrange(CLIENTID, PERIOD, DAY, HOUR)
      } else {
        oor1[[i]] <- NULL 
      }
    }
  names(oor1) <- labcat
  
  return(oor1)
}




#' Find the normal range of each test code
#'
#' @title Find normal range for lab test
#' @param lab  the dataset generated by \code{create_lab}
#' @return a data frame containing test codes and their corresponding normal range
#' @export


normal_range <- function(lab){
  
  ranges <- lab %>% select(LB_CAT, LB_TESTC, LB_ORLO, LB_ORHI, LB_SEX_C, LB_STNRC) %>%
            arrange(LB_CAT, LB_TESTC, LB_ORLO, LB_ORHI, LB_SEX_C) %>%
            distinct()
          
  male <- ranges %>% filter( trimws(toupper(LB_SEX_C)) %in% c("MALE", "M")) 
  female <- ranges %>% filter( trimws(toupper(LB_SEX_C)) %in% c("FEMALE", "F")) 
  
  range2 <- full_join(male %>% arrange(LB_CAT, LB_TESTC, LB_ORLO, LB_ORHI, LB_STNRC), 
                     female %>% arrange(LB_CAT, LB_TESTC, LB_ORLO, LB_ORHI, LB_STNRC), 
                     by = c("LB_CAT", "LB_TESTC", "LB_ORLO", "LB_ORHI", "LB_STNRC"))
  
  # create a sex variable, if sex is not empty, then this testcode is specific for this gender.
  range2$sex <- ""
  id1 <- is.na(range2$LB_SEX_C.y);   # is female column empty?
  id2 <- is.na(range2$LB_SEX_C.x)    # is male column empty?
  range2$sex[!id1 & id2] <- range2$LB_SEX_C.y[!id1 & id2]; 
  range2$sex[id1 & !id2] <- range2$LB_SEX_C.x[id1 & !id2]
  range2 <- range2 %>% select(-LB_SEX_C.x, -LB_SEX_C.y)
  
  
  # creating the range variable seems to be a complicated data step, so I write a function to do it.
  range3 <- create_range(range2)  # creates the range variable 
  
  tworg <- range3 %>% select(LB_CAT, LB_TESTC, sex, range ) %>% 
    arrange(LB_CAT, LB_TESTC, sex, range) %>% 
    mutate(sex = replace(sex, sex == "", "BOTH"))
  
  temp1 <- tworg %>% group_by(LB_CAT, LB_TESTC, sex) %>% 
    filter(row_number()==1)
  
  temp2 <- tworg %>% group_by(LB_CAT, LB_TESTC, sex) %>% 
    filter(row_number()>1)
  
  
  
  data1 <- left_join(temp1, temp2, by = c("LB_CAT", "LB_TESTC", "sex")) %>%
    mutate(range.y = replace(range.y, is.na(range.y), ""))
  
  
  final_range <- data1 %>% mutate(range = paste(range.x, range.y, sep =" "))  %>%
    mutate(range = replace(range, range.y=="", range.x), 
           lgthrge = nchar(range)) %>%  # define the length of each range
    select(-range.x, -range.y)     %>%  # don't need these two variable 
    arrange(LB_CAT, LB_TESTC, range, lgthrge)    
  
  lb_orlog <- final_range %>% group_by(LB_CAT, LB_TESTC, range, lgthrge) %>%
    filter(row_number() == n())         # select if last.obs
  
  return(lb_orlog)
  
}



create_range <- function(data){
  
  all_exist <- sum (names(data)  %in% c("LB_ORLO", "LB_ORHI", "LB_TESTC", "sex", "LB_STNRC")) == 5
  if(!all_exist) stop("at least one of the following \n LB_ORLO, LB_ORHI, LB_TESTC, sex, LB_STNRC\n does not exist or not named correctly, check your data") 
  
  low <- trimws(data$LB_ORLO)
  high <- trimws(data$LB_ORHI)
  testc <- trimws(data$LB_TESTC)
  sex <- trimws(data$sex)
  stnrc <- trimws(data$LB_STNRC)
  
  age <- rep("", length(low))
  range0 <- range1 <- rep("", length(low))
  
  id1 <-  testc  =="ALP" &  low == "48"  &  high == "131"
  id2 <-  testc  =="ALP" &  low  == "45" &  high  == "120"
  id3 <-  testc  =="ALP" &  low  == "38" &  high  == "104"
  id4 <-  testc  =="ALP" &  low  == "37" &  high  == "115"
  
  age[id1 | id3] <- "0-25"; age[id2 | id4] <- "26-99"
  
  id5 <- low != ""
  id6 <- high != ""
  id7 <- grepl("NEG", stnrc) | grepl("NOT", stnrc) | grepl("TRAC", stnrc) | grepl("POS", stnrc) |
    grepl("0-", stnrc) | grepl("NON", stnrc) | grepl("CLEAR", stnrc) | grepl("YELLOW", stnrc)
  
  range0[id5] <- paste(low[id5], high[id5], sep = "-")
  range0[!id5 & id6] <- high[!id5 & id6]
  range0[!id5 & !id6 & id7] <- stnrc[!id5 & !id6 & id7]
  range0[!id5 & !id6 & !id7 & stnrc != ""] <- paste("<", stnrc[!id5 & !id6 & !id7 & stnrc != ""])
  
  id8 <- sex == ""
  id9 <- age == ""
  
  range1[!id8 & id9 ] <- paste(substr(sex[!id8 & id9 ], 1, 1), ":", range0[!id8 & id9 ], sep = "")
  range1[!id8 & !id9] <- paste(age[!id8 & !id9], "/", substr(sex[!id8 & !id9], 1, 1), ":", range0[!id8 & !id9], sep = "")
  range1[id8  & id9] <- range0[id8 & id9]
  range1[id8 & !id9] <- paste(age[id8 & !id9], ":", range0[id8 & !id9])
  
  data$age <- age
  data$range <- range1
  return(data)
}
