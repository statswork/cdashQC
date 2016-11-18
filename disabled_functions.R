### old functions


# #' Check the possible issues with vital signs data 
# #'
# #' @title check for possile missing or recheck measurements.
# #' @param vs The dataset vs.
# #' @return a data frame containing observations with possible issues. 
# #' @export
# #'

check_vs_old <- function(vs){
  vs <- create_phour(vs)
  
  vs0 <- vs %>% select(PERIOD, PHOUR, CLIENTID)
  
  vs_table <- as.data.frame(ftable(vs0)) %>% arrange(PERIOD, PHOUR, CLIENTID)
  find_mode <- vs_table %>% group_by(PERIOD, PHOUR) %>%
    summarize_each(funs(calc_mode), Freq) %>% 
    mutate(mode = Freq) %>% select(-Freq)
  
  vs_table1 <- inner_join(vs_table, 
                          find_mode %>% ungroup() %>% arrange(PERIOD, PHOUR), 
                          by = c("PERIOD", "PHOUR"))
  
  vs_problem <- vs_table1 %>% filter(Freq != mode) %>% 
    mutate(status = ifelse(Freq < mode, "Missing Values", "May contain Rechecks  or Measurement Errors")) %>%
    mutate_if(is.factor, as.character)        # if the variable is a factor, change it to character
  
  useful_info <- vs %>% select(PERIOD, PHOUR, CLIENTID, VS_DAT,VS_TIM, DAY, HOUR, VS_TEST, VS_ORRES, VS_COM, VS_RCK) 
  
  vs_problem2 <- inner_join(vs_problem %>% arrange(PERIOD, PHOUR, CLIENTID),
                            useful_info %>% arrange(PERIOD, PHOUR, CLIENTID), 
                            by = c("PERIOD", "PHOUR", "CLIENTID"))
  return(vs_problem2)
}




# #' Check the possible issues with ECG data
# #' 
# #' @title check for possile missing or recheck measurements.
# #' @param eg  the dataset eg.
# #' @return a data frame containing observations with possible issues.
# #' @export

check_eg_old <- function(eg){
  
  eg <- create_phour(eg)
  
  
  eg0 <- eg %>% select(PERIOD, PHOUR, CLIENTID)
  
  eg_table <- as.data.frame(ftable(eg0)) %>% arrange(PERIOD, PHOUR, CLIENTID)
  find_mode <- eg_table %>% 
    group_by(PERIOD, PHOUR) %>% 
    summarise_each(funs(calc_mode), Freq) %>% 
    mutate(mode = Freq) %>% select(-Freq)
  
  eg_table1 <- inner_join(eg_table, 
                          find_mode %>% ungroup() %>% arrange(PERIOD, PHOUR), 
                          by = c("PERIOD", "PHOUR"))
  
  eg_problem <- eg_table1 %>% filter(Freq != mode) %>%
    mutate(status = ifelse(Freq < mode, "Missing Values", "May contain Rechecks or Measurement Errors")) %>%
    mutate_if(is.factor, as.character)        # if the variable is a factor, change it to character
  # get the actual DAY and HOUR information
  
  useful_info <- eg %>% select(PERIOD, PHOUR, CLIENTID, EG_DAT,EG_TIM, DAY, HOUR, EG_TEST, EG_ORRES, EG_SPEC) 
  
  eg_problem2 <- inner_join(eg_problem %>% arrange(PERIOD, PHOUR, CLIENTID),
                            useful_info %>% arrange(PERIOD, PHOUR, CLIENTID), 
                            by = c("PERIOD", "PHOUR", "CLIENTID"))
  
  return(eg_problem2)
}



## print lab_oor

customized_lab_rept_old <- function(lb_cq, ex, cat= "UA", var_per_block=5, digit_keep = 2){
  
  oor <- lab_rept(lb_cq, ex, cat)
  
  
  if (!is.null(oor)){
    # first, separate the data into two, one for fixed columns (those columns will be the same for all output)
    part1 <- oor %>% select(-starts_with( paste(cat, "_", sep = "")))
    part2 <- oor %>% select(starts_with( paste(cat, "_", sep = "")))
    the_rest_name <-  names(part1)
    cat_name <-  names(part2)
    
    # need full name of the codes 
    codes <- the_code(lb_cq) %>% 
      mutate(var_name = paste(LB_CAT, "_", LB_TESTC, sep=""),
             TTL = gsub("~", " ", TTL))      # change "~" to " " in TTL, this will make the column names wrap
    # and also the normal ranges         
    range0 <- create_laborig(lb_cq) %>% normal_range() 
    ranges <- range0  %>% dcast(LB_CAT + LB_TESTC ~sex, value.var= "range")
    ids <- is.na(ranges$A)
    ranges$A[ids] <- paste(ranges$FEMALE[ids], ranges$MALE[ids], sep = " ")
    ranges <- ranges %>% mutate(range = A) %>% select(LB_CAT, LB_TESTC, range)
    
    # the full name will come with its normal range
    codes <- left_join(codes %>% arrange(LB_CAT, LB_TESTC), 
                       ranges %>% arrange(LB_CAT, LB_TESTC), 
                       by = c("LB_CAT", "LB_TESTC")) %>%
      mutate(TTL = paste(TTL, range, sep = " "))
    
    
    ## devivde the variables into multiple blocks and print them one by one.
    
    block <- ceiling(length(cat_name)/var_per_block)  # how many variables per block
    for ( k in 1:block){
      if(k == block){                        # if this is the last block
        from1 <- (k-1)*var_per_block + 1
        to1 <- length(cat_name)
        # c(from1, to1)
        
        # get the corresponding code
        test_code <- as.character(cat_name[from1:to1])
        testcode <- data.frame(var_name = test_code, stringsAsFactors=F) 
        
        # match the code with its full name
        full_name <- left_join(testcode %>% arrange(var_name), 
                               codes %>% arrange(var_name), by = "var_name") %>%
          select(var_name, TTL) %>% distinct()
        
        col_to_show <- which(cat_name %in% test_code)
        
        part2_1 <- part2[, col_to_show]          # keep those names in this output
        emt <- keep_non_empty(part2_1)           # get the index of empty columns and empty rows
        
        part2_keep <- part2_1[, emt$keep_cols]   # remove empty columns
        
        id1 <- is.na(part2_keep)                 # replace "NA" with ""
        part2_keep[id1] <- ""
        
        oor1 <- cbind(part1, part2_keep)         # remove empty rows
        oor1 <- oor1[emt$keep_rows, ]
        
        # now the column names with full test name 
        col_name <- c(as.character(the_rest_name), as.character(full_name$TTL[emt$keep_cols]))
        rownames(oor1) <- NULL
        # print out
        print(kable(oor1, digits =digit_keep, table.attr='class="flat-table"',
                    col.names = col_name, caption = paste(cat, ":block ", k, " of ", block, sep = "")))
      }
      
      else {                 # if this is not the last block
        from1 <- (k-1)*var_per_block + 1
        to1 <-  k*var_per_block;
        
        # get the corresponding code
        test_code <- as.character(cat_name[from1:to1])
        testcode <- data.frame(var_name = test_code, stringsAsFactors=F) 
        
        # try to match the code with its full name
        full_name <- left_join(testcode %>% arrange(var_name), 
                               codes %>% arrange(var_name), by = "var_name") %>%
          select(var_name, TTL) %>% distinct()
        
        col_to_show <- which(cat_name %in% test_code)
        
        part2_1 <- part2[, col_to_show]          # keep those names in this output
        emt <- keep_non_empty(part2_1)           # get the index of empty columns and empty rows
        
        part2_keep <- part2_1[, emt$keep_cols]   # remove empty columns
        
        id1 <- is.na(part2_keep)                 # replace "NA" with ""
        part2_keep[id1] <- ""
        
        oor1 <- cbind(part1, part2_keep)         # remove empty rows
        oor1 <- oor1[emt$keep_rows, ]
        rownames(oor1) <- NULL
        
        # now the column names with full test name 
        col_name <- c(as.character(the_rest_name), as.character(full_name$TTL[emt$keep_cols]))
        
        # print out 
        print(kable(oor1, digits =digit_keep, table.attr='class="flat-table"',col.names = col_name, 
                    caption = paste(cat, ":block ", k, " of ", block, sep = "")))
        
      }
      
    }
    
    
  }
  
}








###################  THIS FUNCTION IS NO LONGER USED (But it's correct) ----------------------------------

labshift_old <- function(lb_cq, included, ex, UA=F){
  
  # clean the lab data
  laborig <- create_lb_cq(lb_cq, included, ex)
  
  
  # get baseline
  baselines <- create_lab_baseline(laborig)
  # get postdose
  postdose <- create_lab_postdose(laborig)
  
  labfinal2 <- inner_join(postdose %>% arrange(LB_CAT, LB_TESTC, ptno),
                          baselines %>% arrange(LB_CAT, LB_TESTC, ptno),
                          by = c("LB_CAT", "LB_TESTC", "ptno")) %>%
    mutate(change = paste(trimws(bl), trimws(LB_NRIND), sep = ""))
  
  if (!UA){                                    # if it's not UA test
    labfinal2 <- labfinal2 %>% filter(trimws(LB_CAT) != "UA")
    nlb_testc <- unique(labfinal2$LB_TESTC)    # how many lab test codes
    nseq <- unique(labfinal2$SEQ)              # how many sequences
    
    numrows <-  0   # should be large enough to hold all results
    LL <- LN <- LH <- NL <- NN <- NH <- HL <- HN <- HH <- rep(0, numrows)
    lb_testc <-  day <- seq <- rep("", numrows)
    
    # create a data frame to store the table results
    tab <- data.frame(seq, lb_testc, day, LL, LN, LH, NL, NN, NH, HL, HN, HH)
    
    
    for (j in 1:length(nlb_testc)) {
      for(k in 1:length(nseq)){
        subset <- labfinal2 %>% filter(LB_TESTC == nlb_testc[j] & SEQ == nseq[k])
        a <- table(subset$DAY, subset$change)
        
        if (nrow(a) > 0)  { # if the table is not empty
          a1 <- colnames(a)
          numrow1 <-  length(rownames(a))   # should be large enough to hold all results
          LL <- LN <- LH <- NL <- NN <- NH <- HL <- HN <- HH <- rep(0, numrow1)
          lb_testc <-  day <- seq <- rep("", numrow1)
          # create a data frame to store the table results
          tab1 <- data.frame(seq, lb_testc, day, LL, LN, LH, NL, NN, NH, HL, HN, HH)
          
          tab1$seq <- nseq[k]
          tab1$lb_testc <- nlb_testc[j]
          tab1$day <- rownames(a)
          # print(c(j, k))
          b <- as.matrix(a)
          for (l in 1:length(a1)){
            col_to_fill <- which(names(tab1) == a1[l])
            tab1[,col_to_fill] <- b[, l]
          }
          tab <- rbind(tab, tab1)
        }
      }
    }
  }
  
  if(UA) {
    labfinal2 <- labfinal2 %>% filter(trimws(LB_CAT) == "UA")
    nlb_testc <- unique(labfinal2$LB_TESTC)    # how many lab test codes
    nseq <- unique(labfinal2$SEQ)              # how many sequences
    
    numrows <-  0   # should be large enough to hold all results
    NN <- NH <- HN <- HH <- rep(0, numrows)
    lb_testc <-  day <- seq <- rep("", numrows)
    
    # create a data frame to store the table results
    tab <- data.frame(seq, lb_testc, day, NN, NH, HN, HH)
    
    
    for (j in 1:length(nlb_testc)) {
      for(k in 1:length(nseq)){
        subset <- labfinal2 %>% filter(LB_TESTC == nlb_testc[j] & SEQ == nseq[k])
        a <- table(subset$DAY, subset$change)
        
        if (nrow(a) > 0)  { # if the table is not empty
          a1 <- colnames(a)
          numrow1 <-  length(rownames(a))   # should be large enough to hold all results
          NN <- NH <- HN <- HH <- rep(0, numrow1)
          lb_testc <- day <- seq <- rep("", numrow1)
          # create a data frame to store the table results
          tab1 <- data.frame(seq, lb_testc, day, NN, NH, HN, HH)
          
          tab1$seq <- nseq[k]
          tab1$lb_testc <- nlb_testc[j]
          tab1$day <- rownames(a)
          # print(c(j, k))
          b <- as.matrix(a)
          for (l in 1:length(a1)){
            col_to_fill <- which(names(tab1) == a1[l])
            tab1[,col_to_fill] <- b[, l]
          }
          tab <- rbind(tab, tab1)
        }
      }
    }
  }
  
  tab <- tab %>% mutate(SEQ = seq, LB_TESTC = lb_testc, DAY = as.numeric(day)) %>%
    select(-seq, -lb_testc, -day) %>%
    arrange(SEQ, LB_TESTC, DAY)
  
  lab_cat <- labfinal2 %>% select(LB_CAT, LB_CAT_D, LB_TEST, LB_TESTC, SEQ, DAY) %>%
    distinct() %>%
    arrange(LB_CAT, SEQ, LB_TESTC, DAY)
  
  result <- inner_join(lab_cat, tab, by = c("SEQ", "LB_TESTC", "DAY"))
  
  return(result)
  
}



################################################################################################
#                                  Lab Shift
#
################################################################################################

# Steps
# 1. clean lb_cq
# 2. create predose baseline
# 3. create postdose,
# 4. merge them into one data set for creating lab shift table.
# 5. make tables


## Step 1:  first build the data and clean it ----------------------------------------------

#' Clean the lb_cq data (for lab shif purpose)
#' @title prepare data for lab shift table.
#' @param lb_cq  the dataset lb_cq read from sas
#' @param ex  the dataset ex read from sas
#' @param included  the dataset created by \code{create_included}. This is the same as the data "included" from sas
#' @return a data frame
#' @export
#' @seealso \code{\link{create_included}}
#'


create_labshift <- function(lb_cq, included, ex){
  
  lb_cq <- create_phour(lb_cq)
  
  laborig <- lb_cq %>% filter(trimws(toupper(LB_UNSCH)) != "SPECIAL" |
                                trimws(toupper(LB_CMTST)) != "CLOTTED UNABLE TO ANALYZE") %>%
    filter(!grepl("EARLY TERMINATION", toupper(PERIOD)))  # filter PERIOD by pattern
  
  
  id1 <- trimws(toupper(laborig$LB_TESTC)) == "CK-MB"
  id2 <- trimws(toupper(laborig$LB_TESTC)) == "A/G"
  id3 <- trimws(toupper(laborig$LB_TESTC)) == "B/C"
  laborig <- laborig %>% mutate(LB_TESTC = replace(LB_TESTC, id1, "CKMBB"),
                                LB_TESTC = replace(LB_TESTC, id2, "AG"),
                                LB_TESTC = replace(LB_TESTC, id3, "BC"),
                                LB_ORREU = replace(LB_ORREU, c(id2), "RATIO"),
                                LB_ORREU = replace(LB_ORREU, c(id3), "RATIO"),
                                ptno = as.numeric(CLIENTID))  %>%
    arrange(ptno) # %>%
  # filter(trimws(toupper(LB_CAT)) %in% c("COAG", "CHEM", "HEME", "UA"))
  
  
  included1 <- included %>% mutate(ptno = as.numeric(CLIENTID)) %>% select(ptno, SEQ) %>% arrange(ptno)
  temp1 <- inner_join(laborig, included1, by= "ptno")
  
  temp1$treat <- ""
  id4 <- !(trimws(toupper(temp1$PERIOD)) %in% c("SCR", "SCREEN", "POST"))
  temp1$treat[id4] <- substr(trimws(temp1$SEQ[id4]), as.numeric(temp1$PERIOD[id4]),as.numeric(temp1$PERIOD[id4]))
  
  
  
  med <- ex %>% mutate(ptno = as.numeric(CLIENTID), drugtype = EX_TRT) %>%
    filter( !is.na(EX_STDAT) ) %>%
    select(ptno, EX_STDAT, EX_STTIM, EX_TRT, PERIOD, drugtype) %>%
    arrange(PERIOD, ptno) %>% # sort by ptno and then PERIOD
    group_by(PERIOD, ptno) %>%  # group by
    filter(row_number(ptno)==1) # select first.ptno
  
  
  temp1 <- left_join(temp1 %>% arrange(ptno, PERIOD), 
                     med %>% arrange(ptno, PERIOD),
                     by = c("ptno","PERIOD")) %>%
    arrange(ptno, PERIOD, DAY, HOUR)
  
  #  Take care of the recheck values
  temp1 <- temp1 %>% filter( !(HOUR > 0 & trimws(LB_UNSCH) != "")) %>%  # remove the after-dose recheck values
    filter( !( (HOUR <= 0 | is.na(HOUR) )  &           # remove pre-dose recheck having empty values
                 (trimws(LB_UNSCH) != "") & (trimws(LB_ORRES) == "")) )
  
  
  # check possible issues with baseline ----------------------------------------------
  id_base <- !(toupper(trimws(temp1$PERIOD)) %in% c("SCREEN", "SCREENING")) &                              # not in the screening
    (temp1$HOUR < 0 | is.na(temp1$HOUR)) &                                                      # predose hours
    ( temp1$LB_DAT > temp1$EX_STDAT | (temp1$LB_DAT == temp1$EX_STDAT & temp1$LB_TIM > temp1$EX_STTIM) )  # LB test is done after dose
  
  baseprob <- temp1[id_base, ]
  # if this data set is not empty, then there might be problem
  if(nrow(baseprob) > 0 & !all(is.na(baseprob)) ) message("Warning: Check your BASEPROB data")
  temp1 <- temp1[!id_base, ]            # the "good" data
  
  # By default, the missing values of LB_NRIND are reset to be "Normal"
  temp1 <- temp1 %>% mutate(LB_NRIND = replace(LB_NRIND, trimws(LB_NRIND) == "", "N"),
                            LB_NRIND = replace(LB_NRIND, trimws(LB_NRIND) != "N" & trimws(LB_CAT) == "UA", "H")) # UA has NORMAL and HIGH
  
  return(temp1)
  
}




# Step 2: Get the baselines  ----------------------------------------------

create_lab_baseline <- function(laborig){
  
  baselines <- create_baseline(laborig) %>%        # create a column indicating whether this obs is baseline or not
    filter(status == "BASELINE") %>%
    mutate(bl = trimws(LB_NRIND), baseperiod = PERIOD, baseday = DAY, basehour = HOUR) %>%
    select(LB_CAT, LB_TESTC, ptno, baseperiod, baseday, basehour, bl) %>%
    arrange(LB_CAT, LB_TESTC, ptno, baseperiod, baseday, basehour)
  
  
  return(baselines)
}


# Step 3: Get the postdoes  ----------------------------------------------

# #' Construct postdose data
# #' @title create a postdose from lb_cq.
# #' @param laborig  the dataset returned by \code{create_labshift()}.
# #' @return the baseline data set.


create_lab_postdose <- function(laborig){
  ## post dose
  temp1 <- create_baseline(laborig) %>%        # create a column indicating whether this obs is baseline or not
    filter(status == "POSTDOSE")
  
  postdose <- temp1 %>% filter(HOUR > 0 & trimws(LB_ORRES)!= "")
  
  return(postdose)
}



# Step 4: create the lab shift table  ----------------------------------------------

#' Create the lab shift table.
#'
#' @title create the lab shift table.
#' @param laborig  the dataset returned by \code{create_labshift}.
#' @param UA  whether to produce shift table for UA (should be done separately)
#' @return the shift table
#' @export
#' @seealso \code{\link{create_labshift}}
#' @examples 
#' included <- create_included(ex, dm, cr, ds)
#' laborig <- create_labshift(lb_cq, included, ex)
#' others <- summary_labshift(laborig,  UA=FALSE)  #not urinalysis
#' UA <- summary_labshift(laborig, UA=TRUE)       # UA
#'


summary_labshift <- function(laborig, UA= FALSE){
  
  # get baseline
  baselines <- create_lab_baseline(laborig)
  # get postdose
  postdose <- create_lab_postdose(laborig)
  
  labfinal2 <- inner_join(postdose %>% arrange(LB_CAT, LB_TESTC, ptno),
                          baselines %>% arrange(LB_CAT, LB_TESTC, ptno),
                          by = c("LB_CAT", "LB_TESTC", "ptno")) %>%
    mutate(change = paste(trimws(bl), trimws(LB_NRIND), sep = ""))
  
  if(!UA){        # if it's not UA test
    labfinal0 <- labfinal2 %>% filter(trimws(LB_CAT) != "UA") 
    labfinal1 <- labfinal0 %>% select(LB_CAT, LB_TESTC,  SEQ, DAY, change)
    result <- as.data.frame(ftable(labfinal1))                # multi-layer tables
    result1 <- dcast(result, LB_CAT + LB_TESTC + SEQ + DAY ~ change, value.var = "Freq")
    
    header <- c("LL", "LN", "LH", "NL", "NN", "NH", "HL", "HN","HH")
    t1 <- !(header %in%  names(result1))    # which columns did not show up 
    
    zeros <- data.frame(matrix(0, nrow = nrow(result1), ncol =sum(t1)))  # make those columns to be 0
    names(zeros) <- header[t1]
    result2 <- bind_cols(result1, zeros) %>% 
      mutate_if(is.factor, as.character)   
    # if the variable is factor, turn it to character, so result3 won't issue warning message
    
    result2 <- result2 %>% select(LB_CAT, LB_TESTC, SEQ, DAY, 
                                  LL, LN, LH, NL, NN, NH, HL, HN, HH) %>%
      filter( LL+ LN+ LH+ NL+ NN+ NH+ HL+ HN+ HH  != 0)
    
    # remove rows that have all zeros
    
    
  }
  
  else {                       # if it's UA test
    labfinal0 <- labfinal2 %>% filter(trimws(LB_CAT) == "UA")
    labfinal1 <- labfinal0 %>% select(LB_CAT, LB_TESTC,  SEQ, DAY, change)
    result <- as.data.frame(ftable(labfinal1))
    
    result1 <- dcast(result, LB_CAT + LB_TESTC + SEQ + DAY ~ change, value.var = "Freq")
    
    header <- c("NN", "NH", "HN", "HH")
    t1 <- !(header %in%  names(result1))    # which columns did not show up 
    
    zeros <- data.frame(matrix(0, nrow = nrow(result1), ncol =sum(t1)))  # make those columns to be 0
    names(zeros) <- header[t1]
    result2 <- bind_cols(result1, zeros) %>% 
      mutate_if(is.factor, as.character)
    
    result2 <- result2 %>% select(LB_CAT, LB_TESTC, SEQ, DAY, 
                                  NN, NH, HN, HH) %>%
      filter(NN + NH + HN + HH  != 0)
  }
  
  full_name <- laborig %>% select(LB_CAT, LB_CAT_D, LB_TEST, LB_TESTC) 
  
  result3 <- right_join(full_name %>% arrange(LB_CAT, LB_TESTC) %>% distinct(), 
                        result2 %>% arrange(LB_CAT, LB_TESTC), 
                        by = c("LB_CAT", "LB_TESTC"))
  
  return(result3)
}








#' Summarize the lab statistics
#'
#' @title summary statistics for lab shift.
#' @param laborig  the dataset returned by \code{create_laborig}.
#' @param included the dataset created by \code{create_included}. This is the same as the data "included" from sas
#' @param digits  how many digits should be kept for the final data.
#' @return the summary statistics by test code, time point and by treatment.
#' @export
#' @seealso \code{\link{create_laborig}}
#' @examples 
#' included <- create_included(ex, dm, cr, ds)
#' laborig <- create_labshift(lb_cq, included, ex)
#' s1 <- summary_lab(laborig, included)  #not urinalysis
#'

summary_lab <- function(laborig, included, digits = 3){
  
  lab1 <- laborig %>% filter(LB_ORLO != "" | LB_ORHI != "") %>% # remove the obs that are categorical (e.g., "NEGATIVE")
    mutate(outcome = as.numeric(LB_ORRES))
  
  # get the normal range values
  nr <- normal_range(laborig) %>% ungroup() 
  lab2 <- left_join(lab1 %>% arrange(LB_CAT, LB_TESTC), 
                    nr %>% select(-lgthrge) %>% arrange(LB_CAT, LB_TESTC), 
                    by = c("LB_CAT", "LB_TESTC"))
  
  # get the treatment information
  lab2_2 <- left_join(lab2 %>% arrange(CLIENTID),         
                      included %>% arrange(CLIENTID) %>% select(CLIENTID, SEQ, RSEQ), 
                      by = "CLIENTID")
  
  
  
  # take care of rechecks/early termination/ unscheduled events.
  lab2_3 <- lab2_2 %>% filter( !grepl("EARLY TERM", toupper(PERIOD))) %>%
    group_by(CLIENTID, LB_TEST, PERIOD, DAY, HOUR, LB_UNSCH) %>% 
    filter( (HOUR > 0 & row_number() == 1 )  |   # postdose: choose the first obs
              ((is.na(HOUR) | HOUR < 0) &  row_number() == n() ) )  #predose: choose the last obs
  
  
  # keep only baseline hour and postdose hours
  basehour <- guess_base_phour(lab2_2)
  lab2_4 <- left_join(lab2_3 %>% arrange(CLIENTID, PERIOD, LB_TEST, PHOUR), 
                      basehour %>% arrange(CLIENTID, PERIOD, LB_TEST, PHOUR), 
                      by = c("CLIENTID", "PERIOD", "LB_TEST", "PHOUR")) %>% 
    filter(status != "" | HOUR > 0) %>% 
    mutate(sortvar = ifelse(status != "", paste("Baseline:", PHOUR, sep = ""), 
                            paste("Postdose:", PHOUR, sep = "")))
  
  
  # statistical summaries
  lab3 <- lab2_4 %>%  group_by(LB_CAT, LB_TEST, range, PHOUR, sortvar, SEQ) %>% 
    summarise(COUNT = n(), 
              mean = mean(outcome),
              sd = sd(outcome),
              mininum = min(outcome),
              q1 = quantile(outcome, probs = 0.25),
              median = median(outcome), 
              q3 = quantile(outcome, probs = 0.75),
              maximu = max(outcome)) %>%
    mutate(cv = sd/mean)
  
  result <- gather(lab3, statistic, value, -LB_CAT, -LB_TEST, -range, -PHOUR, -sortvar, -SEQ) %>%
    spread(SEQ, value) %>% arrange(LB_CAT, LB_TEST, sortvar)
  
  result <- round_df(result, digits) %>% ungroup() %>% select(-sortvar)  # this function is included in the useful.R file.
  
  return(result)
}











################################################################################################
#                                  Lab -- out of range values
#
################################################################################################



# create the range variable, the input data should have at least the following 
# variables included: LB_ORLO, LB_ORHI, LB_TESTC, sex, LB_STNRC
create_range <- function(data){
  
  all_exist <- sum (names(data)  %in% c("LB_ORLO", "LB_ORHI", "LB_TESTC", "LB_SEX_C", "LB_STNRC")) == 5
  if(!all_exist) stop("at least one of the following \n LB_ORLO, LB_ORHI, LB_TESTC, LB_SEX_C, LB_STNRC\n does not exist or not named correctly, check your data") 
  
  low <- trimws(data$LB_ORLO)
  high <- trimws(data$LB_ORHI)
  testc <- trimws(data$LB_TESTC)
  sex <- trimws(data$LB_SEX_C)
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



#' Find the normal range of each test code
#'
#' @title Find normal range
#' @param laborig  the dataset generated by \code{create_laborig}
#' @return a data frame containing test codes and their corresponding normal range


normal_range <- function(laborig){
  
  ranges <- laborig %>% select(LB_CAT, LB_TESTC, LB_ORLO, LB_ORHI, sex, LB_STNRC) %>%
    arrange(LB_CAT, LB_TESTC, LB_ORLO, LB_ORHI, sex) %>%
    distinct()
  
  male <- ranges %>% filter( trimws(toupper(sex)) %in% c("MALE", "M")) %>%
    arrange(LB_CAT, LB_TESTC, LB_ORLO, LB_ORHI, LB_STNRC)
  female <- ranges %>% filter( trimws(toupper(sex)) %in% c("FEMALE", "F")) %>%
    arrange(LB_CAT, LB_TESTC, LB_ORLO, LB_ORHI, LB_STNRC)
  
  newrg <- full_join(male, female, by = c("LB_CAT", "LB_TESTC", "LB_ORLO", "LB_ORHI", "LB_STNRC"))
  
  # create a sex variable, if sex is not empty, then this testcode is specific for this gender.
  newrg$sex <- ""
  id1 <- is.na(newrg$sex.y); id2 <- is.na(newrg$sex.x)
  newrg$sex[!id1 & id2] <- newrg$sex.y[!id1 & id2]; newrg$sex[id1 & !id2] <- newrg$sex.x[id1 & !id2]
  newrg <- newrg %>% select(-sex.x, -sex.y)
  
  # creating the range variable seems to be a complicated data step, so I write a function to do it.
  tworg <- create_range(newrg)  # creates the range variable 
  
  tworg <- tworg %>% select(LB_CAT, LB_TESTC, sex, range ) %>% 
    arrange(LB_CAT, LB_TESTC, sex, range) %>% 
    mutate(sex = replace(sex, sex == "", "A"))
  
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



# #' Find the out-of-range values
# #' 
# #' @title Find OOR
# #' @param lb_cq  the SAS dataset.
# #' @param transpose   Should the data be transposed? FALSE by default (This corresponds to just filtering lb_cq by OOR conditions).
# #' @return a data frame containing OOR

oor_value <- function(lb_cq, transpose = F){
  
  oor  <- create_laborig(lb_cq) %>%                   #  all cases of out of range values
    filter(trimws(LB_NRIND) != "" |
             trimws(LB_CSGFG) != "" |
             trimws(LB_PISIG) != "" |
             trimws(LB_UNSCH) != "" |
             trimws(toupper(PERIOD)) == "EARLY TERMINATION" |
             trimws(toupper(PERIOD)) == "UNSCHEDULED")
  
  if (transpose) {
    id1 <-  trimws(toupper(oor$PERIOD)) == "EARLY TERMINATION" | trimws(toupper(oor$PERIOD)) == "UNSCHEDULED"
    oor$period_code <- oor$PERIOD
    oor$period_code[!id1] = ""
    
    oor <- oor %>% mutate(LB_ORRES = paste(     # compbine all the abnormal signals
      trimws(LB_ORRES), " ",
      trimws(LB_NRIND), "",
      trimws(LB_CSGFG), "",
      trimws(LB_PISIG), "",
      trimws(LB_UNSCH), " ", 
      trimws(period_code), sep = ""),
      LB_TESTC = paste(trimws(LB_CAT), trimws(LB_TESTC), sep = "_")) %>%
      select(ptno, sex, LB_DAT,LB_TIM, LB_TESTC, LB_ORRES, PERIOD, DAY, HOUR) %>%
      dcast(ptno + sex + PERIOD + DAY + HOUR + LB_DAT + LB_TIM ~ LB_TESTC, value.var = "LB_ORRES")     
    
  }
  
  return(oor)
  
}


##  read the full name of the codes and it's normal range indicator

the_code <- function(lb_cq){
  
  lab_cat <- lb_cq %>% select(LB_CAT, LB_TESTC) %>% 
    arrange(LB_CAT, LB_TESTC) %>% distinct()
  
  # the codes data is available once you load the pacage "cdashQC".
  code1 <- right_join(codes %>% arrange(LB_TESTC), 
                      lab_cat %>% arrange(LB_TESTC), by = "LB_TESTC")
  
  # change "~" to " " in TTL, this will make the column names wrap
  # create a new variable for LB_TESTC for easy selection
  code1 <- code1 %>% mutate(var_name = paste(LB_CAT, "_", LB_TESTC, sep=""),
                            TTL = gsub("~", " ", TTL))     
  # and also the normal ranges         
  range0 <- create_laborig(lb_cq) %>% normal_range() %>% ungroup()
  
  if (length(unique(range0$sex)) > 1) {             # more than one gender is present
    ranges <- range0  %>% dcast(LB_CAT + LB_TESTC ~sex, value.var= "range")
    ids <- is.na(ranges$A)
    ranges$A[ids] <- paste(ranges$FEMALE[ids], ranges$MALE[ids], sep = " ")
    ranges <- ranges %>% mutate(range = A) %>% select(LB_CAT, LB_TESTC, range)
  }
  else { # only one gender
    ranges <-  range0 %>% select(LB_CAT, LB_TESTC, range)
  }
  
  
  # the full name will come with its normal range
  code2 <- left_join(code1 %>% arrange(LB_CAT, LB_TESTC), 
                     ranges %>% arrange(LB_CAT, LB_TESTC), 
                     by = c("LB_CAT", "LB_TESTC")) %>%
    mutate(TTL = paste(TTL, range, sep = " "))
  
  
  return(code2)
}



##  I don't know what this function is doing

oor_range <- function(lb_cq){
  
  laborig  <- oor_value(lb_cq)  # all cases of out of range values
  
  new0 <- laborig %>% select(LB_CAT, LB_TESTC, LB_ORRES, LB_NRIND, LB_CSGFG, LB_PISIG)  %>%
    mutate(lgth = nchar(LB_ORRES))
  
  # This data set contains the length that each test code requires (by its actual value)
  new1 <- new0 %>% arrange(LB_CAT, LB_TESTC, lgth) %>% 
    distinct() %>%  # select distinct values
    group_by(LB_CAT, LB_TESTC, lgth) %>%
    filter(row_number() == n())  %>%  # keep rows having max(lgth) in each group
    select(-LB_ORRES)                 # drop LB_ORRES
  
  
  lb_orlog <- normal_range(laborig)                 # get the normal range values 
  
  new <- left_join(new1 %>% arrange(LB_CAT, LB_TESTC), 
                   lb_orlog %>% arrange(LB_CAT, LB_TESTC), 
                   by = c("LB_CAT", "LB_TESTC") )
  new$lgth <- ifelse(new$lgthrge > new$lgth, new$lgthrge, new$lgth )
  
  #  pulling in the dataset from labcodes.mac to determine the title names 
  #  and the column width for this test in labcodes.mac.  If this width is 
  #  not as long as the longest lb_orres or range, the width is replaced with the   
  #  longest lb_orres for your lab dataset     
  
  # codes <- read.csv("Y:/development/users/Zhuob01/R-template/data/CODES.csv",stringsAsFactors=F) %>% 
  #             filter(lb_testc != "")
  # names(codes) <- toupper(names(codes))
  # codes$LB_TESTC <- as.character(codes$LB_TESTC)
  
  test <- right_join(codes %>% arrange(LB_TESTC), new, by = "LB_TESTC")
  test$COLWIDE <- ifelse(test$lgth > test$COLWIDE |  is.na(test$COLWIDE), test$lgth, test$COLWIDE)
  
  codes2 <- test %>% mutate(colwd = COLWIDE + 4) %>% 
    group_by(LB_CAT, TESTNUM, TTL) 
  
  result <- list(laborig= laborig, codes = codes2)
  
  return(result)
}



# lb_orreu <- toupper(trimws(laborig$LB_ORREU))
# id1 <-  lb_orreu == "X 10^9/L" | lb_orreu == "X10^9/L"
# id2 <-  lb_orreu == "X 10^12/L" | lb_orreu == "X10^12/L"
# id3 <- lb_orreu != ""
# 
# one0 <- laborig %>% mutate( LB_ORREU = replace(LB_ORREU, id1, "thou/uL"), 
#                             LB_ORREU = replace(LB_ORREU, id2, "mil/uL"))
# one0$LB_ORREU[id3] <-  paste("(", trimws(one0$LB_ORREU[id3]), ")", sep = "")  
#
# id5 <-  trimws(toupper(PERIOD)) != "EARLY TERMINATION" & trimws(toupper(PERIOD)) != "UNSCHEDULED"
#
# one1 <- one0 %>% select(LB_TESTC, LB_ORREU, LB_TEST)  %>%
#                  arrange(LB_TESTC, LB_ORREU, LB_TEST) %>%
#                  distinct()
# 
# one <- left_join(one1, codes %>% arrange(LB_TESTC), by = "LB_TESTC") %>% 
#         mutate(COLWIDE = replace(COLWIDE, is.na(COLWIDE), 8) ) 
# id4 <- which(is.na(one$TTL))
# one$TTL[id4] <- one$LB_TEST[id4]
# one_update <- one %>% arrange(LB_TESTC, LB_TEST, LB_ORREU, TESTNUM, TTL, LB_CAT, COLWIDE)



# #' Export the oor values
# #' 
# #' @title Export the oor values by LB_CAT
# #' @param lb_cq  the dataset lb_cq read from sas
# #' @param ex  the dataset ex, used to get treatment information.
# #' @return a data frame containing test codes and their corresponding normal range


lab_rept <- function(lb_cq, ex, cat = "UA"){
  
  oor_all <- oor_value(lb_cq, transpose = T) # all cases of out of range values
  
  # get the treatment information
  ex_trt <- ex %>% mutate(ptno = as.numeric(CLIENTID))  %>% select(ptno, EX_TRT_C) 
  
  oor_value <- right_join(ex_trt %>% arrange(ptno, EX_TRT_C), 
                          oor_all %>% arrange(ptno), by = "ptno")
  
  # decide whether this CAT has oor listed in data 
  var_names <- names(oor_value)
  cat_exist <- grepl(paste(cat, "_", sep = ""), var_names)
  
  if (any(cat_exist)){
    oor <- oor_value %>% select(ptno, sex, EX_TRT_C, PERIOD, DAY, HOUR, LB_DAT, LB_TIM, starts_with(paste(cat, "_", sep = "")))
    
    return(oor)
  }
  
  else { message(paste("Whoops! ", cat, " does not have out-of-range values  \nvariable OR 'LB_CAT' does not contain the value '", cat, "'", sep = ""))}
  
}



# #' print out the OOR values
# #' 
# #' @title print oor values by LB_CAT
# #' @param lb_cq  the dataset lb_cq read from sas
# #' @param ex  the dataset ex, used to get treatment information.
# #' @param cat which LB_CAT code will be used ("UA" for example)
# #' @param var_per_block  how many variables to be shown in each block
# #' @param digit_keep  the digit kept in the output. see \code{kable()} for more details
# #' @export


print_by_block <- function(data, col_label = names(data),  cat = "UA", 
                           var_per_block = 5, digit_keep = 2){
  
  # first, separate the data into two, one for fixed columns (those columns will be the same for all output)
  part1 <- data %>% select(-starts_with( paste(cat, "_", sep = "")))
  part2 <- data %>% select(starts_with( paste(cat, "_", sep = "")))
  the_rest_name <-  names(part1)
  cat_name <-  names(part2)
  
  ## devivde the variables into multiple blocks and print them block by block.
  block <- ceiling(length(cat_name)/var_per_block)  # how blocks are needed
  
  for ( k in 1:block){
    if(k == block){                        # if this is the last block
      from1 <- (k-1)*var_per_block + 1
      to1 <- length(cat_name)
    }
    else{         # if this is not the last block
      from1 <- (k-1)*var_per_block + 1
      to1 <-  k*var_per_block;
    }
    # c(from1, to1)
    
    # get the corresponding code
    test_code <- as.character(cat_name[from1:to1])
    col_to_show <- which(cat_name %in% test_code)
    
    part2_1 <- part2[, col_to_show]          # keep those names in this output
    emt <- keep_non_empty(part2_1)           # get the index of empty columns and empty rows
    part2_keep <- part2_1[, emt$keep_cols]   # remove empty columns
    id1 <- is.na(part2_keep)                 # replace "NA" with ""
    part2_keep[id1] <- ""
    oor1 <- cbind(part1, part2_keep)         # remove empty rows
    oor1 <- oor1[emt$keep_rows, ]
    
    # now the column names with desired lables
    p1 <- ncol(part1)
    col_num <- c(1:p1, p1 + from1:to1)
    
    col_name <- col_label[col_num]
    rownames(oor1) <- NULL
    # print out
    
    
    print(kable(oor1, digits =digit_keep, table.attr='class="flat-table"',
                col.names = col_name, caption = paste(cat, ":block ", k, " of ", block, sep = "")))
    
  }
  
  
}







#' print out the OOR values for all LB_CAT automatically. This function is built on top of customized_lab_rept()
#' 
#' @title print OOR values for all LB_CAT
#' @param lb_cq  the dataset lb_cq read from sas
#' @param ex  the dataset ex, used to get treatment information.
#' @param var_per_block  how many variables to be shown in each block
#' @param digit_keep  the digit kept in the output. see \code{\link[knitr]{kable()}} for more details
#' @param prt  Whether to print the results or not. Default is \code{TRUE}.
#' @export
#' @examples
#' listing_lab_oor(lb_cq, ex, var_per_block = 6, digit_keep =3)


listing_lab_oor <- function(lb_cq, ex, var_per_block = 5, digit_keep = 2, prt = TRUE){
  
  laborig <- create_laborig(lb_cq)
  cat_all <- unique(laborig$LB_CAT)  # how many LAB_CAT
  full_name <- the_code(lb_cq)       # the code with its full name and normal ranges
  
  result <- list()
  indcat <- rep(F, length(cat_all))
  for (i in 1: length(cat_all)){
    
    oor <- lab_rept(lb_cq, ex, cat=cat_all[i])  # create the lab oors
    
    
    if(!is.null(oor)){  # if the data is not empty
      if(prt) {     # if you want to print it out in html
        var1 <- names(oor)   # 
        id1 <- which(grepl(paste(cat_all[i], "_", sep = ""), var1))
        
        #try to get the full name in order
        t1 <- data.frame(var_name = var1[id1], sorted = 1:length(id1), stringsAsFactors = F)
        t2 <- left_join(t1 %>% arrange(var_name), 
                        full_name %>% arrange(var_name), by = "var_name")
        # this should give the right name order
        col_name_all <- c(var1[-id1], t2$TTL[order(t2$sorted)] )
        # pass the name to the following function
        
        print_by_block(oor, col_label = col_name_all, cat= cat_all[i], 
                       var_per_block=var_per_block, digit_keep = digit_keep)
      }
      
      else {   # if you do not want to print, save it as a list.
        result[[i]] <-  oor
        indcat[i] <- T
      }
    }
    
  }
  if (!prt){
    names(result) <- cat_all[indcat]
    return(result)}
  
}








