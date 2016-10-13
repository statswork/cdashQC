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

#' Clean the lb_cq data
#' @title prepare data for lab shift table.
#' @param lb_cq  the dataset lb_cq read from sas
#' @param ex  the dataset ex read from sas
#' @param included  the dataset created by \code{new_create_included()}. This is the same as the data "included" from sas
#' @return a data frame
#' @export
#' @seealso \code{\link{new_create_included}}
#'


create_lb_cq <- function(lb_cq, included, ex){
  
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
# #' @param laborig  the dataset returned by \code{create_lb_cq()}.
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
#' @param lb_cq  the dataset lb_cq read from sas
#' @param ex  the dataset ex read from sas
#' @param included  the dataset included from sas (need variable SEQ)
#' @param UA  whether to produce shift table for UA (should be done separately)
#' @return the shift table
#' @export
#' @examples 
#' included <- new_create_included(ex, dm, cr, ds)
#' others <- labshift(lb_cq, included, ex,  UA=FALSE)  #not urinalysis
#' UA <- labshift(lb_cq, included, ex, UA=TRUE)       # UA
#'


labshift <- function(lb_cq, included, ex, UA= FALSE){
  
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

















