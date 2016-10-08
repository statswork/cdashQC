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


# #' Create baseline data
# #' @title create a baseline from lb_cq.
# #' @param laborig  the dataset returned by \code{create_lb_cq()}.
# #' @return the baseline data set.
# #'


# Step 2: Get the baselines  ----------------------------------------------

create_lab_baseline <- function(laborig){

  temp1 <- laborig
  ## construct the baseline------------------------
      # separate the test codes into two categories: 1. negative hours as baseline, 2. missing hours as baseline
      # output: a data frame with first column being
      baseline_hour <- function(temp1){
        testc<- temp1 %>% select(LB_CAT, LB_TESTC) %>% distinct()  # all the test codes
        testc_Negative <- rep(F, nrow(testc)) # create an indicator to specify whether this code has negative hours as baseline
        for( i in 1: nrow(testc)){
          hours <- unique(temp1$HOUR[temp1$LB_TESTC == testc$LB_TESTC[i]])
          if (any(hours<0, na.rm = T)) {testc_Negative[i] <- T }
          else {testc_Negative[i] <- F}
        }
        testc$negativeHour <- testc_Negative
        data.frame(testc)
      }


  baseline_code_index <- baseline_hour(temp1)

  # temp2 has one more column than temp1, which indicates whether we should use NA or negative hours as baseline hours
  temp2 <-  inner_join(temp1 %>% arrange(LB_CAT, LB_TESTC),
                       baseline_code_index %>% arrange(LB_CAT, LB_TESTC),
                       by =c("LB_CAT", "LB_TESTC"))

  # subset to have negative hours as baseline
  baseline1 <-  temp2 %>% filter(negativeHour==T) %>%
                  filter((HOUR < 0  & !is.na(HOUR)) & trimws(LB_ORRES) != "") %>%
                  mutate(period_baseline = PERIOD, basehour = HOUR) %>%
                  group_by(LB_CAT, LB_TESTC, ptno, period_baseline) %>%
                  filter(row_number()== n())  %>%  # select last.obs, equivalent to if last.hour
                  mutate(bl = trimws(LB_NRIND), basehour= HOUR) %>%
                  ungroup() %>%
                  select(LB_CAT, LB_TESTC, ptno, period_baseline, bl, basehour)

  # subset to have NA hours as baseline
  baseline2 <- temp2 %>% filter(negativeHour==F) %>%
                  filter(is.na(HOUR) & trimws(LB_ORRES) != "") %>%
                  mutate(period_baseline = PERIOD, basehour = HOUR) %>%
                  group_by(LB_CAT, LB_TESTC, ptno, period_baseline) %>%
                  filter(row_number()== n())  %>%  # select last.obs, equivalent to if last.hour
                  mutate(bl = trimws(LB_NRIND)) %>%
                  select(LB_CAT, LB_TESTC, ptno, period_baseline, bl, basehour)

  # this is the baseline
  baselines <- bind_rows(baseline1, baseline2)

  return(baselines)
}


# Step 3: Get the postdoes  ----------------------------------------------

# #' Construct postdose data
# #' @title create a postdose from lb_cq.
# #' @param laborig  the dataset returned by \code{create_lb_cq()}.
# #' @return the baseline data set.


create_lab_postdose <- function(laborig){
  ## post dose
  temp1 <- laborig
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
#'


labshift <- function(lb_cq, included, ex, UA=F){

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



# lab <- create_labshift(lb_cq, ex, included)
# tab <- labshift(lab, UA=F)
# tab2 <- labshift(lab, UA=T)

## example of selecting first.obs
# dat <- data.frame(a = rep(1, 10), b = rep(c(1, 2), each= 5), c = c(rep(1, 3), rep(2, 3), rep(3, 3), 4), d= rnorm(10))
# dat %>% group_by(a, b, c) %>%
#   filter(row_number(c)==1 )
#
#
# dat %>% group_by(a, b) %>% filter(row_number(b) == n())
# dat


