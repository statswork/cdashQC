### old functions


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


