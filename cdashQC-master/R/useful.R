




##########################################################################################
#                                   USEFUL FUNCTIONS
#
##########################################################################################


# read as many data sets as needed.
#
# @title race and ethnicity indicator, age.
# @param data_vector  the name list of the data sets to be read
# @param data_dir  where the data sets are stored
# @return datasets  the data sets have been loaded
# @export a list, each corresponding to a specific data set
#
#
# read_all_data <- function(data_vector, data_dir){
#   data_name <- c("ae", "bd", "cm", "cr", "cv", "dm", "ds", "eg", "ex", "hlt", "ie", "lb_cq",
#                  "lbd", "mh", "ml", "pe", "su", "ti", "ur", "vs", "xd")
#   data <- list()
#   ids <- which(data_name %in% data_vector)
#   for(i in 1:length(ids)){
#     per_data <- read_sas(paste(data_dir, data_name[ids[i]], ".sas7bdat", sep = ""))
#     data[[i]] <-  per_data
#   }
#   names(data) <- data_vector
#   return(data)
# }


# create a cumalative count for each variable
# so I can label "QTCF" as fig1QTCF, ... fig4QTCF, and fig1PR, fig2PR
cumcount <- function(vec_c)
{
  vec_c <- as.character(vec_c)
  seqs <- rep(0, length(vec_c))
  items <- unique(vec_c)
  tab <- table(vec_c)
  for (k in 1:length(items))
  {
    c1 <- as.numeric(tab[k])
    seqs[vec_c == names(tab)[k]] <- 1:c1
  }
  return(seqs)
}



#' falg variables by threshold
#'
#' @title label the variables of interest.
#' @param data  the dataset to be flagged.
#' @param thresh  a data frame returned by \code{create_threshold()}
#' @param prefix how will the flag variable be named (by adding a prefix).
#' @param oor  is the current variable to be labeled for out of range?
#' @return the data with variables flagged
#' @export
#' @examples
#' a <- create_threshold(flagvar = "PR", lower = 150, upper = 200, include_lower = FALSE, include_upper = TRUE,  flg_label = "^")
#' b <- create_threshold(flagvar = "QTCF",  upper = 430, flg_label = "*", add2existing = TRUE, thresh = a)
#' data <- data.frame(PR = rnorm(10, 175, 25), QTCF = rnorm(10, 440, 20))
#' flg_var(data, thresh=b, oor=c(TRUE, FALSE))
#' 
flg_var <- function(data, thresh, prefix = "flg", oor= rep(F, nrow(thresh))){

  n_var <- nrow(thresh)

  seqs <- cumcount(thresh[, 1])
  for (i in 1:n_var)
  {
    col_num <- which(trimws(toupper(names(data)))== toupper(thresh[i, 1]))  # column to be flagged


    # flg only extreme values?
    if (oor[i]){  # flag if a <  c1 or a > c2
      ind_1 <- ifelse(thresh[i, 4], "<=", "<")
      ind_2 <- ifelse(thresh[i, 5], ">=", ">")
      
      message(paste("flagging variable", thresh[i, 1],  "if it's", ind_1, 
                    thresh[i, 2], " or ", ind_2, thresh[i, 3]))

      # for lower bound
      if (thresh[i, 4]){
        ind_low <- data[, col_num] <= thresh[i, 2]
      }
      else { ind_low <- data[, col_num] < thresh[i, 2] }

      # for upper bound
      if (thresh[i, 5]){
        ind_upper <- data[, col_num] >= thresh[i, 3]
      }
      else { ind_upper <- data[, col_num] >= thresh[i, 3] }


      row_num <- c(which(ind_low), which(ind_upper))
      ncols <- ncol(data) + 1
      data[, ncols] <- ""
      data[row_num, ncols] <- as.character(thresh[i, 6])
    }


    else {   # flag when c1 < a < c2
      
      ind_1 <- ifelse(thresh[i, 4], "<=", "<")
      ind_2 <- ifelse(thresh[i, 5], "<=", "<")
      
      
      message(paste("flagging variable", thresh[i, 1],  "if", thresh[i, 2], ind_1, thresh[i, 1], 
              ind_2, thresh[i, 3]))

      # for lower bound
      if (thresh[i, 4]){
        ind_low <- data[, col_num] >= thresh[i, 2]
      }
      else { ind_low <- data[, col_num] > thresh[i, 2] }

      # for upper bound
      if (thresh[i, 5]){
        ind_upper <- data[, col_num] <= thresh[i, 3]
      }
      else { ind_upper <- data[, col_num] < thresh[i, 3] }

      row_num <- intersect(which(ind_low), which(ind_upper))
      ncols <- ncol(data) + 1
      data[, ncols] <- ""
      data[row_num, ncols] <- as.character(thresh[i, 6])
    }

    names(data)[ncols] <- paste(prefix, seqs[i], tolower(thresh[i, 1]), sep = "")

  }
  return(data)
}





#' remove empty rows and empty columns of a data set
#'
#' @title remove empty rows and empty columns of a data set.
#' @param data  the dataset
#' @param pattern  the pattern to be considered as empty. \code{NA} by default.
#' @param return_truncated_data Controls the output.
#' @return a data frame with empty rows and empty columns removed,  or a list containing the following elements.
#' \item{keep_rows}{a vector of \code{TRUE} (for keeping row) or \code{FALSE} (otherwise)}
#' \item{keep_rows}{a vector of \code{TRUE} (for keeping column) or \code{FALSE} (otherwise)}
#' @export
#' @examples 
#' data1 <- data.frame(x = rep(c("A", NA, "C"), each = 3), y = NA, z = c(1:3, NA, 5:9) )
#' keep_non_empty(data1, return_truncated_data = T)   
#' d1 <- keep_non_empty(data1)

keep_non_empty <- function(data, pattern = NA, return_truncated_data = F){

  col_total <- ncol(data)
  row_total <- nrow(data)
  if(is.na(pattern)){             # if want to remove NA
    if (col_total == 1) {         # if there is only one column
      keep_rows <- !is.na(data)
    }
    else {
      keep_rows <- rowSums(is.na(data)) < col_total
    }
    if(row_total == 1){
      keep_cols <- !is.na(as.vector(data))
    }
    else {
      keep_cols <- colSums(is.na(data)) < row_total
    }
  }

  else {     # if want to remove pattern , for example "" blank space
    data_new <- apply(data, 2, trimws)
    if (col_total == 1) {     # if there is only one column
      keep_rows <- data_new == pattern
    }
    else {
      keep_rows <- rowSums(data_new == pattern) < col_total
    }
    if(row_total == 1){
      keep_cols <- data_new == pattern
    }
    else {
      keep_cols <- colSums(data_new == pattern) < row_total
    }
  }


  if(return_truncated_data){
    data_result <- data.frame(data[keep_rows, keep_cols])
    names(data_result) <- names(data)[keep_cols]
    return(data_result)
  }
  else{
    return(list(keep_rows = keep_rows, keep_cols = keep_cols))
  }
}




#' create threshold data frame
#'
#' @title create threshold table (for flagging variable purpose).
#' @param flagvar  the variable to be flagged
#' @param lower   lower bound. Set to be \code{-Inf} if none.
#' @param upper  upper bound. Set to be \code{Inf} if none.
#' @param include_lower whether the lower bound should be included, \code{TRUE} by default
#' @param include_upper whether the upper bound should be included, \code{FALSE} by default
#' @param flg_label what label will be used to flag the variable
#' @param add2existing  \code{TRUE} or \code{FALSE}. If \code{TRUE}, must specify thresh.
#' @param thresh  a data frame with three columns: first column, the variable to be flagged, second column, lower bound (if any); third column, upper bound (if any).
#' @return a data frame
#' @export
#' @examples
#' a <- create_threshold(flagvar = "QTCF", lower = 0, upper = 20, include_lower = FALSE, include_upper = TRUE,  flg_label = "^")
#' b <- create_threshold(flagvar = "PR",  upper = 430, flg_label = "*", add2existing = TRUE, thresh = a)
#' c <- create_threshold(flagvar = "QRs",  lower = 100, flg_label = "%",add2existing = TRUE, thresh = b)
#' print(a); print(b);print(c);


create_threshold <- function(flagvar, lower = -Inf, upper = Inf,
                             include_lower = T, include_upper = F,
                             flg_label = "#", add2existing= F, thresh = NULL){
  if (!add2existing)
  {
    thresh1 <- data.frame(variable = flagvar, lower = lower, upper = upper,
                          include_lower = include_lower,
                          include_upper = include_upper,
                          flg_label=flg_label)
  }
  else {
    nx <- nrow(thresh)
    newthresh <- data.frame(variable = flagvar, lower = lower, upper = upper,
                            include_lower = include_lower,
                            include_upper = include_upper,
                            flg_label=flg_label)
    names(newthresh) <- names(thresh)
    thresh1 <- rbind(thresh, newthresh)
  }
  return(thresh1)
}




## 
# create protocol hour (PHOUR) if it's not already in the data set. 

create_phour <- function(data){
  
  if (!any(names(data) == "PHOUR")) {
    message("Variable 'PHOUR' not detected, creating it by concatnating variables 'DAY' and 'HOUR'")
    data <- data %>% mutate(PHOUR = paste("Day", DAY, "Hour", round(HOUR, 2)))  
  }
  
  return(data)
  
}







# get the summary statistics
#' @title get summary statistics
#' @param data the data 
#' @param group which column should be the group 
#' @param var which columns are used to summarize
#' @param na.rm should missing value be removed? \code{TRUE} by default.
#' @return a data frame
#' @export 
#' @examples 
#' SEQ = rep(c("A", "B", "C"), 3); subtype = sample(c("ONE", "TWO", "THREE"), 9, replace = TRUE)
#' data <- data.frame(SEQ, subtype, BMI = rnorm(9, 25, 4), HEIGHT = rnorm(9, 175,3)) 
#' get_summary_stats(data, group = "SEQ", var = "subtype")
#' get_summary_stats(data, group = "SEQ", var = "BMI")

get_summary_stats <- function(data, group = "EX_TRT_C", var = "race", na.rm =TRUE){

  var_col <- which( names(data)== var)
  var_attrib <- is.numeric(as.vector(data[, var_col]))
  
  if(!var_attrib){   # if it's not numerical, treate it as categorical 
    
    result <- as.data.frame(ftable(data %>% select_(var, group))) %>% 
      spread_(group, "Freq") %>% mutate(trait = toupper(var))
    names(result)[1] <- "type"
    
    t1 <- result %>% select(trait, type) 
    t2 <- result %>% select(-trait, -type)
    
    result1 <- bind_cols(t1, t2) %>% mutate_if(is.factor, as.character)
    
  }
  else{  # if it's numerical 
    result <- data %>% select_(var, group) %>% 
      group_by_(group) %>% 
      summarise_(N = interp(~sum(val >0, na.rm = na.rm), val = as.name(var)), 
                 MEAN = interp(~mean(val, na.rm = na.rm), val = as.name(var)),
                 SD = interp(~sd(val, na.rm = na.rm), val = as.name(var)), 
                 MINIMUM = interp(~min(val, na.rm = na.rm), val= as.name(var)), 
                 MEDIAN = interp(~median(val, na.rm = na.rm), val = as.name(var)), 
                 MAXIMUM = interp(~max(val, na.rm= na.rm), val =as.name(var)))
    
    result0 <- result %>% melt(id = group, variable.name = "type") %>%
      spread_(group, "value")  %>%  mutate(trait = var)
    
    p1 <-  result0 %>% select(trait, type)
    p2 <-  result0 %>% select(-trait, -type)
    result1 <- bind_cols(p1, p2) 
    
  }
  
  return(result1)
}


#' count to percent
#' 
#' @title add percentage to a frequency table
#' @param data the data should contain at least 2 columns
#' @param var1 which column should be the group 
#' @param var2 which columns are used to calculate percentage (could be a vector)
#' @param digit_keep how many digits should be kept. 
#' @return a data frame
#' @export
#' @examples 
#' trait = rep(c("A", "B", "C"), 3); subtype = paste(trait, rep(1:3, each=3), sep = "")
#' data <- data.frame(trait, subtype, count1 = rpois(9, 5), count2 = rpois(9, 10)) 
#' count_percent(data, var1= 1, var2 = 3:4)
#' 
#' r1 <- get_summary_stats(eg, group = "PERIOD", var = "EG_TEST")
#' count_percent(r1, var1 = 1, var2 = 3:5)
count_percent <- function(data, var1, var2, digit_keep = 3){

  data <- data[order(data[, var1]), ] # sort by the group variable 
  trait <- unique(data[, var1] )
  ntrait <- length(trait)
  result <- data.frame(matrix(NA, nrow = 0, ncol=length(var2))) 
  names(result) <- names(data)[var2]
  
  for ( i in 1:ntrait){
    if (length(var2) > 1){
      p1 <- as.matrix(data[data[, var1]==trait[i], var2])
      pct <- sweep(p1, 2, colSums(p1), "/")   # convert to percentage
      ctpct <- paste(p1, " (", round(pct, digit_keep)*100, "%)", sep = "")
      ctpct <- data.frame(matrix(ctpct, nrow=nrow(p1), ncol= ncol(p1), byrow = F))
    }
    else {
      p1 <- as.vector(data[data[, var1]==trait[i], var2])
      pct <- p1/sum(p1)
      ctpct <- paste(p1, " (", round(pct, digit_keep)*100, "%)", sep = "")
      ctpct <- data.frame(ctpct)
    }
    names(ctpct) <- names(data)[var2]
    result <- rbind(result, ctpct)
  }

  r1 <- as.data.frame(data[, -var2])
  names(r1) <- names(data)[-var2]
  
  result1 <- dplyr::bind_cols(r1, result)
  return(result1)
}



