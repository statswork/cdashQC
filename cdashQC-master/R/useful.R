
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

#

# get the mode of a vector
calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
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

  col_attrib <- sapply(data, class)
  var_attrib <- col_attrib[names(col_attrib) == var] == "numeric"
  
  if(var_attrib==F){   # if it's not numerical, treate it as categorical 
    
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




## round the values if it's numerical 

#' round a data frame if the column is numerical
#' @title round data frame
#' @param df the data frame
#' @param digits how many digits you want to keep
#' @return a data frame with numerical columns rounded.
#' @export

round_df <- function(df, digits = 3){
  
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[, nums] <- round(df[, nums], digits = digits)
  return(df)
  
}



#' Format the time variable in a data set
#' 
#' @titile format time variable in a data set
#' @param data the data which contains the time variables
#' @param date the date variable
#' @param time the time (in seconds)
#' @param newname what should the new variable that concatnates date and time be named?
#' @return the same data set but \code{time} has been changed to "hh:mm:ss" format.
#' @export 
#' 
#'

format_time <- function(data, date = "AE_STDT", time = "AE_STTM", newname = paste(date, time, sep= "_")){
  
  col_id <- which(names(data) %in% c(date, time))
  
  names(data)[col_id] <- c("col1", "col2")
  
  dat1 <- data %>% mutate(newtime = as.POSIXct(as.numeric(col2), origin = col1, tz = "GMT")) %>%
                   mutate(col2 = format(newtime, "%H:%M:%S")) 
  
  names(dat1)[c(col_id, ncol(dat1))] <- c(date, time, newname)  

  return(dat1)
}

  



