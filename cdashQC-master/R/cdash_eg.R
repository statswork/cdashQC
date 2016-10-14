################################################################################################
#                                               ECGs
#
################################################################################################


#' dcast the ecg variables
#' 
#' @title create variable columns for the ECGs parameters.
#' @param eg  the dataset eg read from sas
#' @return eg1 the transposed data 
#' @export
#' 

create_eg <- function(eg){
  
  #  if there's no PHOUR variable then create it 
  eg <- create_phour(eg)
  
   # do the transpose
   eg_value <- eg %>% filter(trimws(EG_TEST) != "OVERALL INTERPRETATION") %>%
     select(CLIENTID, PERIOD, PHOUR, DAY, HOUR, EG_TEST, EG_ORRES, EG_DAT, EG_TIM) %>% # choose columns
     mutate(EG_ORRES = as.numeric(EG_ORRES)) %>%   # charactor to numerical
     dcast(CLIENTID + PERIOD + PHOUR +  DAY + HOUR + EG_DAT + EG_TIM ~ EG_TEST, value.var = "EG_ORRES") %>% # transpose
     arrange(CLIENTID, PHOUR, DAY, HOUR, EG_DAT, EG_TIM)  # sort them 
   
   # get the specifications
   eg_spec <- eg %>% filter(trimws(EG_TEST) ==  "OVERALL INTERPRETATION") %>%
          select(CLIENTID, PHOUR,  DAY, HOUR, EG_DAT, EG_TIM, EG_ORR_D, EG_SPEC) %>%
          arrange(CLIENTID, PHOUR, DAY, HOUR, EG_DAT, EG_TIM)
  
    eg1 <- inner_join(eg_value, eg_spec, 
              by = c("CLIENTID", "PHOUR", "EG_DAT", "EG_TIM", "HOUR", "DAY") ) %>%
            arrange(CLIENTID, PHOUR,  EG_DAT, EG_TIM, HOUR, DAY) %>% 
            mutate(egdate = parse_date_time(paste(ymd(EG_DAT), seconds_to_period(EG_TIM)), "Ymd HMS", truncated= 3),
                   HOUR = round(HOUR, 2))
      
    eg1 <- eg1 %>% mutate(EG_TIM = format(egdate, "%H:%M:%S")) %>% select(-egdate)
   
    return(eg1)
}




## By now you should have collected all the replicates for each PHOUR  correctly
#' Get the averages of the replicates
#' 
#' @title get the averanges of the replicates
#' @param data  an object returned from \code{replicate_clean}
#' @param var the variables used to do the calculation
#' @param prefix give a prefix to \code{var} so the names are changed after calculating the averages
#' @return the averages
#' @export
#' @examples
#' eg2 <- replicate_data(eg) # find the triplicates
#' eg_prob <- eg2$data_dirty         # need manual check
#' # the following rows should be removed
#' rows_removed <- c(2, 4, 7, 13, 14, 15, 19, 25, 28, 32, 37, 40, 44, 52, 57, 59, 64, 65)
#' eg3 <- replicate_clean(eg2, rows_removed)
#' ave <- replicate_average(eg3, prefix = "Ave")
#' @seealso \code{\link{replicate_clean}}

replicate_average <- function(data, var = c("HR", "PR", "QRS", "QT", "QTCF"), prefix= "Base"){
  names(data) <- toupper(names(data))
  var_of_interest <- data %>% select(CLIENTID, PERIOD, PHOUR, 
                                     one_of(var))    # how to select a column whose name is quoted.
  averages <- var_of_interest %>% group_by(CLIENTID, PERIOD, PHOUR) %>%
    summarize_each(funs(mean))
  
  id_col <- names(averages) %in% var
  names(averages)[id_col] <- paste(prefix, var, sep = "")
  
  return(averages)
  
}



## By now you should have collected all the replicates for each PHOUR  correctly
#' Get the averages of the replicates
#' 
#' @title calculate the change from baselines
#' @param data  An object returned from \code{replicate_clean}
#' @param var the variables used to do the calculation
#' @return a data frame
#' @examples
#' eg2 <- replicate_data(eg) # find the triplicates
#' eg_prob <- eg2$data_dirty             # need manual check
#' # the following rows should be removed
#' rows_removed <- c(2, 4, 7, 13, 14, 15, 19, 25, 28, 32, 37, 40, 44, 52, 57, 59, 64, 65)
#' eg3 <- replicate_clean(eg2, rows_removed)
#' baseline <- create_baseline(eg) %>%          # get the baseline hours
#'                  select(CLIENTID, PERIOD, PHOUR, status) %>% distinct()          
#' eg4 <- left_join(eg3 %>% arrange(CLIENTID, PERIOD, PHOUR), 
#'                  baseline %>% arrange(CLIENTID, PERIOD, PHOUR), 
#'                  by = c("CLIENTID", "PERIOD", "PHOUR"))
#' eg_change <- change_from_base(eg4, var = c("HR", "PR", "QRS", "QT", "QTCF"))
#' @export
#' @seealso \code{\link{replicate_clean}}  
#' @seealso \code{\link{replicate_data}}
#' @seealso \code{\link{create_eg}}
#' 

change_from_base <- function(data, var = c("HR", "PR", "QRS", "QT", "QTCF")){
  
  baseline <- data %>% filter(status == "BASELINE")
  postdose <- data %>% filter(status == "POSTDOSE")
  
  base_ave <- replicate_average(baseline, var = var, prefix = "Base")
  post_ave <- replicate_average(postdose, var = var, prefix = "")
  
  base_post_ave <- left_join(post_ave %>% arrange(CLIENTID, PERIOD),
                             base_ave %>% arrange(CLIENTID, PERIOD) %>% select(-PHOUR), 
                             by = c("CLIENTID", "PERIOD"))
  ncols <- ncol(base_post_ave)
  for (i in 1:length(var)){
    col_base<- which(names(base_post_ave) %in% paste("Base", var[i], sep = ""))
    col_pose <- which(names(base_post_ave) %in% var[i])
    base_post_ave[, ncols + i] <- base_post_ave[, col_pose] - base_post_ave[, col_base]
  }
  
  names(base_post_ave)[(ncols +1) : (ncols + length(var))] <- paste("Change", var, sep = "")
  
  return(base_post_ave)
  
}



