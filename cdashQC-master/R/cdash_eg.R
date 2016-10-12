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





#' get the replicates for each protocol hour (PHOUR)
#' 
#' @title get the replicates for eg data.
#' @param eg the data set eg.
#' @param reps  how many replicates will be measured for each protocol hour. 
#' @param isBase Is this for finding baseline? Default set to be \code{TRUE}.
#' @return  a list 
#'  \item{data_clean}{the subjects containing correct number of resplicates}
#'  \item{data_dirty}{the subjects that have different number of replicates than desired}
#' @export
#' @seealso \code{\link{create_eg}}
#' 

replicate_eg <- function(eg, reps= 3){
    
    eg1 <- create_eg(eg)             # transpose the data
    eg2 <- create_baseline(eg)       # label the baselines
    
    # merge by PHOUR
    bl <- eg2 %>% filter(trimws(EG_TEST) != "OVERALL INTERPRETATION") %>%
                  select(CLIENTID, PERIOD, PHOUR, status) %>% 
                  arrange(CLIENTID, PERIOD, PHOUR) %>% 
                  distinct()
    
    # ts <- eg2 %>% filter(! status %in% c("BASELINE", "POSTDOSE")) %>% select(CLIENTID, PHOUR, DAY, HOUR, EG_TEST, status)
   
    # then merge choose_hour with eg1 data to get the baseline (or postdose) measurement
    eg3 <- left_join(eg1 %>% arrange(CLIENTID, PERIOD, PHOUR), 
                     bl,  by = c("CLIENTID", "PERIOD", "PHOUR")) %>%
                filter(status == "BASELINE" | status == "POSTDOSE")  # only interested in baseline and postdose

  # check whether each time point has correct replicate numbers
  choose_hour <- bl %>% filter(status == "BASELINE" | status == "POSTDOSE") %>% 
          select(CLIENTID, PERIOD, PHOUR)
  phour_tab <- as.data.frame(ftable(choose_hour ));
  phour_tab$Freq <- phour_tab$Freq*reps
  
  obs_hour <- eg3 %>% select(CLIENTID, PERIOD, PHOUR)
  data_tab <- as.data.frame(ftable(obs_hour))
  # these are the subjects having issue with measurements
  phour_tab <- phour_tab %>% arrange(CLIENTID, PERIOD, PHOUR)
  data_tab <- data_tab %>% arrange(CLIENTID, PERIOD, PHOUR)

  prob_index <- which(phour_tab != data_tab, arr.ind = T)
  
  id_clean <- rep(T, nrow(eg3))  # by default, choose all rows
  
  # if the index matrix is not empty, then the corresponding subject has measurement issues
  if(nrow(prob_index) > 0 ){
    subject_id <- data_tab[prob_index[, 1], 1]
    nreps <- data_tab[prob_index[, 1], 4]
    nper <- data_tab[prob_index[, 1], 2]
    nphour <- data_tab[prob_index[, 1], 3]
    message("WARNING: number of replicates for each protocol hour should be ", reps, "\n", sep = "")
    for (i in 1:length(subject_id)){
      message("subject ", trimws(subject_id[i]), " has ", nreps[i], " measurements at ", 
              nphour[i], " PERIOD ", nper[i], sep = "")
      dirty_obs <- which(eg3$CLIENTID == subject_id[i] & eg3$PERIOD == nper[i] &
                           eg3$PHOUR == nphour[i])
      id_clean[dirty_obs] <- F  # these rows have replicate issues
    }
  }
  
  data_clean <- eg3[id_clean, ]
  data_dirty <- eg3[!id_clean, ]
  rownames(data_dirty) <- 1:nrow(data_dirty)
  
  result <- list(data_clean = data_clean, data_dirty=data_dirty)
  return(result)
  
}


## after checking the data_dirty data, need to decide which rows to be used as replicates
#' get cleaned replicates 
#' 
#' @title get the cleaned replicates by combining the "clean" data and "dirty" data
#' @param data  an object returned from \code{replicate_eg}
#' @param rm_row a vector of integers specifying which rows should be removed from the dirty data.
#' @return the cleaned replicates
#' @export
#' @seealso \code{\link{replicate_eg}}


replicate_clean <- function(data, rm_row = NULL){
  
  data_clean <- data$data_clean
  data_dirty <- data$data_dirty
  
  if(nrow(data_dirty) == 0){return(data_clean)}
  else {
    if(is.null(rm_row)){
      stop("there are issues with replicates. You must specify which rows of data_dirty to be removed")
    }
    data_keep <- data_dirty[-rm_row, ]
    
    data_clean_new <- bind_rows(data_clean, data_keep) %>%
      arrange(CLIENTID, PERIOD, HOUR)
  }
  
}


## By now you should have collected all the replicates for each PHOUR  correctly
#' Get the averages of the replicates
#' 
#' @title get the averanges of the replicates
#' @param data  an object returned from \code{replicate_clean}
#' @param var the variables used to do the calculation
#' @return the averages
#' @export
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
#' @param data. An object returned from \code{replicate_clean}
#' @param var the variables used to do the calculation
#' @return a data frame
#' @export
#' @seealso \code{\link{replicate_clean}}  
#' @seealso \code{\link{replicate_eg}}
#' @seealso \code{\link{create_eg}}
#' 

change_from_base <- function(data, var = c("HR", "PR", "QRS", "QT", "QTCF")){
  
  baseline <- data %>% filter(status == "BASELINE")
  postdose <- data %>% filter(status == "POSTDOSE")
  
  base_ave <- replicate_average(baseline, var = var)
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



