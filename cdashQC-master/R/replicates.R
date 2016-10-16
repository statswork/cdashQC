
#' get the replicates for each protocol hour (PHOUR)
#' 
#' @title get the replicates for eg data.
#' @param data the data set eg.
#' @param reps specifies the structures of reps. If not specified, \code{guess_resp} will be invoked. Default set to be \code{NULL}. 
#' @return  a list 
#'  \item{data_clean}{the subjects containing correct number of resplicates}
#'  \item{data_dirty}{the subjects that have different number of replicates than desired}
#' @export
#' @seealso \code{\link{create_eg}}, \code{\link{guess_resp}} and  \code{\link{check_data}}
#' 

replicate_data <- function(data, reps= NULL){
  
  d1 <- guess_test(data)
  if (grepl("EG", d1)){ data1 <- create_eg(data) }       # if the data is EG
  else if (grepl("VS", d1)){ data1 <- create_vs(data)  } # if the data is VS
  
  t0 <- check_data(data, reps = reps, printed = FALSE)
  t1 <- t0 %>% select(CLIENTID, PERIOD, PHOUR) %>%   # those are data having reps issues
              mutate(dirty_obs = TRUE)
  data2 <- left_join(data1 %>% arrange(CLIENTID, PERIOD, PHOUR), 
                     t1 %>% arrange(CLIENTID, PERIOD, PHOUR), 
                     by = c("CLIENTID", "PERIOD", "PHOUR"))
  
  data_clean <- data2 %>% filter(is.na(dirty_obs)) %>% select(-dirty_obs)
  data_dirty <- data2 %>% filter(dirty_obs == TRUE) %>% select(-dirty_obs)
  
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
#' @examples
#' eg2 <- replicate_data(eg) # step 1: find the triplicates
#' eg_prob <- eg2$data_dirty         # need manual check
#' # the following rows should be removed
#' rows_removed <- c(2, 4, 7, 13, 14, 15, 19, 25, 28, 32, 37, 40, 44, 52, 57, 59, 64, 65)
#' eg3 <- replicate_clean(eg2, rows_removed)
#' @seealso \code{\link{replicate_data}}


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







#' create an extra column inidcating whether the current row should be considered as baseline. 
#' 
#' @title create baseline indicator.
#' @param data currently support \code{vs}, \code{eg} and \code{lb_cq}.
#' @param var_identifier a string that can be used to identify the variable name e.g.(\code{VS_TEST, EG_TEST, LB_TEST})
#' @return a data frame with an extra column \code{status} whose value could be one of \code{BASELINE}, \code{POSTDOSE} and \code{PREDOSE (NOT BASELINE)} 
#' @export
#' @seealso \code{\link{guess_base_phour}}


create_baseline <- function(data, var_identifier = "_TEST"){
  
  data <- create_phour(data)
  
  # find the baseline hours for each test category
  basehour <- guess_base_phour(data, var_identifier)
  var_sort <- names(basehour)[!names(basehour) %in% c("status")]  # sort by these variables
  
  # merge to the original data to create an extra column showing whether it's baseline or not.
  data1 <- left_join(data %>% arrange_(.dots = var_sort), 
                     basehour %>% arrange_(.dots= var_sort), 
                     by = var_sort)
  
  id1 <- is.na(data1$status) & (is.na(data1$HOUR) | data1$HOUR < 0)
  id2 <- is.na(data1$status) & ( data1$HOUR >= 0)
  id3 <- toupper(trimws(data1$PERIOD)) == "SCREEN"
  
  data1$status[id1] <- "PREDOSE (NOT BASELINE)"
  data1$status[id2] <- "POSTDOSE"
  data1$status[id3] <- "SCREEN"
  
  return(data1)
}

