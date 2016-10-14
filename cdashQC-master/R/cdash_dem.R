
# #' create necessary variable for the demographic summary tables and listing.
# #' 
# #' @title race and ethnicity indicator, age.
# #' @param dm  the dataset dm read from sas
# #' @param ex  the dataset ex read from sas
# #' @return a data frame with additional columns listed as follows
# #' \item{race}{the race of the subject}
# #' \item{ethnic}{has two levels, "NOT HISPANIC OR LATINO" and "HISPANIC OR LATINO".}
# #' \item{EX_TRT_C}{the treatment groups}
# #' \item{ptno}{convert CLIENTID to numerical values of subject number}
# #' \item{age}{Age calculated from start date of treatment}


guess_race_age <- function(dm, ex){
  sort_col <- sort(names(dm))
  dm1 <- data.frame(dm[, sort_col])

  # combine the race categories, if some one has multiple race attribute
  race_category <- sort(c("WHTE", "BLCK", "AMERIND", "HAWAIIAN", "ASIAN", "OTHER"))
  race_name <- sort(c('White','Black or African American','American Indian/Alaska Native',
                 'Native Hawaiian/Pacific Islander','Asian','Other Race'))
  col_id <- which(names(dm1) %in% race_category)
  race_matrix <- dm1[, col_id]=="YES"
  race_sum <- apply(race_matrix, 1, sum)
  race <- c()
  for ( i in 1:nrow(race_matrix)){
      if (race_sum[i] == 1) {  # if this subject belongs to a single race
        race[i] <-  race_name[which(race_matrix[i, ])] }
      else if (race_sum[i] > 1) { # if the subject belongs to multi-race
        race[i] <- paste(race_name[which(race_matrix[i, ])], sep= ",", collapse = ", ")
      }
      else race[i] <- ""
    }
  dm1$race <- race

  dm1$ptno <- as.numeric(dm1$CLIENTID)

  # # create ethnicity variable if not already exist
  if (!( "ETHNIC" %in% (names(dm1) ))) {  # not exist
      id1 <- dm1$HISPANIC == "" # obs have empty value of HISPANIC
      dm1$ethnic[!id1] <- ifelse(trimws(dm1$HISPANIC[!id1]) %in%
                                c("NOT HISPANIC OR LATINO", "NO"),
                    "NOT HISPANIC OR LATINO", "HISPANIC OR LATINO")
      dm1$ethnic[id1] <- ""
  }

  # get the treatment information
  ex1 <- ex %>% select(CLIENTID, EX_TRT_C, EX_STDAT) %>% 
            arrange(CLIENTID, EX_TRT_C, EX_STDAT) %>% 
            group_by(CLIENTID, EX_TRT_C) %>%
            filter(row_number()==1)
  
  dm1 <- left_join(dm1, ex1, by = "CLIENTID")

  # calculate age by EX_STDAT (start date of treatement)
  span <- time_length(interval(ymd(dm1$BRTHDAT), ymd(dm1$EX_STDAT)), "year")
  dm1$age <- floor(span)


  return(dm1)
}



# #' extract BMI, weight and height from vs.sas7bat.
# #'
# #' @title read BMI Weight and Height info from Screening stage.
# #' @param  vs  the vs sas data
# #' @return a data frame containing BMI, WEIGHT and HEIGHT from admission stage
# #' @export

weight_height_bmi <- function(vs){

  vs$VS_TEST[trimws(toupper(vs$VS_TEST)) == "BODY MASS INDEX"] <- "BMI"
  
  # select the variables of interest
  row_vs <- which(toupper(vs$PERIOD) == "SCREEN" &
                    toupper(vs$VS_TEST) %in%
                    c('BMI','WEIGHT','HEIGHT','ELBOW','FRAME'))


  #
  col_vs <- names(vs) %in%  c("CLIENTID", "VS_REU_R","VS_RES_R", "VS_TEST", "VS_DAT")

  vs1 <- vs[row_vs, col_vs] %>% arrange(VS_RES_R)
  vs1$VS_RES_R <- as.numeric(vs1$VS_RES_R)

  vs2 <- dcast(vs1, CLIENTID + VS_DAT ~ VS_TEST, value.var = "VS_RES_R")

  return(vs2)
}


#' @title create the demographics data.
#' @param dm the dm data set
#' @param ex the ex data set
#' @param vs the vs data set
#' @param inlcuded the included data set created by \code{create_included}
#' @return a data frame
#' @export
#' @seealso \code{\link{create_included}}

create_dem <- function(dm, ex, vs, included){
  
  vsdm_1 <-guess_race_age(dm, ex) %>% arrange(CLIENTID)
  vsdm_2 <- weight_height_bmi(vs) %>% arrange(CLIENTID)
  
  vsdm <- inner_join(vsdm_1, vsdm_2 , by = "CLIENTID")  # combine race, ethnicity with BMI HEIGHT, WEIGHT
  vsdm <- inner_join(vsdm %>% arrange(CLIENTID),    # get the SEQ info
                     included %>% select(CLIENTID, SEQ), by = "CLIENTID")
  return(vsdm)
  
}


#' Summarize the demographic data
#'
#' @title demographic summary.
#' @param dmt the data set created by \code{create_dem}.
#' @param group which variable name would be used to calculate summary statistics? 
#' @param na.rm  should missing values be included?  \code{TRUE} by default.
#' @return a data frame
#' @export
#' @examples 
#' included <- create_included(ex, dm, cr, ds)
#' dmt <- create_dem(dm, ex, vs, included)
#' summary_dem(dmt, group = "SEQ")   # the summary by group
#' summary_dem(dmt, group = "SPONSOR")  # to get the overall summary
#' @seealso \code{\link{create_included}} and \code{\link{create_dem}}


summary_dem <- function(dmt, group = "EX_TRT_C", na.rm = TRUE){

   vsdm <- dmt
    # for categorical
    race_sum <- get_summary_stats(vsdm, group = group, var = "race", na.rm = na.rm)
    sex_sum <- get_summary_stats(vsdm, group = group, var = "SEX", na.rm = na.rm)
    ethnic_sum <- get_summary_stats(vsdm, group = group, var = "ethnic", na.rm = na.rm)
    cat_sum <- dplyr::bind_rows(race_sum, sex_sum, ethnic_sum)
    
  
    # turn the counts to percentages
    cat_result <- count_percent(cat_sum, var1 = 1, var2 = 3:ncol(cat_sum), digit_keep =3)

    # for continuous
    bmi <- get_summary_stats(vsdm, group = group, var = "BMI", na.rm = na.rm)
    height <- get_summary_stats(vsdm, group = group, var = "HEIGHT", na.rm = na.rm)
    weight <- get_summary_stats(vsdm, group = group, var = "WEIGHT", na.rm = na.rm)
    age <- get_summary_stats(vsdm, group = group, var = "age", na.rm = na.rm)
    continous_sum <- dplyr::bind_rows(bmi, height, weight, age)

    # customize the output
    
     result <- list(categorical = cat_result, continous = continous_sum)

   return(result)
}



##
#' Summarize the demographic data
#'
#' @title demographics listing.
#' @param dmt the data set created by \code{create_dem}.
#' @return a data frame
#' @export
#' @seealso \code{\link{create_included}} and \code{\link{create_dem}}
#' 
#' 
listing_dem <- function(dmt){

 result <- dmt %>% 
   select(CLIENTID, BRTHDAT, age, SEX_D, race, ethnic, HEIGHT, WEIGHT, BMI) %>%
   arrange(CLIENTID)

  return(result)
}




