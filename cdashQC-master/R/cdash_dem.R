
#' create necessary variable for the demographic summary tables and listing.
#'
#' @title race and ethnicity indicator, age.
#' @param dm  the dataset dm read from sas
#' @param ex  the dataset ex read from sas
#' @return a data frame with additional columns listed as follows
#' \item{race}{the race of the subject}
#' \item{ethnic}{has two levels, "NOT HISPANIC OR LATINO" and "HISPANIC OR LATINO".}
#' \item{EX_TRT_C}{the treatment groups}
#' \item{ptno}{convert CLIENTID to numerical values of subject number}
#' \item{age}{Age calculated from start date of treatment}


guess_race_age <- function(dm, included){
  
  dm1 <- dm %>% mutate(AMERIND = ifelse(AMERIND == "YES", "American Indian/Alaska Native", ""), 
                       WHTE = ifelse(WHTE == "YES", "White", ""), 
                       BLCK = ifelse(BLCK == "YES", "Black or African American", ""), 
                       HAWAIIAN = ifelse(HAWAIIAN == "YES", "Native Hawaiian/Pacific Islander", ""), 
                       ASIAN = ifelse(ASIAN == "YES", "Asian", ""), 
                       OTHER = ifelse(OTHER == "YES", "Other Race", ""))
  names(dm1)[names(dm1)=="HISPANIC"] <- "ethnic"
  dm1 <- dm1 %>% unite_("race", c("AMERIND", "WHTE", "BLCK", "HAWAIIAN", "ASIAN", "OTHER"),"") 
  
  # get the treatment information
  ex1 <- included %>% select(CLIENTID, EX_STDAT, SEQ) %>% 
                      arrange(CLIENTID, EX_STDAT, SEQ) %>% 
                      group_by(CLIENTID) %>%
                      filter(row_number()==1) %>% 
                      ungroup()
            
  dm2 <- left_join(dm1 %>% arrange(CLIENTID), 
                   ex1 %>% arrange(CLIENTID), 
                   by = "CLIENTID")

  # calculate age by EX_STDAT (start date of treatement)
  span <- time_length(interval(ymd(dm2$BRTHDAT), ymd(dm2$EX_STDAT)), "year")
  dm2$age <- floor(span)


  return(dm2)
}



#' extract BMI, weight and height from vs.sas7bat.
#'
#' @title read BMI Weight and Height info from Screening stage.
#' @param  vs  the vs sas data
#' @return a data frame containing BMI, WEIGHT and HEIGHT from admission stage
#' @export

weight_height_bmi <- function(vs, period = "SCREEN"){
  
  
  vs1 <- vs %>% mutate(VS_TEST = replace(VS_TEST, trimws(toupper(VS_TEST)) == "BODY MASS INDEX", "BMI")) %>%
                filter(trimws(toupper(VS_TEST)) %in% c('BMI','WEIGHT','HEIGHT','ELBOW','FRAME'))
  
  vs2 <- vs1 %>% filter(trimws(toupper(PERIOD)) == period) %>% arrange(CLIENTID) %>%
                  select(CLIENTID, VS_DAT, VS_TEST, VS_REU_R, VS_RES_R) %>%
                  mutate(VS_RES_R = as.numeric(VS_RES_R))

  vs3 <- dcast(vs2, CLIENTID + VS_DAT ~ VS_TEST, value.var = "VS_RES_R")

  return(vs3)
}


#' @title create the demographics data.
#' @param dm the dm data set
#' @param vs the vs data set
#' @param inlcuded the included data set created by \code{create_included}
#' @return a data frame
#' @export
#' @seealso \code{\link{create_included}}

create_dem <- function(dm, vs, included){
  
  vsdm_1 <-guess_race_age(dm, included) 
  vsdm_2 <- weight_height_bmi(vs) %>% arrange(CLIENTID)
  
  vsdm <- inner_join(vsdm_1 %>% arrange(CLIENTID), 
                     vsdm_2 %>% arrange(CLIENTID),
                     by = "CLIENTID")  # combine race, ethnicity with BMI HEIGHT, WEIGHT
  
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


summary_dem <- function(dmt, group = "SEQ", na.rm = TRUE){

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




