
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


find_race <- function(dm, ex){
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
  vs$ptno <- as.numeric(vs$CLIENTID)

  # select the variables of interest
  row_vs <- which(toupper(vs$PERIOD) == "SCREEN" &
                    toupper(vs$VS_TEST) %in%
                    c('BMI','WEIGHT','HEIGHT','ELBOW','FRAME'))


  #
  col_vs <- names(vs) %in%  c("ptno", "VS_REU_R","VS_RES_R", "VS_TEST", "VS_DAT")

  vs1 <- vs[row_vs, col_vs] %>% arrange(VS_RES_R)
  vs1$VS_RES_R <- as.numeric(vs1$VS_RES_R)

  vs2 <- dcast(vs1, ptno + VS_DAT ~ VS_TEST, value.var = "VS_RES_R")

  return(vs2)
}


# get the summary statistics


get_summary <- function(data, categorical = F, group = "EX_TRT_C", var = "race", na.rm =T){
  
  if(categorical){
    
    result1 <- as.data.frame(ftable(data %>% select_(var, group))) %>% 
      mutate(trait = toupper(var)) %>% spread_(group, "Freq")
  }
  else{
    
    
    result <- data %>% select_(var, group) %>% 
                  group_by_(group) %>% 
                  summarise_(N = interp(~sum(val >0, na.rm = na.rm), val = as.name(var)), 
                             MEAN = interp(~mean(val, na.rm = na.rm), val = as.name(var)),
                             SD = interp(~sd(val, na.rm = na.rm), val = as.name(var)), 
                             MINIMUM = interp(~min(val, na.rm = na.rm), val= as.name(var)), 
                             MEDIAN = interp(~median(val, na.rm = na.rm), val = as.name(var)), 
                             MAXIMUM = interp(~max(val, na.rm= na.rm), val =as.name(var))) %>% 
                  mutate(trait = var)
    
    result1 <- result %>% melt(id = group) %>%
               mutate(trait = var) %>% spread_(group, "value")
  }
  
  return(result1)
}


#' Summarize the demographic data
#'
#' @title demographic summary.
#' @param dm the dm data set
#' @param ex the ex data set
#' @param vs the vs data set
#' @return a data frame
#' @export


dem_s1 <- function(dm, ex, vs, included){
  
  vsdm_1 <-find_race(dm, ex) %>% arrange(ptno)
  vsdm_2 <- weight_height_bmi(vs) %>% arrange(ptno)
  
  vsdm <- inner_join(vsdm_1, vsdm_2 , by = "ptno")  # combine race, ethnicity with BMI HEIGHT, WEIGHT
  vsdm <- inner_join(vsdm %>% arrange(CLIENTID), 
                     included %>% select(CLIENTID, SEQ), by = "CLIENTID")
  

  cat_race <- as.data.frame(ftable(vsdm %>% select(race, EX_TRT_C))) %>%
                      mutate(trait= "RACE") %>% spread(EX_TRT_C, Freq)
  cat_sex <- as.data.frame(ftable(vsdm %>% select(SEX, EX_TRT_C))) %>%
                      mutate(trait= "SEX") %>% spread(EX_TRT_C, Freq)
  cat <- bind_rows(cat_ethnic, cat_race, cat_sex) %>% mutate_if(is.factor, as.character)
  cat[is.na(cat)] <- ""
  coln <- which(names(cat) %in% c("ethnic", "race", "SEX"))
  cat <- unite(cat, "type", coln, sep = "")


       
  
}


dem_summary <- function(dm, ex, vs){
  vsdm_1 <-find_race(dm, ex) %>% arrange(ptno)
  vsdm_2 <- weight_height_bmi(vs) %>% arrange(ptno)
  
    vsdm <- inner_join(vsdm_1, vsdm_2 , by = "ptno")

    obs <- 1:nrow(vsdm)
    # for categorical
    race_sum <- get_summary(vsdm$race[obs], categorical = T, name = "Race")
    gender_sum <- get_summary(vsdm$SEX[obs], categorical = T, name = "Gender")
    cat_sum <- rbind(race_sum, gender_sum)
    # for continuous
    bmi <- get_summary(vsdm$BMI[obs], name = "BMI")
    height <- get_summary(vsdm$HEIGHT[obs], name = "Height")
    weight <- get_summary(vsdm$WEIGHT[obs], name = "Weight")
    age <- get_summary(vsdm$age[obs], name = "Age")
    continous_sum <- rbind(bmi, height, weight, age) %>%
        melt(id ="trait", value.name = "value", variable.name = "Type")%>% arrange(trait)

    # combine for output
    cat_sum <- cat_sum %>% 
              mutate(value = paste(frequency, " (", round(100*percent, 2), "%)", sep = "")) %>%
              select(-frequency, -percent) %>%
              mutate_if(is.factor, as.character)
    
    continous_sum <- continous_sum %>% mutate(value = as.character(round(value, 2))) %>%
                    mutate_if(is.factor, as.character)
     result <- bind_rows(cat_sum, continous_sum)
  

   return(result)
}



##
#' Summarize the demographic data
#'
#' @title demographics listing.
#' @param dm the dm data set
#' @param ex the ex data set
#' @param vs the vs data set
#' @return a data frame
#' @export

dem_listing <- function(dm, ex, vs){

  vsdm_1 <-find_race(dm, ex) %>% arrange(ptno)
  vsdm_2 <- weight_height_bmi(vs) %>% arrange(ptno)
  
  vsdm <- inner_join(vsdm_1, vsdm_2 , by = "ptno")
  
 
 result <- vsdm %>% 
   select(ptno, BRTHDAT, age, SEX_D, race, ethnic, HEIGHT, WEIGHT, BMI) %>%
   arrange(ptno)

  return(result)
}




