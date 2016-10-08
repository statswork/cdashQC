
################################################################################################
#                                  concomitant medication
#
################################################################################################


#' list concomitant medication data
#' 
#' @title list concomitant.
#' @param cm  the dataset cm read from sas
#' @return the data with related columns 
#' @export
#' 

concom <- function(cm){
  
  id1 <- cm$CM_TRT == "" & toupper(cm$CM_YN) %in% c("N", "NO")
  cm$CM_TRT[id1] <- "NONE"
  # lable period
  cm$per <- NA
  cm$per[trimws(toupper(cm$PERIOD)) %in% c("SCREEN")] <- -2
  cm$per[trimws(toupper(cm$PERIOD)) %in% c("ALL")] <- -1
  cm$per[trimws(toupper(cm$PERIOD)) %in% c("UNKNOWN")] <- 0
  id_others <- !(trimws(toupper(cm$PERIOD)) %in% c("SCREEN", "ALL", "UNKNOWN"))
  cm$per[id_others] <- as.numeric(cm$PERIOD[id_others]) 
  cm <- cm %>% mutate (dosen = paste(trimws(cm$CM_DSTXT), trimws(cm$CM_UNT)), 
                       ptno = as.numeric(CLIENTID))
  
#   
#   conmed <- numdtt <- numtmt <- numtmp <- chardts <- 
#     chardtp <- sett <- setp <- doseprob <- unk <- T
#   
#   if (any(cm$CM_TRT != "NONE")) {conmed <- T}
#   if (any(!is.na(cm$CM_STDT) )) {numdtt <- T}
#   if (any(!is.na(cm$CM_STTM) )) {numtmt <- T}
#   if (any(!is.na(cm$CM_ENDT) )) {numdtp <- T}
#   if (any(!is.na(cm$CM_ENTM) )) {numtmp <- T}
#   if (any(trimws(toupper(cm$CM_STDTC)) %in% c("", "UNK", "UNKNOWN")) && numdtt) {chardts <- T}
#   if (any(trimws(toupper(cm$CM_STTMC)) %in% c("", "UNK", "UNKNOWN")) && numtmt) {chardts <- T}
#   # FUTURE: more about chardts and chardtp is needed
#   
#   if (any(!is.na(cm$CM_STTM))) {sett <- T}
#   if (any(!is.na(cm$CM_ENTM))) {setp <- T}
#   if (any(trimws(toupper(cm$CM_STDTC)) %in% c("UNK", "UNKNOWN")) | 
#       any(trimws(toupper(cm$CM_STTMC)) %in% c("UNK", "UNKNOWN")) |
#       any(trimws(toupper(cm$CM_ENDTC)) %in% c("UNK", "UNKNOWN")) |
#       any(trimws(toupper(cm$CM_ENTMC)) %in% c("UNK", "UNKNOWN")) )
#   {unk <- T}
#     
# #  indicator <-   c(conmed, numdtt, numtmt, numtmp, chardts, chardtp, sett, setp, doseprob, unk)
#   final <- cm %>% select(ptno, CM_YN, per, CM_STDT, CM_STTM, CM_TRT)
#   
#   if (conmed) { # any concomitant medication?
#     final <- cbind(final, cm %>% select (dosen, CM_RTE_D))
#   }  
#   
#   if (chardts) { # 
#     final <- cbind(final, cm %>% select(CM_STDTC, CM_STTMC))
#   }   
#   if (numdtt) {
#     final <- cbind(final, cm %>% select(CM_STDT))
#   }
#   if (numtmt) {
#     final <- cbind(final, cm %>% select(CM_STTM))
#   }
# 
#   if (chardtp) {
#     final <- cbind(final, cm %>% select(CM_ENDTC, CM_ENTMC))
#   }
#   
#   if (numdtp) {
#     final <- cbind(final, cm %>% select(CM_ENDT))
#   }
#   if (numtmp) {
#     final <- cbind(final, cm %>% select(CM_ENTM))
#   }
#   
#   if (conmed) {
#     final <- cbind(final, cm %>% select(CM_FRQ_D, CM_INDC, CM_ON, CM_PRIOR))
#   }
#     
  final <-  cm %>% select(ptno, CM_YN, per, CM_STDT, CM_STTM, CM_TRT, dosen, CM_RTE_D, 
                          CM_STDTC, CM_STTMC, CM_ENDTC, CM_ENTMC,
                          CM_ENDT, CM_ENTM, CM_FRQ_D, CM_INDC, CM_ON, CM_PRIOR )
  
  # seperate the characters and other numerics
  attrib <- sapply(final, class)
  col_character <- as.numeric(which(attrib == "character"))
  empty_character <- colSums(final[, col_character] != "")
  
  # other empty columns
  col_other <- as.numeric(which(attrib != "character"))
  empty_other <- colSums(!is.na(final[, col_other]))  # remove empty columns
  
  # the empty columns to be removed
  empty1 <- c(empty_other, empty_character) == 0
  empty_column <- names(empty1)[empty1]
  final <- final[, !(names(final) %in% empty_column)]
  
  return(final)
  
}

