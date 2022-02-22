#' @title Ensure Factor Variable in Data Set
#' @description Ensure that factor variables in data set are of the correct type.
#' @param data A data frame.
#' @param execute Bool, default \code{= FALSE}. If TRUE, then output data frame.
#' @param threshold_factor An integer, default \code{= 5}. Criteria to judge as factor type. When a variable has more than \code{threshold_factor} unique values, it is considered continuous.
#'
#' @return A list containing data frame and description.
#' @export
#'
#' @examples ## Load Mayo Clinic Primary Biliary Cirrhosis Data
#' library(survival)
#' library(tableeasy)
#' data(pbc)
#' ## Check variables
#' head(pbc)
#' ##
#' ensure_factor(pbc)
#'
#' ensure_factor(pbc,execute=TRUE)[['message']]
#' pbc_exe <- ensure_factor(pbc,execute=TRUE)[['data']]

ensure_factor<-function(data,execute=FALSE,threshold_factor=5){
  message<-c('maybe factor:','(levels<=5)')
  for(i in colnames(data)){
    len<-length(unique(data[,i]))
    if(len<=threshold_factor){
      if(execute==TRUE){data[,i]<-factor(data[,i])}
      message<-c(message,i,len)
    }
  }
  message<-matrix(message,ncol=2,byrow = TRUE)
  ifelse(execute==TRUE,return(list('message'=message,'data'=data)),return(message))
}
