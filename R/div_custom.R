#' Split a Variable by Custom Values
#' @description Split a continuous variable by custom values. Converts a continuous variable to a categorical variable.
#' @param var A string. A variable to be summarized given as a string.
#' @param div A numeric vector. The variable can be split into at least two levels by custom values.
#' @param data A data frame in which these variables exist.
#'
#' @return A factor variable.
#' @export
#'
#' @examples ## Load Mayo Clinic Primary Biliary Cirrhosis Data
#' library(survival)
#' library(tableeasy)
#' data(pbc)
#' ## Check variables
#' head(pbc)
#' ##
#' div_custom(var = 'age',div = c(40,60),data = pbc)
div_custom<-function(var,div,data){
  len_div<-length(div)
  x<-data[,var]
  y<-x
  if(len_div>=1 && len_div%%1==0){
    y[x< div[1]]<-paste0('    1 (<', round(div[1], 1), ')')
    if(len_div>=2){
      for(i in 1:(len_div-1)){
        y[x>= div[i] & x< div[i+1]]<-paste0('    ',(i+1),' (', round(div[i], 1), '~', round(div[i+1], 1), ')')
      }
    }
    y[x>= div[len_div]]<-paste0('    ',len_div+1,' (','\u2265', round(div[len_div], 1), ')')
  }
  y<-as.factor(y)
  return(y)
}
###############################################################
div_custom_name<-function(var, div, data){
  name<-c()
  name<-c(levels(data[,var]),'P for trend')
  return(name)
}
