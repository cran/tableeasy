#' @title Split a Variable by Quantile Statistics
#' @description Split a continuous variable by quantile statistics, Converts a continuous variable to a categorical variable.
#' @param var A string. A variable to be summarized given as a string.
#' @param div A positive integer greater than 1. The number of factor levels when the variable is split by quantile statistics.
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
#' div_quantile(var = 'age', div = 5, data = pbc)
div_quantile <- function(var,div,data){
  x<-data[,var]
  len_div<-length(div)
  n<-sum(div)
  n_1<-sum(div[1:(len_div-1)])
  prob<-stats::quantile(x, probs = 1:(n-1)/n, na.rm = T)
  y<-x
  Qorother<-ifelse(n==2,'B',ifelse(n==3,'T','Q'))
  # Quantile
  if(len_div==1){

    if(n>=2 && n%%1==0){
      y[x< prob[1]]<-paste0('    ',Qorother,'1 (<', round(prob[1], 1), ')')
      if(n>=3){
        for(i in 1:(n-2)){
          y[x>= prob[i] & x< prob[i+1]]<-paste0('    ',Qorother,(i+1),' (', round(prob[i], 1), '~', round(prob[i+1], 1), ')')
        }
      }
      y[x>= prob[n-1]]<-paste0('    ',Qorother,n,' (','\u2265', round(prob[n-1], 1), ')')
    }

  }
  # Categories
  else if(len_div>=2 && len_div%%1==0){
    y[x<prob[div[1]]]<-ifelse(div[1]==1,paste0('    ',Qorother,'1 (<', round(prob[div[1]], 1), ')'),
                              paste0('    ',Qorother,'1-',Qorother,div[1],' (<', round(prob[div[1]], 1), ')'))
    if(len_div>=3){
      for(i in 2:(len_div-1)){
        y[x>=prob[sum(div[1:(i-1)])] & x<prob[sum(div[1:i])]]<-ifelse(div[i]==1,
                                                                      paste0('    ',Qorother,sum(div[1:(i-1)])+1,' (', round(prob[sum(div[1:(i-1)])], 1), '~', round(prob[sum(div[1:i])], 1), ')'),
                                                                      paste0('    ',Qorother,sum(div[1:(i-1)])+1,'-',Qorother,sum(div[1:i]),' (', round(prob[sum(div[1:(i-1)])], 1), '~', round(prob[sum(div[1:i])], 1), ')'))
      }
    }
    y[x>=prob[n_1]]<-ifelse(div[len_div]==1,paste0('    ',Qorother,n,' (','\u2265', round(prob[n_1], 1), ')'),
                            paste0('    ',Qorother,n_1+1,'-',Qorother,n,' (','\u2265', round(prob[n_1], 1), ')'))
  }
  y<-as.factor(y)
  return(y)
}
###############################################################
div_quantile_name<-function(var, div, data){
  name<-c()
  n<-sum(div)
  len_div<-length(div)
  y<-data[,var]
  # Quantile Name
  if(len_div==1){
    if (n == 2) {name <- c('Binaries',levels(y),'P for trend')}
    if (n == 3) {name <- c('Tertiles',levels(y),'P for trend')}
    if (n == 4) {name <- c('Quartiles',levels(y),'P for trend')}
    if (n == 5) {name <- c('Quintiles',levels(y),'P for trend')}
    if(n > 5 && n %% 1==0){name <- c(paste0('Q1~Q',n),levels(y),'P for trend')}
  }else if(len_div>1 && len_div%%1==0){
    name <- c('Categories',levels(y),'P for trend')
  }
  return(name)
}
