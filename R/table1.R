#' @title Table 1
#' @description Create an object summarizing all baseline variables ( both continuous and categorical ). The function is improved on the basis of tableone::CreateTableOne to become more convenient to use.
#' @param var A vector of strings. Variables to be summarized given as a character vector.
#' @param strata A vector of strings. Stratifying (grouping) variable name(s) given as a character vector.
#' @param data A data frame in which these variables exist.
#' @param normal A vector of strings, default \code{= c("age", "bmi", "sbp", "dbp")}. Show the mean and standard deviation of some variables.
#' @param catDigits An integer, Number of decimal places in the table of continuous variables.
#' @param contDigits An integer, Number of decimal places in the table of categorical variables.
#' @param pDigits An integer, Number of decimal places in the table of p values.
#' @param showAllLevels Bool, default \code{= FALSE}. If TRUE, the table contains all levels of categorical variables.
#'
#' @return An object describing baseline characteristics.
#' @export
#' @examples ## Load Mayo Clinic Primary Biliary Cirrhosis Data
#' library(survival)
#' library(tableeasy)
#' data(pbc)
#' ## Check variables
#' head(pbc)
#' ## Make categorical variables factors
#' varsToFactor <- c('status','trt','ascites','hepato','spiders','edema','stage','sex')
#' pbc[varsToFactor] <- lapply(pbc[varsToFactor], factor)
#' ##Table 1
#' table1(var=c('age','albumin','alk.phos','ast','edema','ascites','bili','chol'),strata='trt',pbc)

table1<-function(var,strata,data,normal=c('age','bmi','sbp','dbp'),catDigits=1,contDigits=1,pDigits=3,showAllLevels=FALSE){
  factorvar<-c()
  no_normal<-c()
  for(i in 1:length(var)){
    if(length(unique(data[,var[i]]))<=5){
      factorVars<-c(factorvar,var[i])
    }
    else if(length(unique(data[,var[i]]))>5 &&
            nortest::ad.test(data[,var[i]])$p.value<=0.05 && #ad normality test
            stats::shapiro.test(data[,var[i]][0:5000])$p.value<=0.05){#shapiro normality test
      no_normal<-c(no_normal,var[i])
    }
  }
  tableone<-tableone::CreateTableOne(vars = var,strata = strata,data = data,#table1
                                     factorVars = factorvar,addOverall = T)
  no_normal<-no_normal[!no_normal %in% normal]#default age and bmi are normal
  return(print(tableone,nonnormal=no_normal,catDigits = catDigits,contDigits = contDigits,pDigits = pDigits,showAllLevels = showAllLevels))
}
