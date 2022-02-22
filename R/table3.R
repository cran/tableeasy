#' Table 3
#' @description Creates 'Table 3' which is about stratified analysis. The three regression methods include general linear regression, logistic regression and cox proportional hazards regression.
#' @param x A string. The independent variable to be summarized given as a string.
#' @param y A string. The dependent variable to be summarized given as a string.
#' @param y_time A string. The survival time variable to be summarized given as a string. It only works when \code{method = "cox"}.
#' @param adj A vector of strings. Moderator variables to be summarized given as a character vector.
#' @param data A data frame in which these variables exist.
#' @param split_var A vector of strings. Strata variables to be summarized given as a character vector.
#' @param split_div A list containing numeric vectors, default \code{= list()}.
#' @param outformat \code{1} or \code{2} or \code{3} or \code{4}, default \code{= 4}. Output format.
#' @param method (\code{"general"}, \code{"logistic"}, \code{"cox"}), default \code{= "general"}.
#'
#' @return An object about stratified analysis.
#' @export
#'
#' @examples ## Load Mayo Clinic Primary Biliary Cirrhosis Data
#' library(survival)
#' library(tableeasy)
#' data(pbc)
#' ## Check variables
#' head(pbc)
#' ##The censored data is not discussed here
#' pbc_full <- subset(pbc,status!=0)
#' pbc_full$status <- pbc_full$status-1
#' ## Make categorical variables factors
#' varsToFactor <- c('status','trt','ascites','hepato','spiders','edema','stage','sex')
#' pbc_full[varsToFactor] <- lapply(pbc_full[varsToFactor], factor)
#'
#' ## Moderator variables
#' adj_pbc <- c('age','alk.phos','ast')
#' ## Converts the continuous variables named 'albumin' to a categorical variable named 'albumin_2'.
#' albumin_2 <- div_quantile('albumin',div = c(2),pbc_full)
#' pbc_full <- data.frame(pbc_full,'albumin_2' = albumin_2)
#'
#' ## General linear regression:
#' table3(x = 'albumin_2', y = 'bili',
#'        adj = adj_pbc, data = pbc_full,
#'        split_var = c('age','alk.phos','ast','trt'), split_div = list(),
#'        outformat = 1)
#'
#' ## Logistic regression:
#' table3(x = 'albumin_2', y = 'status',
#'        adj = adj_pbc, data = pbc_full,
#'        split_var = c('age','alk.phos','ast','trt'), split_div = list(c(45)),
#'        outformat = 2,method = 'logistic')
#'
#' ## Cox proportional hazards regression:
#' table3(x = 'albumin_2',y = 'status',y_time = 'time',
#'        adj = adj_pbc,data = pbc_full,
#'        split_var = c('age','alk.phos','ast','trt'), split_div = list(c(45),c(1500,1700),c(),c()),
#'        outformat = 3,method = 'cox')

table3<-function(x,y,y_time,adj,data,split_var,split_div=c(),outformat=4,method='general'){
  xx<-data[,x]
  j_len<-ifelse(length(levels(xx))==0,2,length(levels(xx)))
  table3_data<-data.frame()
  len<-length(split_var)
  split_div<-c(split_div,rep(list(c()),len-length(split_div)))
  if(outformat==1||outformat==2){
    totalcount<-T
  }
  else if(outformat==3||outformat==4){
    totalcount<-F
  }
  if(method=='general'){
    for(i in 1:len){
      out<-general_table3(x,y,split_var[i],adj,data,div=split_div[[i]],totalcount=totalcount)
      table3_data<-rbind(table3_data,out)#Integrate the output
    }
  }
  if(method=='logistic'){
    for(i in 1:len){
      out<-logistic_table3(x,y,split_var[i],adj,data,div=split_div[[i]],totalcount=totalcount)
      table3_data<-rbind(table3_data,out)#Integrate the output
    }
  }
  else if(method=='cox'){
    for(i in 1:len){
      out<-cox_table3(x,y_time,y,split_var[i],adj,data,div=split_div[[i]],totalcount=totalcount)
      table3_data<-rbind(table3_data,out)#Integrate the output
    }
  }
  #Output format
  if(method=='general'){
    if(outformat==1){
      #Integrate mean with sd
      number<-table3_data$N
      meansd<-paste0(table3_data$Mean,'(',table3_data$sd,')')
      na<-meansd[1]
      for(i in 1:length(meansd)){
        if (all.equal(meansd[i],na)==TRUE){meansd[i]=NA}
      }
      tabletext<-cbind(c("Subgroup",rownames(table3_data)),
                       c("N",number),
                       c("Mean(sd)",meansd)
      )
    }
    else if(outformat==2){
      #Integrate median with IQR
      number<-table3_data$N
      medianIQR<-paste0(table3_data$Median,'(',table3_data$Q1,',',table3_data$Q3,')')
      na<-medianIQR[1]
      for(i in 1:length(medianIQR)){
        if (all.equal(medianIQR[i],na)==TRUE){medianIQR[i]=NA}
      }
      tabletext<-cbind(c("Subgroup",rownames(table3_data)),
                       c("N",number),
                       c("Median(IQR)",medianIQR)
      )
    }
    else if(outformat==3){
      tabletext<-c("Subgroup",rownames(table3_data))
      for(j in 1:j_len)
      {#Integrate mean with sd one by one
        meansd1<-paste0(table3_data[,paste0('Mean_',j)],'(',table3_data[,paste0('sd_',j)],')')
        na<-meansd1[1]
        for(i in 1:length(meansd1)){
          if (all.equal(meansd1[i],na)==TRUE){meansd1[i]=NA}
        }
        tabletext<-cbind(tabletext,
                         c(paste0("N",'(',levels(data[,x])[j],')'),table3_data[,paste0('N_',j)]),
                         c(paste0("Mean(sd)",'(',levels(data[,x])[j],')'),meansd1))
      }
    }
    else if(outformat==4){
      tabletext<-c("Subgroup",rownames(table3_data))
      for(j in 1:j_len)
      {#Integrate median and IQR one by one
        medianIQR1<-paste0(table3_data[,paste0('Median_',j)],'(',table3_data[,paste0('Q1_',j)],',',table3_data[,paste0('Q3_',j)],')')
        na<-medianIQR1[1]
        for(i in 1:length(medianIQR1)){
          if (all.equal(medianIQR1[i],na)==TRUE){medianIQR1[i]=NA}
        }
        tabletext<-cbind(tabletext,
                         c(paste0("N",'(',levels(data[,x])[j],')'),table3_data[,paste0('N_',j)]),
                         c(paste0("Median(IQR)",'(',levels(data[,x])[j],')'),medianIQR1))
      }
    }
  }
  else if(method=='logistic'||method=='cox'){
    if(outformat==1){
      #Integrate count with percent
      number<-table3_data$N
      casepercent<-paste0(table3_data$Case,'(',table3_data$Percent,')')
      na<-casepercent[1]
      for(i in 1:length(casepercent)){
        if (all.equal(casepercent[i],na)==TRUE){casepercent[i]=NA}
        }
      tabletext<-cbind(c("Subgroup",rownames(table3_data)),
                       c("N",number),
                       c("Events(%)",casepercent))
      }
    else if(outformat==2){
      #Integrate case with control
      casecontrol<-paste0(table3_data$Case,'/',table3_data$Control)
      na<-casecontrol[1]
      for(i in 1:length(casecontrol)){
        if (all.equal(casecontrol[i],na)==TRUE){casecontrol[i]=NA}
        }
      tabletext<-cbind(c("Subgroup",rownames(table3_data)),
                       c("Cases/Controls",casecontrol))
      }
    else if(outformat==3){
      tabletext<-c("Subgroup",rownames(table3_data))
      #Integrate case with control one by one
      for(j in 1:j_len){
        number1<-table3_data[,paste0('N_',j)]
        # na<-number1[1]
        # for(i in 1:length(number1)){
        #   if (all.equal(number1[i],na)==TRUE){number1[i]=NA}
        #   }
        casepercent1<-paste0(table3_data[,paste0('Case_',j)],'(',table3_data[,paste0('Percent_',j)],')')
        na<-casepercent1[1]
        for(i in 1:length(casepercent1)){
          if (all.equal(casepercent1[i],na)==TRUE){casepercent1[i]=NA}
          }
        tabletext<-cbind(tabletext,
                         c(paste0('N','(',levels(data[,x])[j],')'),number1),
                         c(paste0('Event(%)','(',levels(data[,x])[j],')'),casepercent1))
        }
    }
    else if(outformat==4){
      tabletext<-c("Subgroup",rownames(table3_data))
      #Integrate case with control one by one
      for(j in 1:j_len){
        casecontrol1<-paste0(table3_data[,paste0('Case_',j)],'/',table3_data[,paste0('Control_',j)])
        na<-casecontrol1[1]
        for(i in 1:length(casecontrol1)){
          if (all.equal(casecontrol1[i],na)==TRUE){casecontrol1[i]=NA}
        }
        tabletext<-cbind(tabletext,
                         c(paste0("Cases/Controls",'(',levels(data[,x])[j],')'),casecontrol1))
      }
    }
  }
  #Integrate OR with 95%CI
  if(method=='logistic'){
    for(j in 2:j_len){
      or_ci<-paste0(table3_data[,paste0('OR_estimate_',j)],'(',
                    table3_data[,paste0('OR_lowerCI_',j)],',',
                    table3_data[,paste0('OR_upperCI_',j)],')')
      na<-or_ci[1]
      for(i in 1:length(or_ci)){
        if (all.equal(or_ci[i],na)==TRUE){or_ci[i]=NA}
      }
      tabletext<-cbind(tabletext,
                       c(paste0("OR(95%CI)_",j,'(',levels(data[,x])[j],')'),or_ci))
    }
  }else if(method=='cox'){
    for(j in 2:j_len){
      or_ci<-paste0(table3_data[,paste0('HR_estimate_',j)],'(',
                    table3_data[,paste0('HR_lowerCI_',j)],',',
                    table3_data[,paste0('HR_upperCI_',j)],')')
      na<-or_ci[1]
      for(i in 1:length(or_ci)){
        if (all.equal(or_ci[i],na)==TRUE){or_ci[i]=NA}
      }
      tabletext<-cbind(tabletext,
                       c(paste0("HR(95%CI)_",j,'(',levels(data[,x])[j],')'),or_ci))
    }
  }else if(method=='general'){
    for(j in 2:j_len){
      or_ci<-paste0(table3_data[,paste0('beta_estimate_',j)],'(',
                    table3_data[,paste0('beta_lowerCI_',j)],',',
                    table3_data[,paste0('beta_upperCI_',j)],')')
      na<-or_ci[1]
      for(i in 1:length(or_ci)){
        if (all.equal(or_ci[i],na)==TRUE){or_ci[i]=NA}
      }
      tabletext<-cbind(tabletext,
                       c(paste0("beta(95%CI)_",j,'(',levels(data[,x])[j],')'),or_ci))
    }
  }
  tabletext<-cbind(tabletext,
                   c("P for interaction",table3_data$p))
  tabletext<-data.frame(tabletext)
  return(list(tabletext=tabletext,table3_data=table3_data))
}
