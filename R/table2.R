#' Table 2
#' @description ' Table 2 ' was created through regression analysis to research influence factor. The four regression methods include general linear regression, logistic regression, conditional logistic regression and cox proportional hazards regression.
#' @param x A string. The independent variable to be summarized given as a string.
#' @param y A string. The dependent variable to be summarized given as a string.
#' @param y_time A string. The survival time variable to be summarized given as a string. It only works when \code{method = "cox"}.
#' @param strata A string. The paired variable to be summarized given as a string. It only works when \code{method = "con_logistic"}.
#' @param adj A vector of strings, moderator variables to be summarized given as a character vector.
#' @param data A data frame in which these variables exist.
#' @param div A list containing Positive int greater than 1 or integer vector, If it is a positive integer greater than 1, it is the number of factor levels when x is split by quantile statistics. If it is a vector of integers, it is the strategy of grouping x by quantile statistics and then merging groups.
#' @param div_num A list containing numeric vectors, Elements in the list are custom values, and x can be split into at least two levels by elements in the list.
#' @param ref A vector of integers. The control level of factor levels when x is split by quantile statistics.
#' @param ref_num A vector of integers. The control level of factor levels when x is split by custom values.
#' @param continuous Bool, default \code{= FALSE}.
#' @param case A vector of integers, default \code{= 2}. The case level of y.
#' @param method (\code{"general"}, \code{"logistic"}, \code{"con_logistic"}, \code{"cox"}), default \code{= "general"}.
#' @param outformat \code{1} or \code{2}, default \code{= 2}, Output format. It only works when \code{method = "general"}. The table ouput mean(sd) when \code{outformat=1} and ouput median(IQR) when \code{outformat=2}.

#' @return An object researching influence factor.
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
#'
#' ## General linear regression:
#' table2(x = 'albumin', y = 'bili',
#'        adj = adj_pbc, data = pbc_full,
#'        div = list(5,c(2,3)), div_num = list(c(3.2,4)),
#'        ref = c(2,1), ref_num = c(2),
#'        outformat = 2)
#'
#' ## Logistic regression:
#' table2(x ='albumin', y = 'status',
#'        adj = adj_pbc, data = pbc_full,
#'        div = list(5,c(2,3)),
#'        method ='logistic')
#'
#' ## Conditional logistic regression:
#' table2(x = 'albumin', y = 'status', strata = 'trt',
#'        adj = adj_pbc, data = pbc_full,
#'        div = list(5,c(2,3)),
#'        method = 'con_logistic')
#'
#' ## Cox proportional hazards regression:
#' table2(x = 'albumin', y = 'status', y_time = 'time',
#'        adj = adj_pbc, data = pbc_full,
#'        div = list(5,c(2,3)),
#'        method = 'cox')

table2<-function(x,y,y_time,strata,adj,data,div=list(),div_num=list(),ref=c(),ref_num=c(),continuous=FALSE,case=2,method='general',outformat=2){
  ### Padding of ref and ref_num
  ref1_padding<-ifelse(length(div)-length(ref)==0,1,length(div)-length(ref))
  ref2_padding<-ifelse(length(div_num)-length(ref_num)==0,1,length(div_num)-length(ref_num))
  ref1<-c(ref,rep(1,ref1_padding))#padding
  ref2<-c(ref_num,rep(1,ref2_padding))#padding
  if(is.factor(data[,x])){
    ###x is factor###
    data[,x]<-stats::relevel(data[,x], ref=levels(data[,x])[ref1[1]])
    if(method=='general'){divdata<-general(x=x, y=y, adj=adj, data=data)
    }else if(method=='logistic'){divdata<-logistic(x=x, y=y, adj=adj, data=data,case=case)
    }else if(method=='con_logistic'){divdata<-con_logistic(x=x, y=y, strata=strata, adj=adj, data=data,case=case)
    }else if(method=='cox'){divdata<-cox(x=x, y_time=y_time, y_factor=y, adj=adj, data=data,case=case)}
    divdata<-rbind('',divdata)#Add a blank line
    #Row names
    rownames(divdata)<-c(x,levels(data[,x]),'p for trend')
    output<-divdata
  }
  ###x is continuous###
  else {
    if(method=='general'){con<-general(x=x, y=y, adj=adj, data=data)
    }else if(method=='logistic'){con<-logistic(x, y, adj=adj, data,case=case)
    }else if(method=='con_logistic'){con<-con_logistic(x=x, y=y, strata=strata,adj=adj, data=data,case=case)
    }else if(method=='cox'){con<-cox(x=x, y_time=y_time, y_factor=y,adj=adj, data=data,case=case)}
    output<-con

    ### If length of div and length of div_num both equal 0,then we think x is factor
    if(length(div)==0 && length(div_num)==0){
      data[,x]<-stats::relevel(data[,x], ref=levels(data[,x])[ref1[1]])
      if(method=='general'){divdata<-general(x=x, y=y, adj=adj, data=data)
      }else if(method=='logistic'){divdata<-logistic(x=x, y=y, adj=adj, data=data,case=case)
      }else if(method=='con_logistic'){divdata<-con_logistic(x=x, y=y, strata=strata, adj=adj, data=data,case=case)
      }else if(method=='cox'){divdata<-cox(x=x, y_time=y_time, y_factor=y, adj=adj, data=data,case=case)}
      divdata<-rbind('',divdata)#Add a blank line
      #Row names
      rownames(divdata)<-c(x,levels(data[,x]),'p for trend')
      output<-divdata
    }
    ###x is continuous###
    else{
      ###Split x by quantile statistics###
      if(length(div)!=0){
        for (i in 1:length(div)){
          n1<-div[[i]]
          newcol<-div_quantile(x,n1,data)
          newdata<-data.frame(data,newcol=newcol)
          newdata$newcol<-factor(newdata$newcol)
          newdata$newcol<-stats::relevel(newdata$newcol, ref=levels(newdata$newcol)[ref1[i]])
          if(method=='general'){divdata<-general(x='newcol', y=y, adj=adj, data=newdata)
          }else if(method=='logistic'){divdata<-logistic(x='newcol', y, adj=adj, newdata,case=case)
          }else if(method=='con_logistic'){divdata<-con_logistic(x='newcol', y=y,strata=strata, adj=adj, data=newdata,case=case)
          }else if(method=='cox'){divdata<-cox(x='newcol', y_time=y_time, y_factor=y, adj=adj, data=newdata,case=case)}
          #Row names
          divdata<-rbind('',divdata)#Add a blank line
          rownames(divdata)<-div_quantile_name('newcol',n1,newdata)
          output<-rbind(output,divdata)
        }
      }
      ###split x by custom values###
      if(length(div_num)!=0){
        for(i in 1:length(div_num)){
          n2<-div_num[[i]]
          newcol<-div_custom(x,n2,data)
          newdata<-data.frame(data,newcol=newcol)
          newdata$newcol<-factor(newdata$newcol)
          newdata$newcol<-stats::relevel(newdata$newcol, ref=levels(newdata$newcol)[ref2[i]])
          if(method=='general'){divdata<-general(x='newcol', y, adj=adj, newdata)
          }else if(method=='logistic'){divdata<-logistic(x='newcol', y, adj=adj, newdata,case=case)
          }else if(method=='con_logistic'){divdata<-con_logistic(x='newcol', y, strata=strata,adj=adj, newdata,case=case)
          }else if(method=='cox'){divdata<-cox(x='newcol', y_time=y_time, y_factor=y, adj=adj, newdata,case=case)}
          #Row names
          divdata<-rbind('',divdata)#Add a blank line
          rownames(divdata)<-c(x,div_custom_name('newcol',n2,newdata))
          output<-rbind(output,divdata)#Add the continuous line
        }
      }
      #Row names
      rownames(output)[1]<-c('Continuous')
      #If print the continuous line
      if(continuous==FALSE){output<-output[-1,]}
    }
  }
  #Column names
  if(method=='general'){colnames(output)<-c('N',ifelse(outformat==1,'mean(sd)','median(IQR)'),'beta(95%CI)_crude','P_crude','beta(95%CI)_adj','P_adj')
  }else if(method=='logistic'){colnames(output)<-c('N','Event(%)','OR(95%CI)_crude','P_crude','OR(95%CI)_adj','P_adj')
  }else if(method=='con_logistic'){colnames(output)<-c('N','Case/Control','OR(95%CI)_crude','P_crude','OR(95%CI)_adj','P_adj')
  }else if(method=='cox'){colnames(output)<-c('N','Event(%)','HR(95%CI)_crude','P_crude','HR(95%CI)_adj','P_adj')}
  return(output)
}


