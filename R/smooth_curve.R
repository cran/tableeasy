#' Smooth Curve
#' @description Draw smooth curves. The four regression methods include general linear regression, logistic regression, conditional logistic regression and cox proportional hazards regression.
#' @param x A string. The independent variable to be summarized given as a string.
#' @param y A string. The dependent variable to be summarized given as a string.
#' @param data A data frame in which these variables exist.
#' @param y_time A string. The survival time variable to be summarized given as a string.
#' @param strata A string. The paired variable to be summarized given as a string.
#' @param adj A vector of strings, default = \code{c()}. Moderator variables to be summarized given as a character vector.
#' @param fx Bool, default \code{= FALSE}.
#' @param k A vector of integers, default \code{= c()}. Degree of freedom, It only works when \code{fx = TRUE}.
#' @param split_var A string, default \code{= NULL}. The split variables to be summarized given as a string.
#' @param div A numeric vector, default \code{= c()}. It only works when split_var is a continuous variable.
#'
#' @return An object about smooth curve.
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
#' ## Smooth curve of General linear regression:
#' gam <- smooth_curve(x='albumin',
#'                     y='bili',
#'                     adj=adj_pbc,
#'                     data=pbc_full)
#' plot(gam$gam,se=TRUE,rug=TRUE,shift=gam$shift)
#'
#'
#' ## Smooth curve of logistic regression:
#' gam <- smooth_curve(x = 'albumin',
#'                     y = 'status',
#'                     adj = adj_pbc,
#'                     split_var ='age',
#'                     div = c(45),
#'                     data = pbc_full)
#' plot(gam$gam[[1]],se=FALSE,rug=TRUE,xlim=c(2,4.5),ylab = 'Adjusted ln ORs for death')
#' oldpar <- par(new=TRUE)
#' plot(gam$gam[[2]],se=FALSE,rug=TRUE,xlim=c(2,4.5),ylab = 'Adjusted ln ORs for death',lty=2)
#' on.exit(par(oldpar))
#'
#' ## Smooth curve of conditional logistic regression:
#' pbc_full <- data.frame(pbc_full,'ytime'=1)
#' gam <- smooth_curve(x ='albumin',
#'                     y_time = 'ytime',
#'                     y = 'status',
#'                     adj = adj_pbc,
#'                     strata = 'trt',
#'                     data = pbc_full)
#'
#' termplot(gam,term =c(1),col.term ="black",col.se = "black",se=TRUE,rug=FALSE,
#'            ylab="Log ORs for death")
#'
#' ## Smooth curve of Cox proportional hazards regression:
#' gam <- smooth_curve(x ='albumin',
#'                     y_time = 'time',
#'                     y = 'status',
#'                     adj = adj_pbc,
#'                     data = pbc_full)
#' termplot(gam,term =c(1),col.term ="black",col.se = "black",se=TRUE,rug=FALSE)

smooth_curve<-function(x,y,data,y_time=NULL,strata=NULL,adj=c(),fx=FALSE,k=c(),
                       split_var=NULL,div=c()){
  if(is.null(strata)){
    if(is.null(k)){
      if(is.null(split_var)){
        yy<-data[,y]
        if(is.factor(yy)){
          if(is.null(y_time)){
            adjj<-setdiff(adj, c(x, y))
            if(length(adjj)==0){
              formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ')'))
            } else {
              formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ')', ' + ',
                                                   paste0(adjj, collapse = ' + ')))
            }
            gam<-mgcv::gam(formula_logit, family = stats::binomial(link='logit'), data = data)
          }
          else{
            adjj<-setdiff(adj, c(x,y_time,y))
            if(length(adjj)==0){
              formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ', 'pspline(',x,')'))
            }else{
              formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ','pspline(', x, ')',' + ', paste(adjj, collapse = ' + ')))
            }
            gam<-survival::coxph(formula_cox, data = data)
          }
        }
        else{
          adjj<-setdiff(adj, c(x, y))
          if(length(adjj)==0){
            formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ')'))
            formula_logitcu<-stats::as.formula(paste0(y, ' ~ s(', x, ')'))
          } else {
            formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ')', ' + ',
                                                 paste0(adjj, collapse = ' + ')))
            formula_logitcu<-stats::as.formula(paste0(y, ' ~ s(', x, ')'))
          }
          gam<-mgcv::gam(formula_logit, data = data)
          gamcu<-mgcv::gam(formula_logitcu, data = data)
          shift<-summary(gamcu)[["p.coeff"]][1]
        }
        ifelse(is.factor(yy)==TRUE,return(gam),return(list('gam'=gam,'shift'=shift)))
      }
      else{
        gam<-list()
        gamcu<-list()
        ldata<-list()
        shift<-list()
        yy<-data[,y]
        adjj<-setdiff(adj, c(x, y,split_var))
        if(length(div)==0){
          sdata<-split(data,data[,split_var])
        }
        else{
          split_var_factor<-div_custom(split_var,div,data)
          divdata<-data
          divdata[,split_var]<-factor(split_var_factor)
          sdata<-split(divdata,divdata[,split_var])
        }
        for (i in 1:length(sdata)){
          ldata[[i]]<-sdata[[i]]
          if(is.factor(yy)){
            if(is.null(y_time)){
              adjj<-setdiff(adj, c(x, y,split_var))
              if(length(adjj)==0){
                formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ')'))
              } else {
                formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ')', ' + ',
                                                     paste0(adjj, collapse = ' + ')))
              }
              gam[[i]]<-mgcv::gam(formula_logit, family = stats::binomial(link='logit'), data =ldata[[i]])
            }
            else{
              adjj<-setdiff(adj, c(x,y_time,y,split_var))
              if(length(adjj)==0){
                formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ', 'pspline(',x,')'))
              }else{
                formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ','pspline(', x, ')',' + ', paste(adjj, collapse = ' + ')))
              }
              gam[[i]]<-survival::coxph(formula_cox, data =ldata[[i]])
            }
          }
          else{
            adjj<-setdiff(adj, c(x, y,split_var))
            if(length(adjj)==0){
              formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ')'))
              formula_logitcu<-stats::as.formula(paste0(y, ' ~ s(', x, ')'))
            } else {
              formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ')', ' + ',
                                                   paste0(adjj, collapse = ' + ')))
              formula_logitcu<-stats::as.formula(paste0(y, ' ~ s(', x, ')'))
            }
            gam[[i]]<-mgcv::gam(formula_logit, data = ldata[[i]])
            gamcu[[i]]<-mgcv::gam(formula_logitcu, data = ldata[[i]])
            shift[[i]]<-summary(gamcu[[i]])[["p.coeff"]][1]
          }
        }
      }
      ifelse(is.factor(yy)==TRUE,return(list('gam'=gam,'ldata'=ldata)),return(list('gam'=gam,'shift'=shift)))
    }
    else{
      if(is.null(split_var)){
        yy = data[,y]
        for(i in 1:length(k)){
          if(is.factor(yy)){
            if(is.null(y_time)){
              adjj<-setdiff(adj, c(x, y))
              if(length(adjj)==0){
                formula_logit<-stats::as.formula(paste0( y, ' ~ s(', x, ',fx = ', fx,  ',k =', k[i], ')'))
              } else {
                formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ',k =', k[i], ')', ' + ',
                                                     paste0(adjj, collapse = ' + ')))
              }
              gam<-mgcv::gam(formula_logit, family = stats::binomial(link='logit'), data = data)
            }
            else{
              adjj<-setdiff(adj, c(x,y_time,y))
              if(length(adjj)==0){
                formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ', 'pspline(', x,',df =', k[i], ')'))
              }else{
                formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ','pspline(', x,',df =', k[i], ')', '+', paste(adjj, collapse = ' + ')))
              }
              gam<-survival::coxph(formula_cox, data = data)
            }
          }
          else{
            adjj<-setdiff(adj, c(x, y))
            if(length(adjj)==0){
              formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ', k =', k[i],')'))
              formula_logitcu<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ', k =', k[i],')'))
            } else {
              formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ', k =', k[i], ')', '+ ',
                                                   paste0(adjj, collapse = ' + ')))
              formula_logitcu<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ', k =', k[i],')'))
            }
            gam<-mgcv::gam(formula_logit, data = data)
            gamcu<-mgcv::gam(formula_logitcu, data = data)
            shift<-summary(gamcu)[["p.coeff"]][1]
          }
        }
        ifelse(is.factor(yy)==TRUE,return(gam),return(list('gam'=gam,'shift'=shift)))
      }
      else{
        gam<-list()
        gamcu<-list()
        ldata<-list()
        shift<-list()
        yy<-data[,y]
        adjj<-setdiff(adj, c(x, y,split_var))
        if(length(div)==0){
          sdata<-split(data,data[,split_var])
        }
        else{
          split_var_factor<-div_custom(split_var,div,data)
          divdata<-data
          divdata[,split_var]<-factor(split_var_factor)
          sdata<-split(divdata,divdata[,split_var])
        }
        for (i in 1:length(sdata)){
          ldata[[i]]<-sdata[[i]]
          if(is.factor(yy)){
            if(is.null(y_time)){
              adjj<-setdiff(adj, c(x, y,split_var))
              if(length(adjj)==0){
                formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ', k =', k[i],')'))
              } else {
                formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ', k =', k[i],')', '+',
                                                     paste0(adjj, collapse = ' + ')))
              }
              gam[[i]]<-mgcv::gam(formula_logit, family = stats::binomial(link='logit'), data =ldata[[i]])
            }
            else{
              adjj<-setdiff(adj, c(x,y_time,y,split_var))
              if(length(adjj)==0){
                formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ', 'pspline(',x,',df =', k[i], ')'))
              }else{
                formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ','pspline(', x, ',df =', k[i], ')', '+', paste(adjj, collapse = ' + ')))
              }
              gam[[i]]<-survival::coxph(formula_cox, data =ldata[[i]])
            }
          }
          else{
            adjj<-setdiff(adj, c(x, y,split_var))
            if(length(adjj)==0){
              formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ', k =', k[i], ')'))
              formula_logitcu<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ', k =', k[i], ')'))
            } else {
              formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ', k =', k[i], ')', '+',
                                                   paste0(adjj, collapse = ' + ')))
              formula_logitcu<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ', k =', k[i], ')'))
            }
            gam[[i]]<-mgcv::gam(formula_logit, data = ldata[[i]])
            gamcu[[i]]<-mgcv::gam(formula_logitcu, data = ldata[[i]])
            shift[[i]]<-summary(gamcu[[i]])[["p.coeff"]][1]
          }
        }
      }
      ifelse(is.factor(yy)==TRUE,return(list('gam'=gam,'ldata'=ldata)),return(list('gam'=gam,'shift'=shift)))
    }
  }
  else{
    if(is.null(k)){
      yy<-data[,y]
      if(is.null(y_time)){
        adjj<-setdiff(adj, c(x, y))
        if(length(adjj)==0){
          formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ')','+','strata(',strata,')'))
        } else {
          formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ')', '+','strata(',strata,')','+',
                                               paste0(adjj, collapse = ' + ')))
        }
        gam<-mgcv::gam(formula_logit, family = stats::binomial(link='logit'), data = data)
      }
      else{
        adjj<-setdiff(adj, c(x,y_time,y))
        if(length(adjj)==0){
          formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ', 'pspline(',x,')','+','strata(',strata,')'))
        }else{
          formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ','pspline(', x, ')','+','strata(',strata,')','+', paste(adjj, collapse = ' + ')))
        }
        gam<-survival::coxph(formula_cox, data = data)
      }
      return(gam)
    }
    else{
      yy = data[,y]
      for(i in 1:length(k)){
        if(is.null(y_time)){
          adjj<-setdiff(adj, c(x, y))
          if(length(adjj)==0){
            formula_logit<-stats::as.formula(paste0( y, ' ~ s(', x, ',fx = ', fx,  ',k =', k[i], ')','+','strata(',strata,')'))
          } else {
            formula_logit<-stats::as.formula(paste0(y, ' ~ s(', x, ',fx = ', fx,  ',k =', k[i], ')', '+','strata(',strata,')','+',
                                                 paste0(adjj, collapse = ' + ')))
          }
          gam<-mgcv::gam(formula_logit, family = stats::binomial(link='logit'), data = data)
        }
        else{
          adjj<-setdiff(adj, c(x,y_time,y))
          if(length(adjj)==0){
            formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ', 'pspline(', x,',df =', k[i], ')','+','strata(',strata,')'))
          }else{
            formula_cox<-stats::as.formula(paste0('Surv(',y_time,' , as.numeric(',y,')) ~ ','pspline(', x,',df =', k[i], ')', '+','strata(',strata,')','+', paste(adjj, collapse = ' + ')))
          }
          gam<-survival::coxph(formula_cox, data = data)
        }
      }
      return(gam)
    }
  }
}


