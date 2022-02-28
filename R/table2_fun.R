###############################################################
general<-function(x, y,adj, data,outformat=2){
  xx<-data[,x]
  yy<-data[,y]
  #Formula of model
  formula_crude<-stats::as.formula(paste0(y, ' ~ ', x))
  if(length(adj)==0){
    formula_adj<-stats::as.formula(paste0(y, ' ~ ', x))
  }else{
    formula_adj<-stats::as.formula(paste0(y, ' ~ ', x , ' + ', paste(adj, collapse = ' + ')))
    }
  #General regression
  model_crude<-stats::glm(formula_crude, data = data, family = stats::gaussian())
  model_adj<-stats::glm(formula_adj, data = data, family = stats::gaussian())
  #Compute OR and p value
  if(is.factor(xx)){
    summary_y<-NULL
    #Compute p-trend
    formula_crude_trend<-stats::as.formula(paste0(y, ' ~ as.numeric(', x, ')'))
    model_crude_trend<-stats::glm(formula_crude_trend, data = data, family = stats::gaussian())

    if(length(adj)==0){
      formula_adj_trend<-stats::as.formula(paste0(y, ' ~ as.numeric(', x, ')'))
    }else{
      formula_adj_trend<-stats::as.formula(paste0(y, ' ~ as.numeric(', x, ') +', paste(adj, collapse = ' + ')))
      }
    model_adj_trend<-stats::glm(formula_adj_trend, data = data, family = stats::gaussian())

    trend_p_crude<-summary(model_crude_trend)$coefficients[2, 4]
    trend_p_adj<-summary(model_adj_trend )$coefficients[2, 4]
    #Output p-trend
    p_for_trend<-c('', '', '',
                     ifelse(trend_p_crude < 0.001, '<0.001', sprintf("%.3f", trend_p_crude)),
                     '',
                     ifelse(trend_p_adj   < 0.001, '<0.001', sprintf("%.3f", trend_p_adj)))
    #Compute OR
    level<-length(levels(xx))
    beita_estimate_crude<-summary(model_crude)$coefficients[2:level, 1]
    beita_lowerCI_crude<-summary(model_crude)$coefficients[2:level, 1] + stats::qnorm(0.025)*summary(model_crude)$coefficients[2:level, 2]
    beita_upperCI_crude<-summary(model_crude)$coefficients[2:level, 1] + stats::qnorm(0.975)*summary(model_crude)$coefficients[2:level, 2]

    beita_estimate_adj<-summary(model_adj)$coefficients[2:level, 1]
    beita_lowerCI_adj<-summary(model_adj)$coefficients[2:level, 1] + stats::qnorm(0.025)*summary(model_adj)$coefficients[2:level, 2]
    beita_upperCI_adj<-summary(model_adj)$coefficients[2:level, 1] + stats::qnorm(0.975)*summary(model_adj)$coefficients[2:level, 2]
    #Output OR, p value
    beita_crude<-c('ref',
                     paste0(sprintf("%.2f", beita_estimate_crude), '(',
                             sprintf("%.2f", beita_lowerCI_crude),  ',',
                             sprintf("%.2f", beita_upperCI_crude),  ')'))

    beita_adj<-c('ref',
                   paste0(sprintf("%.2f", beita_estimate_adj), '(',
                           sprintf("%.2f", beita_lowerCI_adj),  ',',
                           sprintf("%.2f", beita_upperCI_adj),  ')'))

    p_crude<-c('',
                 ifelse(summary(model_crude)$coefficients[2:level, 4] < 0.001, '<0.001',
                         sprintf('%.3f', summary(model_crude)$coefficients[2:level, 4])))
    p_adj<-c('',
               ifelse(summary(model_adj)$coefficients[2:level, 4] < 0.001, '<0.001',
                       sprintf('%.3f', summary(model_adj)$coefficients[2:level, 4])))
    N<-table(xx)
    for(i in 1:level){

      dd<-data[ data[ ,x] == levels(xx)[i], ]
      yyy<-stats::na.omit(dd[,y])
      if(outformat==1){
        summary_y[i]<-paste0(round(mean(yyy),2), '(',
                             round(stats::sd(yyy),2), ')')
      }else{
        summary_y[i]<-paste0(round(stats::quantile(yyy,1:3/4)[2],1), '(',
                             round(stats::quantile(yyy,1:3/4)[1],1),'-',round(stats::quantile(yyy,1:3/4)[3],1),')')
      }
    }
    var_information<-data.frame(matrix(c(N, summary_y, beita_crude, p_crude, beita_adj, p_adj),ncol=6))
    var_information<-rbind(var_information, data.frame(matrix(p_for_trend,nrow=1)))
  }
  else{
    #Compute OR
    beita_estimate_crude<-summary(model_crude)$coefficients[2, 1]
    beita_lowerCI_crude<-summary(model_crude)$coefficients[2, 1] + stats::qnorm(0.025)*summary(model_crude)$coefficients[2, 2]
    beita_upperCI_crude<-summary(model_crude)$coefficients[2, 1] + stats::qnorm(0.975)*summary(model_crude)$coefficients[2, 2]

    beita_estimate_adj<-summary(model_adj)$coefficients[2, 1]
    beita_lowerCI_adj<-summary(model_adj)$coefficients[2, 1] + stats::qnorm(0.025)*summary(model_adj)$coefficients[2, 2]
    beita_upperCI_adj<-summary(model_adj)$coefficients[2, 1] + stats::qnorm(0.975)*summary(model_adj)$coefficients[2, 2]
    #Output OR, p value
    beita_crude<-paste0(sprintf("%.2f", beita_estimate_crude), '(',
                           sprintf("%.2f", beita_lowerCI_crude),  ',',
                           sprintf("%.2f", beita_upperCI_crude),  ')')

    beita_adj<-paste0(sprintf("%.2f", beita_estimate_adj), '(',
                         sprintf("%.2f", beita_lowerCI_adj),  ',',
                         sprintf("%.2f", beita_upperCI_adj),  ')')

    p_crude<-ifelse(summary(model_crude)$coefficients[2, 4] < 0.001, '<0.001',
                       sprintf('%.3f', summary(model_crude)$coefficients[2, 4]))
    p_adj<-ifelse(summary(model_adj)$coefficients[2, 4] < 0.001, '<0.001',
                       sprintf('%.3f', summary(model_adj)$coefficients[2, 4]))

    N<-length(xx)
    if(outformat==1){
      summary_y <- paste0( round(mean(yy),2), '(',
                           round(stats::sd(yy),2), ')' )
    }else{
      summary_y<-paste0(round(stats::quantile(yy,1:3/4)[2],1), '(',
                        round(stats::quantile(yy,1:3/4)[1],1),'-',round(stats::quantile(yy,1:3/4)[3],1),')')
    }
    var_information<-data.frame(matrix(c(N, summary_y, beita_crude, p_crude, beita_adj, p_adj), nrow = 1))
  }
  return(var_information)
}
###############################################################
logistic<-function(x, y, adj=NULL, data,case=2,condition='quantile'){
  xx<-data[,x]
  yy<-data[,y]
  case1<-levels(yy)[case]
  #Formula of model
  formula_crude<-stats::as.formula(paste0(y, ' ~ ', x))
  if(length(adj)==0){
    formula_adj<-stats::as.formula(paste0(y, ' ~ ', x))
  }else{
    formula_adj<-stats::as.formula(paste0(y, ' ~ ', x , ' + ', paste(adj, collapse = ' + ')))
    }
  #Logistic regression
  model_crude<-stats::glm(formula_crude, data = data, family = stats::binomial(link = "logit"))
  model_adj<-stats::glm(formula_adj,   data = data, family = stats::binomial(link = "logit"))
  #Compute OR, p value
  if(is.factor(xx)){
    #Compute p-trend
    formula_crude_trend<-stats::as.formula(paste0(y, ' ~ as.numeric(', x, ')'))
    model_crude_trend<-stats::glm(formula_crude_trend, data = data, family = stats::binomial(link = "logit"))

    if(length(adj)==0){
      formula_adj_trend<-stats::as.formula(paste0(y, ' ~ as.numeric(', x, ')'))
    }else{
      formula_adj_trend<-stats::as.formula(paste0(y, ' ~ as.numeric(', x, ') +', paste(adj, collapse = ' + ')))
      }
    model_adj_trend<-stats::glm(formula_adj_trend, data = data, family = stats::binomial(link = "logit"))

    trend_p_crude<-summary(model_crude_trend)$coefficients[2, 4]
    trend_p_adj<-summary(model_adj_trend  )$coefficients[2, 4]
    #Compute p-trend
    p_for_trend<-c(rep('',3),
                   ifelse(trend_p_crude < 0.001, '<0.001', sprintf("%.3f", trend_p_crude)),
                   rep('',1),
                   ifelse(trend_p_adj   < 0.001, '<0.001', sprintf("%.3f", trend_p_adj)))
    #Compute OR
    level<-length(levels(xx))
    OR_estimate_crude<-exp(summary(model_crude)$coefficients[2:level, 1])
    OR_lowerCI_crude<-exp(summary(model_crude)$coefficients[2:level, 1] + stats::qnorm(0.025)*summary(model_crude)$coefficients[2:level, 2])
    OR_upperCI_crude<-exp(summary(model_crude)$coefficients[2:level, 1] + stats::qnorm(0.975)*summary(model_crude)$coefficients[2:level, 2])

    OR_estimate_adj<-exp(summary(model_adj)$coefficients[2:level, 1])
    OR_lowerCI_adj<-exp(summary(model_adj)$coefficients[2:level, 1] + stats::qnorm(0.025)*summary(model_adj)$coefficients[2:level, 2])
    OR_upperCI_adj<-exp(summary(model_adj)$coefficients[2:level, 1] + stats::qnorm(0.975)*summary(model_adj)$coefficients[2:level, 2])
    #Output OR, p value
    OR_crude<-c('ref',
                  paste0(sprintf("%.2f", OR_estimate_crude), '(',
                          sprintf("%.2f", OR_lowerCI_crude),  ',',
                          sprintf("%.2f", OR_upperCI_crude),  ')'))

    OR_adj<-c('ref',
                paste0(sprintf("%.2f", OR_estimate_adj), '(',
                        sprintf("%.2f", OR_lowerCI_adj),  ',',
                        sprintf("%.2f", OR_upperCI_adj),  ')'))

    p_crude<-c(NA,
                 ifelse(summary(model_crude)$coefficients[2:level, 4] < 0.001, '<0.001',
                         sprintf('%.3f', summary(model_crude)$coefficients[2:level, 4])))
    p_adj<-c(NA,
               ifelse(summary(model_adj)$coefficients[2:level, 4] < 0.001, '<0.001',
                       sprintf('%.3f', summary(model_adj)$coefficients[2:level, 4])))

    intertable<-table(data.frame(xx, yy))
    #Broadcasting
    N<-intertable[ ,1] + intertable[ ,2]
    Case<-intertable[ ,case]
    Percent<-paste0(Case, '(', sprintf('%.1f', 100*Case/N), ')')
    if(condition=='quantile'){
      var_information<-data.frame(matrix(c(N, Percent, OR_crude, p_crude, OR_adj, p_adj),ncol=6))
      var_information<-rbind(var_information, data.frame(matrix(p_for_trend,nrow=1)))
    }else if(condition=='categories'){
      var_information<-data.frame(matrix(c(N, Percent, OR_crude, p_crude, OR_adj, p_adj),ncol=6))
    }

  } else{
    #Compute OR
    OR_estimate_crude<-exp(summary(model_crude)$coefficients[2, 1])
    OR_lowerCI_crude<-exp(summary(model_crude)$coefficients[2, 1] + stats::qnorm(0.025)*summary(model_crude)$coefficients[2, 2])
    OR_upperCI_crude<-exp(summary(model_crude)$coefficients[2, 1] + stats::qnorm(0.975)*summary(model_crude)$coefficients[2, 2])

    OR_estimate_adj<-exp(summary(model_adj)$coefficients[2, 1])
    OR_lowerCI_adj<-exp(summary(model_adj)$coefficients[2, 1] + stats::qnorm(0.025)*summary(model_adj)$coefficients[2, 2])
    OR_upperCI_adj<-exp(summary(model_adj)$coefficients[2, 1] + stats::qnorm(0.975)*summary(model_adj)$coefficients[2, 2])
    #Compute OR, p value
    OR_crude<-paste0(sprintf("%.2f", OR_estimate_crude), '(',
                        sprintf("%.2f", OR_lowerCI_crude),  ',',
                        sprintf("%.2f", OR_upperCI_crude),  ')')

    OR_adj<-paste0(sprintf("%.2f", OR_estimate_adj), '(',
                      sprintf("%.2f", OR_lowerCI_adj),  ',',
                      sprintf("%.2f", OR_upperCI_adj),  ')')

    p_crude<-ifelse(summary(model_crude)$coefficients[2, 4] < 0.001, '<0.001',
                       sprintf('%.3f', summary(model_crude)$coefficients[2, 4]))
    p_adj<-ifelse(summary(model_adj)$coefficients[2, 4] < 0.001, '<0.001',
                     sprintf('%.3f', summary(model_adj)$coefficients[2, 4]))

    N<-length(xx)
    Case<-length(which(yy == case1))#case
    Percent<-paste0(Case, '(', sprintf('%.1f', 100*Case/N), ')')

    var_information<-data.frame(matrix(c(N, Percent, OR_crude, p_crude, OR_adj, p_adj),ncol=6))

  }
  return(var_information)
}

###############################################################
con_logistic<-function(x, y,strata,adj=NULL, data,case=2){
  xx<-data[,x]
  yy<-data[,y]
  case1<-levels(yy)[case]
  s<-data[,strata]#diff
  #Formula of model
  formula_crude<-stats::as.formula(paste0('as.numeric(',y, ') ',' ~ ', x,' + ','strata(',strata,')'))
  if(length(adj)==0){
    formula_adj<-stats::as.formula(paste0('as.numeric(',y, ') ',' ~ ', x,' + ','strata(',strata,')'))
  }else{
    formula_adj<-stats::as.formula(paste0('as.numeric(',y,') ', ' ~ ', x , ' + strata(',strata,') +', paste(adj, collapse = ' + ')))
    }
  #Con_logistic regression
  model_crude<-survival::clogit(formula_crude, data = data)
  model_adj<-survival::clogit(formula_adj,   data = data)
  #Compute OR、p value
  if(is.factor(xx)){
    #Compute p-trend
    formula_crude_trend<-stats::as.formula(paste0('as.numeric(', y, ') ', ' ~ as.numeric(', x, ') + strata(',strata,')'))
    model_crude_trend<-survival::clogit(formula_crude_trend, data = data)#diff

    if(length(adj)==0){
      formula_adj_trend<-stats::as.formula(paste0('as.numeric(', y, ') ', ' ~ as.numeric(', x, ') + strata(',strata,')'))
    }else{
      formula_adj_trend<-stats::as.formula(paste0('as.numeric(', y,') ', ' ~ as.numeric(', x, ') + strata(',strata,') + ', paste(adj, collapse = ' + ')))
      }
    model_adj_trend<-survival::clogit(formula_adj_trend, data = data) #diff

    trend_p_crude<-summary(model_crude_trend)$coefficients[1,5]#diff
    trend_p_adj<-summary(model_adj_trend  )$coefficients[1,5]
    #Compute p-trend
    p_for_trend<-c(rep('',3),
                   ifelse(trend_p_crude < 0.001, '<0.001', sprintf("%.3f", trend_p_crude)),
                   rep('',1),
                   ifelse(trend_p_adj   < 0.001, '<0.001', sprintf("%.3f", trend_p_adj)))
    #Compute OR
    level<-length(levels(xx)) #diff
    OR_estimate_crude<-summary(model_crude)$conf.int[1:level-1, 1]
    OR_lowerCI_crude<-summary(model_crude)$conf.int[1:level-1, 3]
    OR_upperCI_crude<-summary(model_crude)$conf.int[1:level-1, 4]

    OR_estimate_adj<-summary(model_adj)$conf.int[1:level-1, 1]
    OR_lowerCI_adj<-summary(model_adj)$conf.int[1:level-1, 3]
    OR_upperCI_adj<-summary(model_adj)$conf.int[1:level-1, 4]
    #Compute OR, p value
    OR_crude<-c('ref',
                  paste0(sprintf("%.2f", OR_estimate_crude), '(',
                          sprintf("%.2f", OR_lowerCI_crude),  ',',
                          sprintf("%.2f", OR_upperCI_crude),  ')'))

    OR_adj<-c('ref',
                paste0(sprintf("%.2f", OR_estimate_adj), '(',
                        sprintf("%.2f", OR_lowerCI_adj),  ',',
                        sprintf("%.2f", OR_upperCI_adj),  ')'))

    p_crude<-c('',
                 ifelse(summary(model_crude)$coefficients[1:level-1, 5] < 0.001, '<0.001',
                         sprintf('%.3f', summary(model_crude)$coefficients[1:level-1, 5])))
    p_adj<-c('',
               ifelse(summary(model_adj)$coefficients[1:level-1, 5] < 0.001, '<0.001',
                       sprintf('%.3f', summary(model_adj)$coefficients[1:level-1, 5])))

    intertable<-table(data.frame(xx, yy))
    #Broadcasting
    N<-intertable[ ,1] + intertable[ ,2]
    Case<-intertable[ ,case]
    Control<-N-Case
    Percent<-paste0(Case, '/', Control)
    var_information<-data.frame(matrix(c(N, Percent, OR_crude, p_crude, OR_adj, p_adj),ncol=6))
    var_information<-rbind(var_information, data.frame(matrix(p_for_trend,nrow=1)))
  }
  else{
    #Compute OR
    OR_estimate_crude<-summary(model_crude)$conf.int[1,1]
    OR_lowerCI_crude<-summary(model_crude)$conf.int[1,3]
    OR_upperCI_crude<-summary(model_crude)$conf.int[1,4]

    OR_estimate_adj<-summary(model_adj)$conf.int[1,1]
    OR_lowerCI_adj<-summary(model_adj)$conf.int[1,3]
    OR_upperCI_adj<-summary(model_adj)$conf.int[1,4]
    #Output OR, p value
    OR_crude<-paste0(sprintf("%.2f", OR_estimate_crude), '(',
                        sprintf("%.2f", OR_lowerCI_crude),  ',',
                        sprintf("%.2f", OR_upperCI_crude),  ')')

    OR_adj<-paste0(sprintf("%.2f", OR_estimate_adj), '(',
                      sprintf("%.2f", OR_lowerCI_adj),  ',',
                      sprintf("%.2f", OR_upperCI_adj),  ')')

    p_crude<-ifelse(summary(model_crude)$coefficients[1,5] < 0.001, '<0.001',
                       sprintf('%.3f', summary(model_crude)$coefficients[1,5]))
    p_adj<-ifelse(summary(model_adj)$coefficients[1,5] < 0.001, '<0.001',
                     sprintf('%.3f', summary(model_adj)$coefficients[1,5]))

    N<-length(xx)
    Case<-length(which(yy == case1))#case
    Control<-N-Case
    Percent<-paste0(Case, '/', Control)

    var_information<-data.frame(matrix(c(N, Percent, OR_crude, p_crude, OR_adj, p_adj),ncol=6))

  }
  return(var_information)
}

###############################################################
cox<-function(x, y_time, y_factor,adj=NULL, data,case=2){
  xx<-data[,x]
  yy_time<-data[,y_time]
  yy_factor<-data[,y_factor]
  case1<-levels(yy_factor)[case]
  #Formula of model
  formula_crude<-stats::as.formula(paste0('survival::Surv(',y_time,' , as.numeric(',y_factor,')) ~ ', x))
  if(length(adj)==0){
    formula_adj<-stats::as.formula(paste0('survival::Surv(',y_time,' , as.numeric(',y_factor,')) ~ ', x))
  }else{
    formula_adj<-stats::as.formula(paste0('survival::Surv(',y_time,' , as.numeric(',y_factor,')) ~ ', x, ' + ', paste(adj, collapse = ' + ')))
    }
  #Cox regression
  model_crude<-survival::coxph(formula_crude, data = data)
  model_adj<-survival::coxph(formula_adj, data = data)
  #Compute OR, p value
  if(is.factor(xx)){
    #Compute p-trend
    formula_crude_trend<-stats::as.formula(paste0('survival::Surv(',y_time,' , as.numeric(',y_factor,')) ~ ', 'as.numeric(', x, ')'))
    model_crude_trend<-survival::coxph(formula_crude_trend, data = data)#diff

    if(length(adj)==0){
      formula_adj_trend<-stats::as.formula(paste0('survival::Surv(',y_time,' , as.numeric(',y_factor,')) ~ ', 'as.numeric(', x, ')'))
    }else{
      formula_adj_trend<-stats::as.formula(paste0('survival::Surv(',y_time,' , as.numeric(',y_factor,')) ~ ', 'as.numeric(', x, ') + ', paste(adj, collapse = ' + ')))
      }
    model_adj_trend<-survival::coxph(formula_adj_trend, data = data) #diff

    trend_p_crude<-summary(model_crude_trend)$coefficients[1,5]#diff
    trend_p_adj<-summary(model_adj_trend  )$coefficients[1,5]
    #Compute p-trend
    p_for_trend<-c(rep('',3),
                   ifelse(trend_p_crude < 0.001, '<0.001', sprintf("%.3f", trend_p_crude)),
                   rep('',1),
                   ifelse(trend_p_adj   < 0.001, '<0.001', sprintf("%.3f", trend_p_adj)))
    #compute OR
    level<-length(levels(xx)) #diff
    OR_estimate_crude<-summary(model_crude)$conf.int[1:level-1, 1]
    OR_lowerCI_crude<-summary(model_crude)$conf.int[1:level-1, 3]
    OR_upperCI_crude<-summary(model_crude)$conf.int[1:level-1, 4]

    OR_estimate_adj<-summary(model_adj)$conf.int[1:level-1, 1]
    OR_lowerCI_adj<-summary(model_adj)$conf.int[1:level-1, 3]
    OR_upperCI_adj<-summary(model_adj)$conf.int[1:level-1, 4]
    #Output OR, p value
    OR_crude<-c('ref',
                  paste0(sprintf("%.2f", OR_estimate_crude), '(',
                          sprintf("%.2f", OR_lowerCI_crude),  ',',
                          sprintf("%.2f", OR_upperCI_crude),  ')'))

    OR_adj<-c('ref',
                paste0(sprintf("%.2f", OR_estimate_adj), '(',
                        sprintf("%.2f", OR_lowerCI_adj),  ',',
                        sprintf("%.2f", OR_upperCI_adj),  ')'))

    p_crude<-c('',
                 ifelse(summary(model_crude)$coefficients[1:level-1, 5] < 0.001, '<0.001',
                         sprintf('%.3f', summary(model_crude)$coefficients[1:level-1, 5])))
    p_adj<-c('',
               ifelse(summary(model_adj)$coefficients[1:level-1, 5] < 0.001, '<0.001',
                       sprintf('%.3f', summary(model_adj)$coefficients[1:level-1, 5])))

    intertable<-table(data.frame(xx, yy_factor))
    #Broadcasting
    N<-intertable[ ,1] + intertable[ ,2]
    Case<-intertable[ ,case]
    Percent<-paste0(Case, '(', sprintf('%.1f', 100*Case/N), ')')
    var_information<-data.frame(matrix(c(N, Percent, OR_crude, p_crude, OR_adj, p_adj),ncol=6))
    var_information<-rbind(var_information, data.frame(matrix(p_for_trend,nrow=1)))
  }
  else{
    #Compute OR
    OR_estimate_crude<-summary(model_crude)$conf.int[1,1]
    OR_lowerCI_crude<-summary(model_crude)$conf.int[1,3]
    OR_upperCI_crude<-summary(model_crude)$conf.int[1,4]

    OR_estimate_adj<-summary(model_adj)$conf.int[1,1]
    OR_lowerCI_adj<-summary(model_adj)$conf.int[1,3]
    OR_upperCI_adj<-summary(model_adj)$conf.int[1,4]
    #Compute OR, p value
    OR_crude<-paste0(sprintf("%.2f", OR_estimate_crude), '(',
                        sprintf("%.2f", OR_lowerCI_crude),  ',',
                        sprintf("%.2f", OR_upperCI_crude),  ')')

    OR_adj<-paste0(sprintf("%.2f", OR_estimate_adj), '(',
                      sprintf("%.2f", OR_lowerCI_adj),  ',',
                      sprintf("%.2f", OR_upperCI_adj),  ')')

    p_crude<-ifelse(summary(model_crude)$coefficients[1,5] < 0.001, '<0.001',
                       sprintf('%.3f', summary(model_crude)$coefficients[1,5]))
    p_adj<-ifelse(summary(model_adj)$coefficients[1,5] < 0.001, '<0.001',
                     sprintf('%.3f', summary(model_adj)$coefficients[1,5]))

    N<-length(xx)
    Case<-length(which(yy_factor == case1))#case
    Percent<-paste0(Case, '(', sprintf('%.1f', 100*Case/N), ')')

    var_information<-data.frame(matrix(c(N, Percent, OR_crude, p_crude, OR_adj, p_adj),ncol=6))

  }
  return(var_information)
}
