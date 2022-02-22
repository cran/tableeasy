###############################################################
general<-function(x, y,adj, data,outformat=2){
  xx<-data[,x]
  yy<-data[,y]
  #确认调整变量不同于x,y
  adj1<-setdiff(adj, c(x, y))
  #model和调整model的表达式
  formula_crude<-stats::as.formula(paste0(y, ' ~ ', x))
  formula_adj<-stats::as.formula(paste0(y, ' ~ ', x , ' + ', paste(adj1, collapse = ' + ')))
  #general regression
  model_crude<-stats::glm(formula_crude, data = data, family = stats::gaussian())
  model_adj<-stats::glm(formula_adj, data = data, family = stats::gaussian())
  #计算OR值、p值并输出
  if(is.factor(xx)){
    summary_y<-NULL
    #计算p-trend
    formula_crude_trend<-stats::as.formula(paste0(y, ' ~ as.numeric(', x, ')'))
    model_crude_trend<-stats::glm(formula_crude_trend, data = data, family = stats::gaussian())

    formula_adj_trend<-stats::as.formula(paste0(y, ' ~ as.numeric(', x, ') +', paste(adj1, collapse = ' + ')))
    model_adj_trend<-stats::glm(formula_adj_trend, data = data, family = stats::gaussian())

    trend_p_crude<-summary(model_crude_trend)$coefficients[2, 4]
    trend_p_adj<-summary(model_adj_trend )$coefficients[2, 4]
    #输出p-trend
    p_for_trend<-c('', '', '',
                     ifelse(trend_p_crude < 0.001, '<0.001', sprintf("%.3f", trend_p_crude)),
                     '',
                     ifelse(trend_p_adj   < 0.001, '<0.001', sprintf("%.3f", trend_p_adj)))
    #计算OR值
    level<-length(levels(xx))
    beita_estimate_crude<-summary(model_crude)$coefficients[2:level, 1]
    beita_lowerCI_crude<-summary(model_crude)$coefficients[2:level, 1] + stats::qnorm(0.025)*summary(model_crude)$coefficients[2:level, 2]
    beita_upperCI_crude<-summary(model_crude)$coefficients[2:level, 1] + stats::qnorm(0.975)*summary(model_crude)$coefficients[2:level, 2]

    beita_estimate_adj<-summary(model_adj)$coefficients[2:level, 1]
    beita_lowerCI_adj<-summary(model_adj)$coefficients[2:level, 1] + stats::qnorm(0.025)*summary(model_adj)$coefficients[2:level, 2]
    beita_upperCI_adj<-summary(model_adj)$coefficients[2:level, 1] + stats::qnorm(0.975)*summary(model_adj)$coefficients[2:level, 2]
    #输出OR值和p值
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
    #计算OR值
    beita_estimate_crude<-summary(model_crude)$coefficients[2, 1]
    beita_lowerCI_crude<-summary(model_crude)$coefficients[2, 1] + stats::qnorm(0.025)*summary(model_crude)$coefficients[2, 2]
    beita_upperCI_crude<-summary(model_crude)$coefficients[2, 1] + stats::qnorm(0.975)*summary(model_crude)$coefficients[2, 2]

    beita_estimate_adj<-summary(model_adj)$coefficients[2, 1]
    beita_lowerCI_adj<-summary(model_adj)$coefficients[2, 1] + stats::qnorm(0.025)*summary(model_adj)$coefficients[2, 2]
    beita_upperCI_adj<-summary(model_adj)$coefficients[2, 1] + stats::qnorm(0.975)*summary(model_adj)$coefficients[2, 2]
    #输出OR值和p值
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
logistic<-function(x, y, adj, data,case=2,condition='quantile'){
  xx<-data[,x]
  yy<-data[,y]
  case1<-levels(yy)[case]
  #确认调整变量不同于x,y
  adj1<-setdiff(adj, c(x, y))
  #model和调整model的表达式
  formula_crude<-stats::as.formula(paste0(y, ' ~ ', x))
  formula_adj<-stats::as.formula(paste0(y, ' ~ ', x , ' + ', paste(adj1, collapse = ' + ')))
  #logistic回归
  model_crude<-stats::glm(formula_crude, data = data, family = stats::binomial(link = "logit"))
  model_adj<-stats::glm(formula_adj,   data = data, family = stats::binomial(link = "logit"))
  #计算OR值、p值并输出
  if(is.factor(xx)){
    #计算p-trend
    formula_crude_trend<-stats::as.formula(paste0(y, ' ~ as.numeric(', x, ')'))
    model_crude_trend<-stats::glm(formula_crude_trend, data = data, family = stats::binomial(link = "logit"))

    formula_adj_trend<-stats::as.formula(paste0(y, ' ~ as.numeric(', x, ') +', paste(adj1, collapse = ' + ')))
    model_adj_trend<-stats::glm(formula_adj_trend, data = data, family = stats::binomial(link = "logit"))

    trend_p_crude<-summary(model_crude_trend)$coefficients[2, 4]
    trend_p_adj<-summary(model_adj_trend  )$coefficients[2, 4]
    #输出p-trend
    p_for_trend<-c(rep('',3),
                   ifelse(trend_p_crude < 0.001, '<0.001', sprintf("%.3f", trend_p_crude)),
                   rep('',1),
                   ifelse(trend_p_adj   < 0.001, '<0.001', sprintf("%.3f", trend_p_adj)))
    #计算OR值
    level<-length(levels(xx))
    OR_estimate_crude<-exp(summary(model_crude)$coefficients[2:level, 1])
    OR_lowerCI_crude<-exp(summary(model_crude)$coefficients[2:level, 1] + stats::qnorm(0.025)*summary(model_crude)$coefficients[2:level, 2])
    OR_upperCI_crude<-exp(summary(model_crude)$coefficients[2:level, 1] + stats::qnorm(0.975)*summary(model_crude)$coefficients[2:level, 2])

    OR_estimate_adj<-exp(summary(model_adj)$coefficients[2:level, 1])
    OR_lowerCI_adj<-exp(summary(model_adj)$coefficients[2:level, 1] + stats::qnorm(0.025)*summary(model_adj)$coefficients[2:level, 2])
    OR_upperCI_adj<-exp(summary(model_adj)$coefficients[2:level, 1] + stats::qnorm(0.975)*summary(model_adj)$coefficients[2:level, 2])
    #输出OR值和p值
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
    #广播
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
    #计算OR值
    OR_estimate_crude<-exp(summary(model_crude)$coefficients[2, 1])
    OR_lowerCI_crude<-exp(summary(model_crude)$coefficients[2, 1] + stats::qnorm(0.025)*summary(model_crude)$coefficients[2, 2])
    OR_upperCI_crude<-exp(summary(model_crude)$coefficients[2, 1] + stats::qnorm(0.975)*summary(model_crude)$coefficients[2, 2])

    OR_estimate_adj<-exp(summary(model_adj)$coefficients[2, 1])
    OR_lowerCI_adj<-exp(summary(model_adj)$coefficients[2, 1] + stats::qnorm(0.025)*summary(model_adj)$coefficients[2, 2])
    OR_upperCI_adj<-exp(summary(model_adj)$coefficients[2, 1] + stats::qnorm(0.975)*summary(model_adj)$coefficients[2, 2])
    #输出OR值和p值
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
con_logistic<-function(x, y,strata,adj, data,case=2){
  xx<-data[,x]
  yy<-data[,y]
  case1<-levels(yy)[case]
  s<-data[,strata]#diff
  #确认调整变量不同于x,y
  adj1<-setdiff(adj, c(x, y,strata))#diff
  #model和调整model的表达式
  formula_crude<-stats::as.formula(paste0('as.numeric(',y, ') ',' ~ ', x,' + ','strata(',strata,')'))
  formula_adj<-stats::as.formula(paste0('as.numeric(',y,') ', ' ~ ', x , ' + strata(',strata,') +', paste(adj1, collapse = ' + ')))
  #con_logistic回归
  model_crude<-survival::clogit(formula_crude, data = data)
  model_adj<-survival::clogit(formula_adj,   data = data)
  #计算OR值、p值并输出
  if(is.factor(xx)){
    #计算p-trend
    formula_crude_trend<-stats::as.formula(paste0('as.numeric(', y, ') ', ' ~ as.numeric(', x, ') + strata(',strata,')'))
    model_crude_trend<-survival::clogit(formula_crude_trend, data = data)#diff

    formula_adj_trend<-stats::as.formula(paste0('as.numeric(', y,') ', ' ~ as.numeric(', x, ') + strata(',strata,') + ', paste(adj1, collapse = ' + ')))
    model_adj_trend<-survival::clogit(formula_adj_trend, data = data) #diff

    trend_p_crude<-summary(model_crude_trend)$coefficients[1,5]#diff
    trend_p_adj<-summary(model_adj_trend  )$coefficients[1,5]
    #输出p-trend
    p_for_trend<-c(rep('',3),
                   ifelse(trend_p_crude < 0.001, '<0.001', sprintf("%.3f", trend_p_crude)),
                   rep('',1),
                   ifelse(trend_p_adj   < 0.001, '<0.001', sprintf("%.3f", trend_p_adj)))
    #计算OR值
    level<-length(levels(xx)) #diff
    OR_estimate_crude<-summary(model_crude)$conf.int[1:level-1, 1]
    OR_lowerCI_crude<-summary(model_crude)$conf.int[1:level-1, 3]
    OR_upperCI_crude<-summary(model_crude)$conf.int[1:level-1, 4]

    OR_estimate_adj<-summary(model_adj)$conf.int[1:level-1, 1]
    OR_lowerCI_adj<-summary(model_adj)$conf.int[1:level-1, 3]
    OR_upperCI_adj<-summary(model_adj)$conf.int[1:level-1, 4]
    #输出OR值和p值
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
    #广播
    N<-intertable[ ,1] + intertable[ ,2]
    Case<-intertable[ ,case]
    Control<-N-Case
    Percent<-paste0(Case, '/', Control)
    var_information<-data.frame(matrix(c(N, Percent, OR_crude, p_crude, OR_adj, p_adj),ncol=6))
    var_information<-rbind(var_information, data.frame(matrix(p_for_trend,nrow=1)))
  }
  else{
    #计算OR值
    OR_estimate_crude<-summary(model_crude)$conf.int[1,1]
    OR_lowerCI_crude<-summary(model_crude)$conf.int[1,3]
    OR_upperCI_crude<-summary(model_crude)$conf.int[1,4]

    OR_estimate_adj<-summary(model_adj)$conf.int[1,1]
    OR_lowerCI_adj<-summary(model_adj)$conf.int[1,3]
    OR_upperCI_adj<-summary(model_adj)$conf.int[1,4]
    #输出OR值和p值
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
cox<-function(x, y_time, y_factor,adj, data,case=2){
  xx<-data[,x]
  yy_time<-data[,y_time]
  yy_factor<-data[,y_factor]
  case1<-levels(yy_factor)[case]
  #确认调整变量不同于x,y
  adj1<-setdiff(adj, c(x, y_time,y_factor))
  #model和调整model的表达式
  formula_crude<-stats::as.formula(paste0('survival::Surv(',y_time,' , as.numeric(',y_factor,')) ~ ', x))
  formula_adj<-stats::as.formula(paste0('survival::Surv(',y_time,' , as.numeric(',y_factor,')) ~ ', x, ' + ', paste(adj1, collapse = ' + ')))
  #cox回归
  model_crude<-survival::coxph(formula_crude, data = data)
  model_adj<-survival::coxph(formula_adj, data = data)
  #计算OR值 p值并输出
  if(is.factor(xx)){
    #计算p-trend
    formula_crude_trend<-stats::as.formula(paste0('survival::Surv(',y_time,' , as.numeric(',y_factor,')) ~ ', 'as.numeric(', x, ')'))
    model_crude_trend<-survival::coxph(formula_crude_trend, data = data)#diff

    formula_adj_trend<-stats::as.formula(paste0('survival::Surv(',y_time,' , as.numeric(',y_factor,')) ~ ', 'as.numeric(', x, ') + ', paste(adj1, collapse = ' + ')))
    model_adj_trend<-survival::coxph(formula_adj_trend, data = data) #diff

    trend_p_crude<-summary(model_crude_trend)$coefficients[1,5]#diff
    trend_p_adj<-summary(model_adj_trend  )$coefficients[1,5]
    #输出p-trend
    p_for_trend<-c(rep('',3),
                   ifelse(trend_p_crude < 0.001, '<0.001', sprintf("%.3f", trend_p_crude)),
                   rep('',1),
                   ifelse(trend_p_adj   < 0.001, '<0.001', sprintf("%.3f", trend_p_adj)))
    #计算OR值
    level<-length(levels(xx)) #diff
    OR_estimate_crude<-summary(model_crude)$conf.int[1:level-1, 1]
    OR_lowerCI_crude<-summary(model_crude)$conf.int[1:level-1, 3]
    OR_upperCI_crude<-summary(model_crude)$conf.int[1:level-1, 4]

    OR_estimate_adj<-summary(model_adj)$conf.int[1:level-1, 1]
    OR_lowerCI_adj<-summary(model_adj)$conf.int[1:level-1, 3]
    OR_upperCI_adj<-summary(model_adj)$conf.int[1:level-1, 4]
    #输出OR值和p值
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
    #广播
    N<-intertable[ ,1] + intertable[ ,2]
    Case<-intertable[ ,case]
    Percent<-paste0(Case, '(', sprintf('%.1f', 100*Case/N), ')')
    var_information<-data.frame(matrix(c(N, Percent, OR_crude, p_crude, OR_adj, p_adj),ncol=6))
    var_information<-rbind(var_information, data.frame(matrix(p_for_trend,nrow=1)))
  }
  else{
    #计算OR值
    OR_estimate_crude<-summary(model_crude)$conf.int[1,1]
    OR_lowerCI_crude<-summary(model_crude)$conf.int[1,3]
    OR_upperCI_crude<-summary(model_crude)$conf.int[1,4]

    OR_estimate_adj<-summary(model_adj)$conf.int[1,1]
    OR_lowerCI_adj<-summary(model_adj)$conf.int[1,3]
    OR_upperCI_adj<-summary(model_adj)$conf.int[1,4]
    #输出OR值和p值
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
