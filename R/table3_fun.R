###########################################################################general_table3
general_table3<-function(x,y,split_var,adj,data,div=c(),totalcount=T){
  xx<-data[,x]
  yy<-data[,y]
  ss<-data[,split_var]
  dataframe<-''
  adj1<-setdiff(adj, c(x, y,split_var))
  if(length(adj1)==0){
    formula_adj<-stats::as.formula(paste0(y, ' ~ ', x ))
  }else{
    formula_adj<-stats::as.formula(paste0(y, ' ~ ', x , ' + ', paste(adj1, collapse = ' + ')))
  }
  ###split_var is a categorical variable###
  if(is.factor(ss)==TRUE){
    sdata<-split(data,data[,split_var])
    #Compute p for interaction
    if(length(adj1)==0){
      formula1<-stats::as.formula(paste0(y,' ~ ', x , ' + ', split_var))
      formula2<-stats::as.formula(paste0(y, ' ~ ', x , ' * ',split_var))
    }else{
      formula1<-stats::as.formula(paste0(y,' ~ ', x , ' + ', split_var,' + ',paste(adj1, collapse = ' + ')))
      formula2<-stats::as.formula(paste0(y, ' ~ ', x , ' * ',split_var,' + ', paste(adj1, collapse = ' + ')))
    }
    model1<-stats::glm(formula1,data=data, family = stats::gaussian())
    model2<-stats::glm(formula2,data=data, family = stats::gaussian())
    p<-lmtest::lrtest(model1,model2)$`Pr(>Chisq)`[2]
  }
  ###split_var is a continuous###
  else{
    if(length(div)==0){
      split_var_factor<-div_quantile(split_var,2,data)
      divdata<-data
      divdata[,split_var]<-factor(split_var_factor)
      sdata<-split(divdata,divdata[,split_var])
    }else{
      if(mode(div)=="character"){
        div_num<-as.numeric(div)
        split_var_factor<-div_quantile(split_var,div_num,data)
        divdata<-data
        divdata[,split_var]<-factor(split_var_factor)
        sdata<-split(divdata,divdata[,split_var])
      }else{
        split_var_factor<-div_custom(split_var,div,data)
        divdata<-data
        divdata[,split_var]<-factor(split_var_factor)
        sdata<-split(divdata,divdata[,split_var])
      }
    }
    #Comupute p for interaction
    if(length(adj1)==0){
      formula1<-stats::as.formula(paste0(y, ' ~ ', x , ' + as.factor(', split_var,')'))
      formula2<-stats::as.formula(paste0(y, ' ~ ', x , ' * as.factor(',split_var,')'))
    }else{
      formula1<-stats::as.formula(paste0(y, ' ~ ', x , ' + as.factor(', split_var,')+ ',paste(adj1, collapse = ' + ')))
      formula2<-stats::as.formula(paste0(y, ' ~ ', x , ' * as.factor(',split_var,')+ ', paste(adj1, collapse = ' + ')))
    }
    model1<-stats::glm(formula1,data=divdata, family = stats::gaussian())
    model2<-stats::glm(formula2,data=divdata, family = stats::gaussian())
    p<-lmtest::lrtest(model1,model2)$`Pr(>Chisq)`[2]
  }
  j_len<-ifelse(length(levels(xx))==0,2,length(levels(xx)))
  for (i in 1:length(sdata)){
    model<-stats::glm(formula_adj, data = sdata[[i]], family = stats::gaussian())
    model<-summary(model)
    number<-length(sdata[[i]][,y])
    mean_y<-round(mean(sdata[[i]][,y]),1)
    sd_y<-round(stats::sd(sdata[[i]][,y]),1)
    quantile_y<-round(stats::quantile(sdata[[i]][,y],1:3/4),1)
    #Compute descriptive statistics
    sdata1<-split(sdata[[i]],sdata[[i]][,x])[[1]]
    number1<-length(sdata1[,y])
    mean1<-round(mean(sdata1[,y]),1)
    sd1<-round(stats::sd(sdata1[,y]),1)
    quantile1<-round(stats::quantile(sdata1[,y],1:3/4),1)
    if(totalcount==T){
      output_data<-data.frame('N'=number,
                              'Mean'=mean_y,
                              'sd'=sd_y,
                              'Median'=quantile_y[2],
                              'Q1'=quantile_y[1],
                              'Q3'=quantile_y[3])
    }
    else{
      output_data<-data.frame('N_1'=number1,
                              'Mean_1'=mean1,
                              'sd_1'=sd1,
                              'Median_1'=quantile1[2],
                              'Q1_1'=quantile1[1],
                              'Q3_1'=quantile1[3])
      for(j in 2:j_len){
        #Compute descriptive statistics for each group
        sdata2<-split(sdata[[i]],sdata[[i]][,x])[[j]]
        number2<-length(sdata2[,y])
        mean2<-round(mean(sdata2[,y]),1)
        sd2<-round(stats::sd(sdata2[,y]),1)
        quantile2<-round(stats::quantile(sdata2[,y],1:3/4),1)
        #output data frame
        output_data[,paste0('N_',j)]<-number2
        output_data[,paste0('Mean_',j)]<-mean2
        output_data[,paste0('sd_',j)]<-sd2
        output_data[,paste0('Median_',j)]<-quantile2[2]
        output_data[,paste0('Q1_',j)]<-quantile2[1]
        output_data[,paste0('Q3_',j)]<-quantile2[3]
      }
    }
    for(j in 2:j_len){
      OR_estimate<-model$coefficients[j, 1]
      OR_lowerCI<-model$coefficients[j, 1] + stats::qnorm(0.025)*model$coefficients[j, 2]
      OR_upperCI<-model$coefficients[j, 1] + stats::qnorm(0.975)*model$coefficients[j, 2]
      p_value<-model$coefficients[j, 4]
      #output data frame
      output_data[,paste('beta_estimate_',j,sep = '')]<-round(OR_estimate,2)
      output_data[,paste('beta_lowerCI_',j,sep = '')]<-round(OR_lowerCI,2)
      output_data[,paste('beta_upperCI_',j,sep = '')]<-round(OR_upperCI,2)
      output_data[,paste('p_value',j,sep = '')]<-ifelse(p_value < 0.001,'<0.001',round(p_value,3))
    }
    dataframe<-rbind(dataframe,output_data)
  }
  if(is.factor(ss)==TRUE){
    dataframe[,'p']<-c(round(p,3),rep('',length(levels(ss))))
    rownames(dataframe)<-c(split_var,paste('  ',levels(data[,split_var])[1:length(levels(ss))]))
  }else{
    if(length(div)==0){
      dataframe[,'p']<-c(round(p,3),rep('',2))
      rownames(dataframe)<-c(split_var,levels(split_var_factor))
    }
    else{
      if(mode(div)=="character"){
        dataframe[,'p']<-c(round(p,3),rep('',ifelse(length(div_num)==1, div_num, length(div_num))))
        rownames(dataframe)<-c(split_var,levels(split_var_factor))
      }else{
        dataframe[,'p']<-c(round(p,3),rep('',length(div)+1))
        rownames(dataframe)<-c(split_var,levels(div_custom(split_var,div,data)))
      }
    }
  }
  return(dataframe)
}


##############################################################################logistic_table3
logistic_table3<-function(x,y,split_var,adj,data,div=c(),totalcount=T){
  xx<-data[,x]
  yy<-data[,y]
  ss<-data[,split_var]
  dataframe<-''
  adj1<-setdiff(adj, c(x, y,split_var))
  if(length(adj1)==0){
    formula_adj<-stats::as.formula(paste0(y, ' ~ ', x ))
  }else{
    formula_adj<-stats::as.formula(paste0(y, ' ~ ', x , ' + ', paste(adj1, collapse = ' + ')))
  }
  ###split_var is a categorical variable###
  if(is.factor(ss)==TRUE){
    sdata<-split(data,data[,split_var])
    #Compute p for interaction
    if(length(adj1)==0){
      formula1<-stats::as.formula(paste0('as.factor(',y, ') ~ ', x , ' + ', split_var))
      formula2<-stats::as.formula(paste0('as.factor(',y, ') ~ ', x , ' * ',split_var))
    }else{
      formula1<-stats::as.formula(paste0('as.factor(',y, ') ~ ', x , ' + ', split_var,' + ',paste(adj1, collapse = ' + ')))
      formula2<-stats::as.formula(paste0('as.factor(',y, ') ~ ', x , ' * ',split_var,' + ', paste(adj1, collapse = ' + ')))
    }
    model1<-stats::glm(formula1,data=data, family = stats::binomial(link = "logit"))
    model2<-stats::glm(formula2,data=data, family = stats::binomial(link = "logit"))
    p<-lmtest::lrtest(model1,model2)$`Pr(>Chisq)`[2]
  }
  ###split_var is a continuous variable###
  else{
    if(length(div)==0){
      split_var_factor<-div_quantile(split_var,2,data)
      divdata<-data
      divdata[,split_var]<-factor(split_var_factor)
      sdata<-split(divdata,divdata[,split_var])
    }else{
      if(mode(div)=="character"){
        div_num<-as.numeric(div)
        split_var_factor<-div_quantile(split_var,div_num,data)
        divdata<-data
        divdata[,split_var]<-factor(split_var_factor)
        sdata<-split(divdata,divdata[,split_var])
      }else{
        split_var_factor<-div_custom(split_var,div,data)
        divdata<-data
        divdata[,split_var]<-factor(split_var_factor)
        sdata<-split(divdata,divdata[,split_var])
      }
    }
    #Compute p for interaction
    if(length(adj1)==0){
      formula1<-stats::as.formula(paste0('as.factor(',y, ') ~ ', x , ' + as.factor(', split_var,')'))
      formula2<-stats::as.formula(paste0('as.factor(',y, ') ~ ', x , ' * as.factor(',split_var,')'))
    }else{
      formula1<-stats::as.formula(paste0('as.factor(',y, ') ~ ', x , ' + as.factor(', split_var,')+ ',paste(adj1, collapse = ' + ')))
      formula2<-stats::as.formula(paste0('as.factor(',y, ') ~ ', x , ' * as.factor(',split_var,')+ ', paste(adj1, collapse = ' + ')))
    }
    model1<-stats::glm(formula1,data=divdata, family = stats::binomial(link = "logit"))
    model2<-stats::glm(formula2,data=divdata, family = stats::binomial(link = "logit"))
    p<-lmtest::lrtest(model1,model2)$`Pr(>Chisq)`[2]
  }
  j_len<-ifelse(length(levels(xx))==0,2,length(levels(xx)))
  for (i in 1:length(sdata)){
    model<-stats::glm(formula_adj, data = sdata[[i]], family = stats::binomial(link = "logit"))
    model<-summary(model)
    number<-table(sdata[[i]][,y])
    count<-number[1]+number[2]
    precent<-round(number[2]/(number[1]+number[2])*100,1)
    #Compute descriptive statistics
    sdata1<-split(sdata[[i]],sdata[[i]][,x])[[1]]
    number1<-table(sdata1[,y])
    count1<-number1[1]+number1[2]
    precent1<-round(number1[2]/(number1[1]+number1[2])*100,1)
    if(totalcount==T){
      output_data<-data.frame('N'=count,
                              'Percent'=precent,
                              'Case'=number[2],
                              'Control'=number[1])
    }
    else{
      output_data<-data.frame('N_1'=count1,
                              'Percent_1'=precent1,
                              'Case_1'=number1[2],
                              'Control_1'=number1[1])
      for(j in 2:j_len){
        #Compute descriptive statistics for each group
        sdata2<-split(sdata[[i]],sdata[[i]][,x])[[j]]
        number2<-table(sdata2[,y])
        count2<-number2[1]+number2[2]
        precent2<-round(number2[2]/(number2[1]+number2[2])*100,1)
        #output data frame
        output_data[,paste0('N_',j)]<-count2
        output_data[,paste0('Percent_',j)]<-precent2
        output_data[,paste0('Case_',j)]<-number2[2]
        output_data[,paste0('Control_',j)]<-number2[1]
      }
    }
    for(j in 2:j_len){
      OR_estimate<-exp(model$coefficients[j, 1])
      OR_lowerCI<-exp(model$coefficients[j, 1] + stats::qnorm(0.025)*model$coefficients[j, 2])
      OR_upperCI<-exp(model$coefficients[j, 1] + stats::qnorm(0.975)*model$coefficients[j, 2])
      p_value<-exp(model$coefficients[j, 4])
      #output data frame
      output_data[,paste('OR_estimate_',j,sep = '')]<-round(OR_estimate,2)
      output_data[,paste('OR_lowerCI_',j,sep = '')]<-round(OR_lowerCI,2)
      output_data[,paste('OR_upperCI_',j,sep = '')]<-round(OR_upperCI,2)
      output_data[,paste('p_value',j,sep = '')]<-ifelse(p_value < 0.001,'<0.001',round(p_value,3))
    }
    dataframe<-rbind(dataframe,output_data)
  }
  if(is.factor(ss)==TRUE){
    dataframe[,'p']<-c(round(p,3),rep('',length(levels(ss))))
    rownames(dataframe)<-c(split_var,paste('  ',levels(data[,split_var])[1:length(levels(ss))]))
  }else{
    if(length(div)==0){
      dataframe[,'p']<-c(round(p,3),rep('',2))
      rownames(dataframe)<-c(split_var,levels(split_var_factor))
    }
    else{
      if(mode(div)=="character"){
        dataframe[,'p']<-c(round(p,3),rep('',ifelse(length(div_num)==1, div_num, length(div_num))))
        rownames(dataframe)<-c(split_var,levels(split_var_factor))
      }else{
        dataframe[,'p']<-c(round(p,3),rep('',length(div)+1))
        rownames(dataframe)<-c(split_var,levels(div_custom(split_var,div,data)))
      }
    }
  }
  return(dataframe)
}

################################################################cox_table3
cox_table3<-function(x,y_time,y_factor,split_var,adj,data,div=c(),totalcount=T){
  xx<-data[,x]
  yy_time<-data[,y_time]
  yy_factor<-data[,y_factor]
  ss<-data[,split_var]
  dataframe<-''
  adj1<-setdiff(adj, c(x, y_time,y_factor,split_var))
  if(length(adj1)==0){
    formula_adj<-stats::as.formula(paste0('survival::Surv(',y_time,', as.numeric(',y_factor,')) ~ ', x))
  }else{
    formula_adj<-stats::as.formula(paste0('survival::Surv(',y_time,', as.numeric(',y_factor,')) ~ ', x, ' + ', paste(adj1, collapse = ' + ')))
  }
  ###Split_var is a categorical variable###
  if(is.factor(ss)==TRUE){
    sdata<-split(data,data[,split_var])
    #Compute p for interaction
    if(length(adj1)==0){
      formula1<-stats::as.formula(paste0('survival::Surv(',y_time,', as.numeric(',y_factor, ')) ~ ', x , ' + as.factor(', split_var,')'))
      formula2<-stats::as.formula(paste0('survival::Surv(',y_time,', as.numeric(',y_factor, ')) ~ ', x , ' * as.factor(',split_var,')'))
    }else{
      formula1<-stats::as.formula(paste0('survival::Surv(',y_time,', as.numeric(',y_factor, ')) ~ ', x , ' + as.factor(', split_var,')+ ',paste(adj1, collapse = ' + ')))
      formula2<-stats::as.formula(paste0('survival::Surv(',y_time,', as.numeric(',y_factor, ')) ~ ', x , ' * as.factor(',split_var,') + ', paste(adj1, collapse = ' + ')))
    }
    model1<-survival::coxph(formula1,data=data)
    model2<-survival::coxph(formula2,data=data)
    p<-lmtest::lrtest(model1,model2)$`Pr(>Chisq)`[2]
  }
  ###Split_var is a continuous variable###
  else{
    if(length(div)==0){
      split_var_factor<-div_quantile(split_var,2,data)
      divdata<-data
      divdata[,split_var]<-factor(split_var_factor)
      sdata<-split(divdata,divdata[,split_var])
    }else{
      if(mode(div)=="character"){
        div_num<-as.numeric(div)
        split_var_factor<-div_quantile(split_var,div_num,data)
        divdata<-data
        divdata[,split_var]<-factor(split_var_factor)
        sdata<-split(divdata,divdata[,split_var])
      }else{
        split_var_factor<-div_custom(split_var,div,data)
        divdata<-data
        divdata[,split_var]<-factor(split_var_factor)
        sdata<-split(divdata,divdata[,split_var])
      }
    }
    #Compute p for interaction
    if(length(adj1)==0){
      formula1<-stats::as.formula(paste0('survival::Surv(',y_time,', as.numeric(',y_factor, ')) ~ ', x , ' + ', split_var))
      formula2<-stats::as.formula(paste0('survival::Surv(',y_time,', as.numeric(',y_factor, ')) ~ ', x , ' * ',split_var))
    }else{
      formula1<-stats::as.formula(paste0('survival::Surv(',y_time,', as.numeric(',y_factor, ')) ~ ', x , ' + ', split_var,' + ',paste(adj1, collapse = ' + ')))
      formula2<-stats::as.formula(paste0('survival::Surv(',y_time,', as.numeric(',y_factor, ')) ~ ', x , ' * ',split_var,' + ', paste(adj1, collapse = ' + ')))
    }
    model1<-survival::coxph(formula1,data=divdata)
    model2<-survival::coxph(formula2,data=divdata)
    p<-lmtest::lrtest(model1,model2)$`Pr(>Chisq)`[2]
  }
  j_len<-ifelse(length(levels(xx))==0,2,length(levels(xx)))
  for (i in 1:length(sdata)){
    model<-survival::coxph(formula_adj, data = sdata[[i]])
    model<-summary(model)
    number<-table(sdata[[i]][,y_factor])
    count<-number[1]+number[2]
    precent<-round(number[2]/(number[1]+number[2])*100,1)
    #Compute descriptive statistics
    sdata1<-split(sdata[[i]],sdata[[i]][,x])[[1]]
    number1<-table(sdata1[,y_factor])
    count1<-number1[1]+number1[2]
    precent1<-round(number1[2]/(number1[1]+number1[2])*100,1)
    if(totalcount==T){
      output_data<-data.frame('N'=count,
                              'Percent'=precent,
                              'Case'=number[2],
                              'Control'=number[1])
    }
    else{
      output_data<-data.frame('N_1'=number1[1]+number1[2],
                              'Percent_1'=precent1,
                              'Case_1'=number1[2],
                              'Control_1'=number1[1])
      for(j in 2:j_len){
        #Compute descriptive statistics for each group
        sdata2<-split(sdata[[i]],sdata[[i]][,x])[[j]]
        number2<-table(sdata2[,y_factor])
        count2<-number2[1]+number2[2]
        precent2<-round(number2[2]/(number2[1]+number2[2])*100,1)
        #output data frame
        output_data[,paste0('N_',j)]<-count2
        output_data[,paste0('Percent_',j)]<-precent2
        output_data[,paste0('Case_',j)]<-number2[2]
        output_data[,paste0('Control_',j)]<-number2[1]
      }
    }
    for(j in 2:j_len){
      OR_estimate<-model$conf.int[j-1,1]
      OR_lowerCI<-model$conf.int[j-1,3]
      OR_upperCI<-model$conf.int[j-1,4]
      p_value<-model$coefficients[j-1,5]
      #output data frame
      output_data[,paste('HR_estimate_',j,sep = '')]<-round(OR_estimate,2)
      output_data[,paste('HR_lowerCI_',j,sep = '')]<-round(OR_lowerCI,2)
      output_data[,paste('HR_upperCI_',j,sep = '')]<-round(OR_upperCI,2)
      output_data[,paste('p_value',j,sep = '')]<-ifelse(p_value < 0.001,'<0.001',round(p_value,3))
    }
    dataframe<-rbind(dataframe,output_data)
  }
  if(is.factor(ss)==TRUE){
    dataframe[,'p']<-c(round(p,3),rep('',length(levels(ss))))
    rownames(dataframe)<-c(split_var,paste('  ',levels(data[,split_var])[1:length(levels(ss))]))
  }else{
    if(length(div)==0){
      dataframe[,'p']<-c(round(p,3),rep('',2))
      rownames(dataframe)<-c(split_var,levels(split_var_factor))
    }
    else{
      if(mode(div)=="character"){
        dataframe[,'p']<-c(round(p,3),rep('',ifelse(length(div_num)==1, div_num, length(div_num))))
        rownames(dataframe)<-c(split_var,levels(split_var_factor))
      }else{
        dataframe[,'p']<-c(round(p,3),rep('',length(div)+1))
        rownames(dataframe)<-c(split_var,levels(div_custom(split_var,div,data)))
      }
    }
  }
  return(dataframe)
}
