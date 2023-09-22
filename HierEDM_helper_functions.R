######
######
###### Some revised functions for the hierarchical EDM #############
##### These were copied her from empirical_analysis.Rmd on 9/1/23 ######


#devtools::install_github("tanyalrogers/GPEDM") #update frequently
library(dplyr)
library(tidyverse)
library(purrr)
library(rEDM)
library(GPEDM)
library(Metrics)

#Revise the individual ages function

indv_age <-function(df, E){
  ages <-unique(df$age_class)
  maxE <-E # 
  fitstats <-list()
  for (m in 1:length(ages)){
    tryCatch({ #error catch line
      new_df <-filter(df, age_class==ages[m])
      newdfLags = makelags(data=new_df, y="value", E=maxE, tau=1)
      new_df = cbind(new_df,newdfLags)
      new_df.train = filter(new_df, time_step <= (max(new_df$time_step)-10))
      new_df.test = filter(new_df, time_step > (max(new_df$time_step)-10))
      mod1 <-fitGP(data = new_df.train, y = "value", x=colnames(newdfLags),newdata=new_df.test,
                   pop="age_class",scaling = "global",predictmethod = "loo")
      mod1_out<-c(mod1$outsampfitstats, mod1$insampfitstats,as.character(ages[m]))
      names(mod1_out)<-c("OOS_R2","OOS_rmse","R2","rmse", "ln_post", "lnL_LOO","df","age class")
      fitstats[[m]] <- mod1_out
    }, error=function(e){}) #error catch line
  }
  fitstats <-bind_rows(fitstats)%>%as.data.frame()%>%mutate(across(OOS_R2:df, as.numeric))
}


######## Function to fit GP for one model based on it's own lags and lags of age classes above and below###
mixed_age <-function(df, maxE){
  ages <-unique(df$age_class)
  #maxE <-round(sqrt(dim(df)[1]/length(ages)))
  fitstats <-list()
  for (m in 1:length(ages)){
    #we might not need this if statement. 
    if (m==1){
      new_df <-filter(df, age_class==ages[m] | age_class == ages[m+1])
      
    } else if (m==length(ages)) {
      new_df <-filter(df, age_class==ages[m] | age_class == ages[m+1])
    } else {
      new_df <-filter(df, age_class==ages[m] | age_class==ages[m+1] | age_class==ages[m-1])
    }
    if ("age" %in% colnames(new_df)){
      new_df <-dplyr::select(new_df, -age)
    }
    
    new_df <-pivot_wider(new_df, names_from = "age_class",values_from = "value")
    #manually scale:
    lastcol <-as.numeric(dim(new_df)[2])
    new_df <- new_df %>% mutate(across(2:all_of(lastcol),scale)) 
    dfLags = makelags(y=new_df[,2:all_of(lastcol)], E=maxE, tau=1)
    dfdata = as.data.frame(cbind(new_df,dfLags))
    #testing and training (not for empirical data)
    #df.train = filter(dfdata, Year < (max(Year)-10))
    #df.test = filter(dfdata, Year >= (max(Year)-10))
    #mod1 = fitGP(data=df.train, y=ages[m], xd=colnames(dfLags), datanew=df.test, predictmethod = "loo")
    mod1 <-fitGP(data = dfdata, y = paste(ages[m]), x=colnames(dfLags), predictmethod = "loo")
    mod1_out<-c(mod1$outsampfitstats, mod1$insampfitstats,as.character(ages[m]))
    names(mod1_out)<-c("OOS_R2","OOS_rmse","R2","rmse", "ln_post", "lnL_LOO","df","age class")
    fitstats[[m]] <- mod1_out
  }
  fitstats <-bind_rows(fitstats)%>%as.data.frame()%>%mutate(across(OOS_R2:df, as.numeric))
}
