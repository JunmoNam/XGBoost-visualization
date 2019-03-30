#==========================================================================
# Topic : XGBoost
# Date : 2019. 03. 30
# Author : Junmo Nam
# Blog : http://apple-rbox.tistory.com
#==========================================================================






#visualizing cross-validation model
cvplot = function(model){ #visualizing function
  eval.log = model$evaluation_log
  
  std = names(eval.log[,2]) %>% gsub('train_','',.) %>% gsub('_mean','',.)
  
  data.frame(error = c(unlist(eval.log[,2]),unlist(eval.log[,4])),
             class = c(rep('train',nrow(eval.log)),
                       rep('test',nrow(eval.log))),
             nround = rep(1:nrow(eval.log),2)
  ) %>%
    ggplot(aes(nround,error,col = class))+
    geom_point(alpha = 0.2)+
    geom_smooth(alpha = 0.4,se = F)+
    theme_bw()+
    ggtitle("XGBoost Cross-validation Visualization",
            subtitle = paste0('fold : ',length(model$folds),
                              '  iteration : ',model$niter
                              )
    )+ylab(std)+theme(axis.title=element_text(size=11))
}


#importance visualization
imp.plot = function(imp){
  data.frame(variable = rep(imp$Feature,3),
             value = c(imp$Gain,imp$Cover,imp$Frequency),
             Type = c(rep('Gain',nrow(imp)),rep('Cover',nrow(imp)),rep('Frequency',nrow(imp)))
  ) %>% ggplot(aes(variable,value,fill = variable))+
    geom_bar(stat = 'identity')+
    facet_grid(~Type)+
    theme_bw()+
    ggtitle('XGBoost : Customized Importance Plot',
            subtitle = "Author : Junmo Nam")

}




#==========================================================================
# Example
#==========================================================================

sapply(c('xgboost','dplyr','ggplot2'),require,character.only = T)

x = datasets::iris %>% select(-Species) %>%
  data.matrix #matrix format
y = datasets::iris$Species


#training cv model
cv_model = xgb.cv(data = x, label = as.numeric(y)-1, num_class = levels(y) %>% length, # claiming data to use
                   nfold = 5, nrounds = 200, early_stopping_rounds = 150, # about folds and rounds
                   objective = 'multi:softprob', eval_metric = 'mlogloss', # model options
                   verbose = F, prediction = T) # do not print messages while training, make prediction


cvplot(cv_model)

#training model for importance
model = xgboost(data = x, label = as.numeric(y)-1, num_class = levels(y) %>% length, # claiming data to use
                       nrounds = 200, early_stopping_rounds = 150, # about rounds
                       objective = 'multi:softprob', eval_metric = 'mlogloss', # model options
                       verbose = F) # do not print messages while training

xgb.importance(model = model) %>% imp.plot
