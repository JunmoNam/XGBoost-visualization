#==========================================================================
# Topic : XGBoost
# Date : 2019. 03. 30
# Author : Junmo Nam
# Blog : http://apple-rbox.tistory.com
#==========================================================================


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


#training model for importance
model = xgboost(data = x, label = as.numeric(y)-1, num_class = levels(y) %>% length, # claiming data to use
                nrounds = 200, early_stopping_rounds = 150, # about rounds
                objective = 'multi:softprob', eval_metric = 'mlogloss', # model options
                verbose = F) # do not print messages while training

xgb.importance(model = model) %>% imp.plot