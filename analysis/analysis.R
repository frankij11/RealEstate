global.time <- proc.time()
source("real_estate.R")
source("median_analysis.R")

library(randomForest)
library(caret)
library(RANN)

# used to reset data
df <- df.raw 
#df.median <- df.median.raw
#df <- df.median %>% dplyr::select(-city, -zipcode, -starts_with("price")) %>% mutate(price = df.median$price.95)

df$price.50.est <- predict(county.mods$lm.50, df)

df$monthSold <- as.factor(df$monthSold)

# df <- df %>% 
#   mutate(r.sqft = round(sqft, -2)) %>%
#   group_by(land_use, county,r.sqft) %>% 
#   mutate(quality_county = percent_rank(price)) %>%
#   ungroup() %>%
#   dplyr::select(-r.sqft)


df <- df %>% 
  mutate(r.sqft = round(sqft, -2)) %>%
  group_by(land_use, zipcode,r.sqft) %>% 
  mutate(quality_zipcode = percent_rank(price)) %>%
  ungroup() %>%
  dplyr::select(-r.sqft)



df <- df %>% select(-county,-zipcode)

# Only use complete cases
df <- df[complete.cases(df),]

# Pre-Process
# pp <- preProcess(df %>% select(-price),
#                 method = c("knnImpute"),
#                 thresh = .95,
#                 k=3)

# df <- predict(pp, df)

# Dummy variables
# df.dummy <- dummyVars(price~.,data = df)
# df <- predict(df.dummy, df) %>% as_tibble()
# df$price <- df.raw$price


# correlated variables


# Train Split
set.seed(123)
inTrain <- createDataPartition(df$price, p=.8, 
                               times = 1, 
                               list=F) %>% as.vector()
df.HoldOut <- df[-inTrain,]
df <- df[inTrain,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated one times
  repeats = 1)

print("Starting Analysis")

mods <- list()
timer <- c()
s = proc.time()
# determine maximum number of variables possible
nVars = model.matrix(price~.-1, df) %>% ncol() - length(sapply(df, is.factor))
set.seed(123)
tune.grid <- expand.grid(nvmax=seq(round(ncol(df)/2),nVars))
mods$lm.leaps <- train(price~., 
                 data =df,
                 metric = "RMSE",
                 method = "leapBackward", # library(MASS)
                 trControl = fitControl,
                 na.action = na.omit,
                 tuneGrid = tune.grid,
                 trace=F
                 
                 )
timer <- bind_rows(timer, (proc.time() - s))

s = proc.time()
set.seed(123)
tune.grid <- NULL 
mods$lm.step <- step(lm(price~.,data =df), trace=0)
timer <- bind_rows(timer, (proc.time() - s))


s = proc.time()
set.seed(123)
tune.grid <- expand.grid(alpha = 1, lambda=10^seq(-5,10))
mods$lasso <- train(price~., 
                 data =df,
                 metric = "RMSE",
                 method = "glmnet", # library(glmnet)
                 trControl = fitControl,
                 na.action = na.omit,
                 tuneGrid = tune.grid,
                 trace=F
                 
)
timer <- bind_rows(timer, (proc.time() - s))

s = proc.time()
set.seed(123)
tune.grid <- expand.grid(alpha = 0, lambda=10^seq(-5,10))
mods$ridge <- train(price~., 
                    data =df,
                    metric = "RMSE",
                    method = "glmnet", # library(glmnet)
                    trControl = fitControl,
                    na.action = na.omit,
                    tuneGrid = tune.grid,
                    trace=F
                    
)
timer <- bind_rows(timer, (proc.time() - s))


s = proc.time()
set.seed(123)
tune.grid <- expand.grid(alpha = seq(.1,.9, by=.1), lambda=10^seq(-5,10))
mods$enet <- train(price~., 
                    data =df,
                    metric = "RMSE",
                    method = "glmnet", # library(glmnet)
                    trControl = fitControl,
                    na.action = na.omit,
                    tuneGrid = tune.grid,
                    trace=F
                    
)
timer <- bind_rows(timer, (proc.time() - s))


print("starting random forest")
s = proc.time()
set.seed(123)
nVars <- ncol(model.matrix(price~.,df))
              
tune.grid <- expand.grid(mtry = floor(c(nVars/2,nVars-2)),
                         splitrule = c("extratrees"),
                         min.node.size = c(10,15)
                         )
mods$rf <- train(price~., 
                   data =df,
                   metric = "RMSE",
                   method = "ranger", # library(glmnet)
                   trControl = fitControl,
                   na.action = na.omit,
                   num.trees=200,
                   tuneGrid = tune.grid
                   
                   
)
timer <- bind_rows(timer, (proc.time() - s))

print("starting xgbtree")

s = proc.time()
set.seed(123)
tune.grid <- expand.grid(
  nrounds = c(200,500),
  max_depth = c(3,6),
  eta = 0.3,
  gamma = c(0,5),
  colsample_bytree = c(.8),
  min_child_weight = 1,
  subsample = c(1)
)
mods$xgb <- train(price~.,
                  data=df,
                  method = "xgbTree",
                  trControl = fitControl,
                  tuneGrid = tune.grid,
                  verbose=F)
print(proc.time() - s)
timer <- bind_rows(timer, (proc.time() - s))

# Log not included in training set
#df.HoldOut <- df.HoldOut[df.HoldOut$material != "Log (013)",]
#df.pred <- bind_cols(df,sapply(mods, predict, df) %>% as.data.frame())
df.pred <- sapply(mods, predict, df) %>% as.data.frame()
df.pred$price <- df$price


#pp <- preProcess(df.pred %>% select(-price), method=c("knnImpute", "pca"), drop=T)
#df.pred <- predict(pp, df.pred)
# s = proc.time()
 set.seed(123)
 ensemable <- train(price~., 
                         data=df.pred, 
                         method="enet",
                         trControl =fitControl,
                         na.action = na.omit
                        
 )
# print(proc.time()-s)
# timer <- bind_rows(timer, (proc.time() - s))

getScores <- function(mods, df, y=df$price, data.type){
  results <- NULL
  for(i in 1:length(mods)){
    pred = predict(mods[i], df) %>% as.data.frame()
    
    tmp <- postResample(pred, y) %>% t() %>% as.data.frame() %>% mutate(Model = names(mods[i])[1], Data = data.type)
    results <- bind_rows(results, tmp)
  }
  results <- results %>% dplyr::select(Model,Data, everything())
  return(results)
}




results <- bind_rows(getScores(mods, df.HoldOut,df.HoldOut$price, data.type="Test"),
         getScores(mods, df,df$price, data.type="Train")) %>% 
  pivot_wider(names_from="Data",values_from=c("RMSE", "Rsquared", "MAE")) 

timer <- timer %>% mutate(Model = names(mods), Minutes = round(elapsed / 60,2)) %>% dplyr::select(Model, Minutes)
#only run when you have time!!!



#################
## Final MODEL ##
#################
df.all <- bind_rows(df, df.HoldOut)
MODEL <- train(price~.,
               data=df.all, 
               method = "ranger",
               trControl = fitControl,
               num.trees=200,
               tuneGrid = mods$rf$bestTune)


#df.median.mod <- df.median %>% filter(!is.na(price.50)) %>% dplyr::select(-dplyr::starts_with("price"))
#df.median.mod$price.50 <- df.median$price.50[!is.na(df.median$price.50)]
  
#mods.med <- train(price.50 ~., 
#                  data = df.median.mod,
#                  method="enet", 
#                  na.action = na.omit,
#                  trControl = fitControl)

## Write results to file
write.csv(df,"results/df.csv", row.names=F)
#write.csv(df.median,"results/df_median.csv", row.names=F)
saveRDS(mods,"results/mods.rds")
saveRDS(results, "results/results.rds")
saveRDS(timer,"results/timer.rds")
saveRDS(MODEL, "results/final_model.rds")

#save.image("results/analysis.RData")


global.time <- proc.time() - global.time
print(global.time)
