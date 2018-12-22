rm(list=ls())

#SETTING WORKING DIRECTORY
setwd("F:/project 2")
# Load all the packages required for the analysis
library(ggplot2)
library(dplyr)
library(e1071)
library(randomForest)
library(scales)
library(gplots)
x = c( "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "Information",
       "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','readxl')
lapply(x, require, character.only = TRUE)
library(fastDummies)
library(VIM)
library(rms)
#IMPORTING DATA
data =data.frame(read_excel("project2.xls"))

###########Expolatry data analysis##############################
colnames(data)
continuous_vars = c("Transportation.expense","Distance.from.Residence.to.Work","Service.time","Age",
                    "Work.load.Average.day","Hit.target","Son","Pet","Weight","Height","Body.mass.index"
                    ,"Absenteeism.time.in.hours")

catagorical_vars = c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week",
                     "Seasons","Disciplinary.failure", "Education", "Social.drinker",
                     "Social.smoker")


#Missing values analysis
# creating dataframe with missing value  persentage
Mising_val=data.frame(apply(data, 2,function(x){sum(is.na(x))}))
Mising_val$Columns = row.names(Mising_val)
names(Mising_val)[1] =  "Missing_percentage"
Mising_val$Missing_percentage = (Mising_val$Missing_percentage/nrow(data)) * 100
Mising_val = Mising_val[order(-Mising_val$Missing_percentage),]
row.names(Mising_val) = NULL
Mising_val = Mising_val[,c(2,1)]

# kNN Imputation
data=knnImputation(data,k=5)

############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
for (i in 1:length(continuous_vars))
   {
     assign(paste0("gn",i), ggplot(aes_string(y = (continuous_vars[i]), x = data$Absenteeism.time.in.hours), data = subset(data))+ 
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=continuous_vars[i],x= data$Absenteeism.time.in.hours)+
              ggtitle(paste("Box plot of responded for",continuous_vars[i])))
}

 ## Plotting plots together
 gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
 gridExtra::grid.arrange(gn6,gn7,ncol=2)
 gridExtra::grid.arrange(gn8,gn9,ncol=2)
 
 # #Replace all outliers with NA and impute using knn
  for(i in continuous_vars){
    val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
    data[,i][data[,i] %in% val] = NA
  }
  
  data = knnImputation(data, k = 5)
 ##################################Feature Selection################################################
 ## Correlation Plot 
 corrgram(data[,continuous_vars], order = F,
          upper.panel=panel.pie,text.panel=panel.txt, main = "Correlation Plot")
  
  
  ## Dimension Reduction
  
  data = subset(data,select = -c(Weight))
  
  #--------------------------------Feature Scaling--------------------------------#
  #Normality check
  hist(data$Absenteeism.time.in.hours)
  
  # Updating the continuous and catagorical variable
  continuous_vars = c("Transportation.expense","Distance.from.Residence.to.Work","Service.time","Age",
                      "Work.load.Average.day","Hit.target","Son","Pet","Height","Body.mass.index"
                      ,"Absenteeism.time.in.hours")
  
  catagorical_vars = c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week",
                       "Seasons","Disciplinary.failure", "Education", "Social.drinker",
                       "Social.smoker")
  
  
  # Normalization
  for(i in continuous_vars)
  {
    print(i)
    data[,i] = (data[,i] - min(data[,i]))/(max(data[,i])-min(data[,i]))
  }

  ########################  Creating Dummy variables ######################
  str(data)
  data = dummy.data.frame(data, catagorical_vars)
  
  ###################Model Development#####################################
  rmExcept("data")
  #Divide the data into train and test
  train_index = sample(1:nrow(data), 0.8*nrow(data))
  train=data[train_index,]
  test=data[-train_index,]
  
  ##Decision tree for classification
  #Develop Model on training data
  fit_DT = rpart(Absenteeism.time.in.hours ~., data = train, method = "anova")
  
  #Summary of DT model
  summary(fit_DT)
  
  #write rules into disk
  write(capture.output(summary(fit_DT)), "Rules.txt")
  
  #Lets predict for training data
  pred_DT_train = predict(fit_DT, train[,names(test) != "Absenteeism.time.in.hours"])
  
  #Lets predict for training data
  pred_DT_test = predict(fit_DT,test[,names(test) != "Absenteeism.time.in.hours"])
  
  
  # For training data 
  print(postResample(pred = pred_DT_train, obs = train[,107]))
  
  # For testing data 
  print(postResample(pred = pred_DT_test, obs = test[,107]))
  
  
  #------------------------------------------Linear Regression-------------------------------------------#
  #Develop Model on training data
  fit_LR = lm(Absenteeism.time.in.hours ~ ., data = train)
  
  #Lets predict for training data
  pred_LR_train = predict(fit_LR, train[,names(test) != "Absenteeism.time.in.hours"])
  
  #Lets predict for testing data
  pred_LR_test = predict(fit_LR,test[,names(test) != "Absenteeism.time.in.hours"])
  
  # For training data 
  print(postResample(pred = pred_LR_train, obs = train[,107]))
  
  # For testing data 
  print(postResample(pred = pred_LR_test, obs = test[,107]))
  
  
  #-----------------------------------------Random Forest----------------------------------------------#
  
  
  #Develop Model on training data
  fit_RF = randomForest(Absenteeism.time.in.hours~., data = train , importance= TRUE ,ntree=500)
  
  #Lets predict for training data
  pred_RF_train = predict(fit_RF, train[,names(test) != "Absenteeism.time.in.hours"])
  
  #Lets predict for testing data
  pred_RF_test = predict(fit_RF,test[,names(test) != "Absenteeism.time.in.hours"])
  
  # For training data 
  print(postResample(pred = pred_RF_train, obs = train[,107]))
  
  # For testing data 
  print(postResample(pred = pred_RF_test, obs = test[,107]))
  
  
  
  
  
  #----------------------Dimensionality Reduction using PCA-------------------------------#
  
  
  #principal component analysis
  prin_comp = prcomp(train)
  
  #compute standard deviation of each principal component
  std_dev = prin_comp$sdev
  
  #compute variance
  pr_var = std_dev^2
  
  #proportion of variance explained
  prop_varex = pr_var/sum(pr_var)
  
  #cumulative scree plot
  plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
  
  #add a training set with principal components
  train.data = data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, prin_comp$x)
  
  # From the above plot selecting 45 components since it explains almost 95+ % data variance
  train.data =train.data[,1:45]
  
  #transform test into PCA
  test.data = predict(prin_comp, newdata = test)
  test.data = as.data.frame(test.data)
  
  #select the first 45 components
  test.data=test.data[,1:45]
  
 
  #------------------------------------Model Development after Dimensionality Reduction--------------------------------------------#
  
  #-------------------------------------------Decision tree for classification-------------------------------------------------#
  
  #Develop Model on training data
  fit_DT = rpart(Absenteeism.time.in.hours ~., data = train.data, method = "anova")
  
  
  #Lets predict for training data
  pred_DT_train = predict(fit_DT, train.data)
  
  #Lets predict for training data
  pred_DT_test = predict(fit_DT,test.data)
  
  
  # For training data 
  print(postResample(pred = pred_DT_train, obs = train$Absenteeism.time.in.hours))
  
  # For testing data 
  print(postResample(pred = pred_DT_test, obs = test$Absenteeism.time.in.hours))
  
  
   
  #LINEAR REGRESTION MODEL
  lm_model = lm(Absenteeism.time.in.hours ~., data = train.data)
  #Lets predict for training data
  pred_LR_train = predict(lm_model, train.data)
  
  #Lets predict for testing data
  pred_LR_test = predict(lm_model,test.data)
  
  # For training data 
  print(postResample(pred = pred_LR_train, obs = train$Absenteeism.time.in.hours))
  
  # For testing data 
  print(postResample(pred = pred_LR_test, obs =test$Absenteeism.time.in.hours))
  
  
  #-----------------------------------------Random Forest----------------------------------------------#
  
  #Develop Model on training data
  fit_RF = randomForest(Absenteeism.time.in.hours~., data = train.data , importance= TRUE ,ntree=500)
  
  #Lets predict for training data
  pred_RF_train = predict(fit_RF, train.data)
  
  #Lets predict for testing data
  pred_RF_test = predict(fit_RF,test.data)
  
  # For training data 
  print(postResample(pred = pred_RF_train, obs = train$Absenteeism.time.in.hours))
  
  # For testing data 
  print(postResample(pred = pred_RF_test, obs = test$Absenteeism.time.in.hours))
  