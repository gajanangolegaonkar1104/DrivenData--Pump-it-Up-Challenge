#FINAL SOURCE CODE - RANDOM FORESTS

#h2o library h2o.randomForest funtion
library(h2o) 

#used for innner join function
library(dplyr)

#read data from respective dataset files
data_training.lb<-read.csv("C:/Users/gajanan/Documents/GitHub/ML-Project/data/train_lables.csv",
                           na.strings = c("na","","NA","unknown","Unknown"));
data_training.val<-read.csv("C:/Users/gajanan/Documents/GitHub/ML-Project/data/train_value.csv",
                            na.strings = c("na","","NA","unknown","Unknown"))
data_testing.val<-read.csv("C:/Users/gajanan/Documents/GitHub/ML-Project/data/test_values.csv",
                           na.strings = c("na","","NA","unknown","Unknown"))

data_training.val<-data_training.val[sample(nrow(data_training.val)),]

#sampling over training data
data_train_indexes <- sample(1:nrow(data_training.val), 0.1 * nrow(data_training.val));

#join train values and train lables using common column ID
data_training = inner_join(data_training.lb, data_training.val, by = "id")

#remove NA values from data frame
data_training <- na.omit(data_training)

#marking 0 and invalid values as NA
for(i in 1:nrow(data_training)) {
  if(!is.na(data_training$funder[[i]][1]))
  {
    if(data_training$funder[[i]][1] == 0)
      data_training$funder[[i]][1] = NA
    
  }
  
  if(!is.na(data_training$installer[[i]][1]))
  {
    if(data_training$installer[[i]][1] == 0)
      data_training$installer[[i]][1] = NA
    
  }
  
  if(!is.na(data_training$wpt_name[[i]][1]))
  {
    if(data_training$wpt_name[[i]][1] == 0)
      data_training$wpt_name[[i]][1] = NA
    
  }
  
  
  if(!is.na(data_training$population[[i]][1]))
  {
    if(data_training$population[[i]][1] == 0)
      data_training$population[[i]][1] = NA
    
  }
  
  if(!is.na(data_training$scheme_management[[i]][1]))
  {
    if(data_training$scheme_management[[i]][1] == 0)
      data_training$scheme_management[[i]][1] = NA
    
  }
  
  if(!is.na(data_training$amount_tsh[[i]][1]))
  {
    if(data_training$amount_tsh[[i]][1] == 0)
      data_training$amount_tsh[[i]][1] = NA
    
  }
  
  if(!is.na(data_training$scheme_name[[i]][1]))
  {
    if(data_training$scheme_name[[i]][1] == 0)
      data_training$scheme_name[[i]][1] = NA
  }
  
  if(!is.na(data_training$construction_year[[i]][1]))
  {
    if(data_training$construction_year[[i]][1] == 0)
      data_training$construction_year[[i]][1] = NA
  }
  
  if(!is.na(data_training$latitude[[i]][1]))
  {
    if(data_training$latitude[[i]][1] > -1e-06)
      data_training$latitude[[i]][1] = NA
  }
  
  if(!is.na(data_training$longitude [[i]][1]))
  {
    if(data_training$longitude [[i]][1] < 1e-06)
      data_training$longitude [[i]][1] = NA
  }
  
}

data_training <- na.omit(data_training)

#remove recorded by column from data frame as it is not related
#due to same value for each instance
data_training <- data_training[,-21]

#remove scheme name due to multiple distinct values
data_training <- data_training[,-22]

#payment and payment_type are having same values
#remove one column from them, payment removed

data_training <- data_training[,-29]

#remove feature quantity group as quantity and quantity group have same values
#keep one and remove other
data_training <- data_training[,-33]

#waterpoint_type and wwaterpoint_type_group, have same values, 
#remove waterpoint_type_group
data_training <- data_training[,-37]

#sampling over training data
data_train_indexes <- sample(1:nrow(data_training), 0.95 * nrow(data_training));

train = data_training[data_train_indexes,]
test = data_training[-data_train_indexes,]

localH2O = h2o.init()

predict_columns = c("id","amount_tsh","date_recorded","funder","gps_height","installer",
                    "longitude","latitude","wpt_name","num_private","basin","subvillage"
                    ,"region","region_code","district_code","lga","ward","population",
                    "public_meeting","scheme_management","permit",
                    "construction_year","extraction_type","extraction_type_group",
                    "extraction_type_class","management","management_group","payment_type","water_quality","quality_group","quantity"
                    ,"source","source_type","source_class","waterpoint_type")

predicted_column = "status_group"

train_h2o = as.h2o(train,destination_frame = "train_h2o.hex")
test_h2o = as.h2o(test,destination_frame = "test_h2o.hex")

random_forest_model = h2o.randomForest(
  x = predict_columns,
  y = predicted_column,
  training_frame = train_h2o,
  mtries = 8,
  seed = 12345,
  ntrees = 1200
) 

h2o.confusionMatrix(random_forest_model)

predictions = as.data.frame(h2o.predict(random_forest_model,test_h2o))[,1]

test_h2o1 = as.h2o(data_testing.val,destination_frame = "test_h2o1.hex")

predictions1 = as.data.frame(h2o.predict(random_forest_model,test_h2o1))[,1]

library(dplyr)
library(data.table) 

final_submission = tbl_df(fread("C:/Users/nileshpharate/Documents/GitHub/ML-Project/data/SubmissionFormat.csv")) %>%
  mutate(status_group = predictions1)

write.csv(final_submission,row.names = FALSE,quote = FALSE,
          file = "C:/Users/nileshpharate/Documents/GitHub/ML-Project/data/submission_final.csv")


#ROC metric
library("pROC")
multiclass.roc(as.numeric(test$status_group), as.numeric(predictions))

mtr <- table(predictions, test$status_group)
sum(diag(mtr))/sum(mtr) * 100

