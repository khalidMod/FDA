# Clear Memory
rm(list=ls())
# Set current file as working directly by defualt
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
#-------------------------------------------------------------------|
#                              Part 1                               |
#-------------------------------------------------------------------|
# read dataset
df <- read.csv("Assignment3-TrainingData.csv",header=T, 
               stringsAsFactors = TRUE,  na.strings=c(""," ","NA"))
# remove row id and GIS_LAST_MOD_DTTM is constant
df <- df[,-c(1,37)]

# Covert selected columns to factors (Nominal) and date type
# (Nominal variables are data whose levels are labels or descriptions)
# They are also called "nominal categorical" or "qualitative" variables, 
# and the levels of a variable are sometimes called "classes" or "groups".
df$HEAT_D <- as.numeric(df$HEAT_D)
df$AC <- as.numeric(df$AC)
df$QUALIFIED <- as.factor(df$QUALIFIED)
df$GRADE_D <- as.numeric(df$GRADE_D)
df$CNDTN_D <- as.numeric(df$CNDTN_D)
df$EXTWALL_D <- as.numeric(df$EXTWALL_D)
df$INTWALL_D <- as.numeric(df$INTWALL_D)  SORRY
df$STYLE_D <- as.numeric(df$STYLE_D)
df$ROOF_D <- as.numeric(df$ROOF_D)
df$STRUCT_D <- as.numeric(df$STRUCT_D)
df$SALEDATE <- as.Date(df$SALEDATE)

# Visualize missing values in dataset (training and validation)
library(VIM)
aggr(df, numbers = TRUE, prop = c(TRUE, FALSE))

# Find out variable, types
str(df)


# Summary statistics
summary(df)
# Further variance, sd can be computed using var(), sd(), range()
# We will drop column YR_RMDL as it has more then 50% NAs
df <- df[,-10]

df <-df[c(13,1:12,14:35)]

# For this we need to delete NAs rows first, then compute Train dataset is the rows
# which does not have NAs
# The price column has arounf 13000+ missing values.
# Rather then deleting we are going to predict these values to complete our dataframe
train <- na.omit(df)

# Test dataset is the rows which have NAs
test <- df[!complete.cases(df),-1]
test <- na.omit(test)

# train lm model
model <- lm(PRICE ~., data = train)

# predict
pred <-  predict(model, test)

# put predicted values
test$PRICE <- pred

# Reorder columns
# Merge train and test to have original datatset without NAs
df <- as.data.frame(rbind(train, test))

library(caTools)
sample = sample.split(df$QUALIFIED, SplitRatio = .70)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)

#--------------------------------------------------------------------|
#             Train Deep Learning Model for Prediction               |
#--------------------------------------------------------------------|
# Compute feature importance using PCA
library(h2o)
h2o.init(nthreads=2, max_mem_size="4G")
h2o.removeAll()

# convert to h2o data
dat1<-as.h2o(train)
dat2<-as.h2o(test)

# model <- h2o.deeplearning(x = 1:34, y = 35, training_frame = dat1, activation = "Tanh",
#                           epochs = 100,hidden = c(128,64), stopping_metric = "AUC", 
#                           model_id = "MyModel", variable_importances = TRUE,
#                           missing_values_handling = "MeanImputation")

# Call saved model for plotting do not create new
model <- h2o.loadModel(path = "MyModel")
# View Feature importance
dfvarimp <- h2o.varimp(model)
dfvarimp$percentage <- dfvarimp$percentage * 100
# Now can remove variable with low importance
# is less them 1% using dfvarimp dataframe

# Model performance
perf <- h2o.performance(model, newdata = dat2)

# Extract info for ROC curve
curve_dat <- data.frame(perf@metrics$thresholds_and_metric_scores) %>%
  select(c(tpr, fpr))

# Plot ROC curve
ggplot(curve_dat, aes(x = fpr, y = tpr)) +
  geom_point() +
  geom_line() +
  geom_segment(
    aes(x = 0, y = 0, xend = 1, yend = 1),
    linetype = "dotted",
    color = "grey50"
  ) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  ggtitle("ROC Curve") +
  theme_bw()


# Predict
pred <- as.data.frame(predict(model, dat2))

library(caret)
# Make confusion matrix to see results
conf <- confusionMatrix(pred$predict, test$QUALIFIED)
conf
# Plot Confusion Matrix
fourfoldplot(conf$table)

# Save model (we already saved it)
#h2o.saveModel(object = model, path = ".", force=TRUE)
# Shut down H2o Cluster
h2o.shutdown(prompt = FALSE)

#-------------------------------------------------------------------|
#                             Predict                               |
#-------------------------------------------------------------------|
# Clear Memory
rm(list=ls())
# Set current file as working directly by defualt
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Switch off Scientific Notation
options(scipen=999)
library(ggplot2)
library(h2o)
h2o.init(nthreads=1, max_mem_size="4G")
h2o.removeAll()

# read testing data
test <- read.csv("Assignment3-UnknownData.csv",header=T, stringsAsFactors = TRUE,  na.strings=c(""," ","NA"))
summary(test)
# its testing data, we simply remove NAs rows
rowid <- test$row.ID
test <- test[,-c(1,11,37)]
# reorder
test <-test[c(13,1:12,14:34)]
#test <- na.omit(test)

test$HEAT_D <- as.numeric(test$HEAT_D)
test$AC <- as.numeric(test$AC)
test$GRADE_D <- as.numeric(test$GRADE_D)
test$CNDTN_D <- as.numeric(test$CNDTN_D)
test$EXTWALL_D <- as.numeric(test$EXTWALL_D)
test$INTWALL_D <- as.numeric(test$INTWALL_D)
test$STYLE_D <- as.numeric(test$STYLE_D)
test$ROOF_D <- as.numeric(test$ROOF_D)
test$STRUCT_D <- as.numeric(test$STRUCT_D)
test$SALEDATE <- as.Date(test$SALEDATE)

dat3 <- as.h2o(test)

#load model
# save h2o model
model <- h2o.loadModel(path = "MyModel")

# Predict
pred <- as.data.frame(predict(model, dat3))
pred$rowid <- rowid
pred <- pred[c(4,1)]
colnames(pred) <- c("RowID", "Predict-Qualified")

#saving the results
write.csv(pred, "results.csv", row.names = FALSE)








