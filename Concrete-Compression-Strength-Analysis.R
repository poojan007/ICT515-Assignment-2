# Install libraries, if not already installed
if(!require(dplyr)) install.packages("dplyr")
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(corrplot)) install.packages("corrplot")
if(!require(car)) install.packages("car")
if(!require(psych)) install.packages("psych")
if(!require(caret)) install.packages("caret")
if(!require(readxl)) install.packages("readxl")
if(!require(visdat)) install.packages("visdat")
if(!require(caTools)) install.packages("caTools")
if(!require(data.table)) install.packages("data.table")


# Loading all the required packages
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(car)
library(psych)
library(caret)
library(readxl)
library(visdat)
library(caTools)
library(data.table)


###### Acquisition of the data set ######
#########################################
# Set the URL of the data set
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls"

# Set the file path
file_path = "concrete_data.xls"

# Download and save the data set
download.file(url, destfile = file_path, method = "curl", extra = "--insecure")

# Read the xls file into a Data Frame
concrete_data = read_excel(file_path)

# Checking the dimension of the dataset
dim(concrete_data)

# Checking the names of the columns
names(concrete_data)

# Since the column names are long and complicated to read
# We will rename the columns
colnames(concrete_data) <- c(
  "cement",
  "blast_furnace_slag",
  "fly_ash",
  "water",
  "superplasticizers",
  "coarse_aggregate",
  "fine_aggregate",
  "age",
  "mpa"
)

str(concrete_data)

###### Exploratory Data Analysis ######
#######################################
summary(concrete_data)

glimpse(concrete_data)

head(concrete_data)

summary(concrete_data$mpa)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]}


ggplot(data = concrete_data) +
  geom_histogram(mapping = aes(x = mpa), bins = 15, boundary = 0, fill = "gray", col = "black") +
  geom_vline(xintercept = mean(concrete_data$mpa), col = "blue", size = 1) +
  geom_vline(xintercept = median(concrete_data$mpa), col = "red", size = 1) +
  geom_vline(xintercept = getmode(concrete_data$mpa), col = "green", size = 1) +
  annotate("text", label = "Median = 34.45", x = 23, y = 100, col = "red", size = 5) +
  annotate("text", label = "Mode = 33.4", x = 23, y = 125, col = "black", size = 5) +
  annotate("text", label = "Mean = 35.82", x = 45, y = 45, col = "blue", size = 5) +
  ggtitle("Histogram of strength") +
  theme_bw()

summary(concrete_data)

# Calculate the Correlation Matrix
correlation_matrix <- cor(concrete_data)
print(correlation_matrix)

# Visualise the Correlation Matrix with a Heatmap
corrplot(correlation_matrix, method="color", type="upper", order="hclust", 
         tl.col="black", tl.srt=45, addCoef.col = "black")

# Determining the relationship and significance level between each pair of variables
chart.Correlation(concrete_data)

### Checking for missing values
# Summarize missing values
missing_summary <- concrete_data %>% 
  summarise_all(list(~sum(is.na(.))/length(.) * 100))
print(missing_summary)

# Visualize missing data
vis_miss(concrete_data)

# Checking for duplicate rows
num_duplicate_rows <- sum(duplicated(concrete_data))
cat("Number of duplicate rows:", num_duplicate_rows, "\n")

# Checking the distribution of features
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
hist(concrete_data$cement)
hist(concrete_data$blast_furnace_slag)
hist(concrete_data$fly_ash)
hist(concrete_data$water)
hist(concrete_data$superplasticizers)
hist(concrete_data$coarse_aggregate)
hist(concrete_data$fine_aggregate)
hist(concrete_data$age)
hist(concrete_data$mpa)

### Checking for outliers
ggplot(melt(concrete_data), aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Boxplots for Each Feature")

# checking for outliers in age feature
boxplot(concrete_data$age, col = "green", xlab = "Age", ylab = "Value") # seems like just 4 outliers

# exploring further to confirm the finding for the outliers in age
age_outliers <- which(concrete_data$age > 100)
concrete_data[age_outliers, "age"]

# checking for multicollinearity
simple_lm <- lm(mpa ~ ., data = concrete_data)
vif(simple_lm)

###### Feature Engineering ######
#################################
par(mfrow = c(2,2))
hist(concrete_data$age)
hist(concrete_data$superplasticizers)
hist(log(concrete_data$age), col = "red")
hist(log(concrete_data$superplasticizers), col = "red")

# converting age to 'log of age' 
concrete_data$age <- log(concrete_data$age)

# converting superplasticizer to log
concrete_data$superplasticizers <- log(concrete_data$superplasticizers)
concrete_data$superplasticizers <- ifelse(concrete_data$superplasticizers == -Inf, 0, concrete_data$superplasticizers)

head(concrete_data)

###### Data Preparation ######
##############################
## Dimensionality Reduction
# removing the feature fly_ash because 
# it did not show any correlation and significance
concrete_data$fly_ash <- NULL
head(concrete_data)

# Removing duplicate rows
concrete_data <- concrete_data %>%
  distinct()

# Randomizing/Shuffling dataset
set.seed(123)
concrete_rand <- concrete_data[sample(1:nrow(concrete_data)), ]
dim(concrete_rand)

### Splitting dataset
set.seed(123)

split <- sample.split(concrete_rand$mpa, SplitRatio = 0.8)
training_set <- subset(concrete_rand, split == TRUE)
test_set <- subset(concrete_rand, split == FALSE)

# Feature scaling
training_set[-8] <- scale(training_set[-8])
test_set[-8] <- scale(test_set[-8])

###### Modelling ######
#######################
# Set up cross-validation
control <- trainControl(method = "cv", number = 10)  # 10-fold CV

### Modelling 1 ###
###################
# 1. Linear Regression
lm_model <- train(mpa ~ ., data = training_set, method = "lm", trControl = control)
summary(lm_model)
print(lm_model)

# 2. Support Vector Regressor (SVR)
svr_model <- train(mpa ~ ., data = training_set, method = "svmRadial", trControl = control)
print(svr_model)

# 3. XGB Linear
xgblinear_model <- train(mpa ~ ., data = training_set, method = "xgbLinear", trControl = control)
print(xgblinear_model)

model_list <- list(lm_model = lm_model, svr_model = svr_model, xgblinear_model = xgblinear_model)

model_results <- data.frame(LM = min(model_list$lm_model$results$RMSE),
                            SVM = min(model_list$svr_model$results$RMSE),
                            XGBL = min(model_list$xgblinear_model$results$RMSE))


print(model_results)

resamples <- resamples(model_list)
dotplot(resamples, metric = "RMSE")

modelCor(resamples)

## Model Evaluation
pred_lm <- predict.train(model_list$lm_model, newdata = test_set)
pred_svm <- predict.train(model_list$svr_model, newdata = test_set)
pred_xgbL <- predict.train(model_list$xgblinear_model, newdata = test_set)


pred_RMSE <- data.frame(LM = RMSE(pred_lm, test_set$mpa),
                        SVR = RMSE(pred_svm, test_set$mpa),
                        XGBL = RMSE(pred_xgbL, test_set$mpa))

print(pred_RMSE)

plot(varImp(xgblinear_model))
plot(varImp(lm_model))

## Prediction Correlation
pred_cor <- data.frame(LM = cor(pred_lm, test_set$mpa),
                       SVR = cor(pred_svm, test_set$mpa),
                       XGBL = cor(pred_xgbL, test_set$mpa))

print(pred_cor)

### Modelling 2 ###
###################
# 1. Linear Regression
lm_model <- train(mpa ~ . - coarse_aggregate, data = training_set, method = "lm", trControl = control)
summary(lm_model)
print(lm_model)

# 2. Support Vector Regressor (SVR)
svr_model <- train(mpa ~ . - coarse_aggregate, data = training_set, method = "svmRadial", trControl = control)
print(svr_model)

# 3. XGB Linear
xgblinear_model <- train(mpa ~ . - coarse_aggregate, data = training_set, method = "xgbLinear", trControl = control)
print(xgblinear_model)

model_list <- list(lm_model = lm_model, svr_model = svr_model, xgblinear_model = xgblinear_model)

model_results <- data.frame(LM = min(model_list$lm_model$results$RMSE),
                            SVM = min(model_list$svr_model$results$RMSE),
                            XGBL = min(model_list$xgblinear_model$results$RMSE))


print(model_results)

resamples <- resamples(model_list)
dotplot(resamples, metric = "RMSE")

modelCor(resamples)

## Model Evaluation
pred_lm <- predict.train(model_list$lm_model, newdata = test_set)
pred_svm <- predict.train(model_list$svr_model, newdata = test_set)
pred_xgbL <- predict.train(model_list$xgblinear_model, newdata = test_set)


pred_RMSE <- data.frame(LM = RMSE(pred_lm, test_set$mpa),
                        SVR = RMSE(pred_svm, test_set$mpa),
                        XGBL = RMSE(pred_xgbL, test_set$mpa))

print(pred_RMSE)

plot(varImp(xgblinear_model))
plot(varImp(lm_model))

## Prediction Correlation
pred_cor <- data.frame(LM = cor(pred_lm, test_set$mpa),
                       SVR = cor(pred_svm, test_set$mpa),
                       XGBL = cor(pred_xgbL, test_set$mpa))

print(pred_cor)
