# Libraries
# Function to check and install packages
check_and_install <- function(package){
  if(!require(package, character.only = TRUE)){
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# List of required packages
packages <- c("mice", "visdat", "VIM", "ggplot2", "dplyr", "caTools", "corrplot", "DT", "rpart.plot", "pROC", "caret", "ggpubr")

# Apply the function to each package
invisible(sapply(packages, check_and_install))


###### Acquisition of the data set ######
#########################################
# Set the URL of the data set
url = "http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"

# Set the file path
file_path = "credit_approval.csv"

# Download and save the data set
download.file(url, destfile = file_path, method = "curl")

# Read the CSV file into a Data Frame
credit_data = read.csv(file_path, header = FALSE, na.strings = "?")


###### Exploration and data set transformation ######
####################################################
# Preview the first few rows of the data set
head(credit_data)
datatable(credit_data)
str(credit_data)

### Variable Names
# Change the name of features/Attributes of the data set in UCI Website: http://rstudio-pubs-static.s3.amazonaws.com/73039_9946de135c0a49daa7a0a9eda4a67a72.html#executive-summary
colnames(credit_data) = cols = c("Gender", "Age", "Debt", "MaritalStatus", 
                            "BankCustomer", "EducationLevel", "Ethnicity", "YearsEmployed",
                            "PriorDefault", "Employed", "CreditScore", "DriversLicense", 
                            "Citizen", "ZipCode", "Income", "Approved")

# Transform categorical features 'MaritalStatus', 'DriversLicense', and 'Citizen' to factor
credit_data$MaritalStatus <- as.factor(credit_data$MaritalStatus)
credit_data$DriversLicense <- as.factor(credit_data$DriversLicense)
credit_data$Citizen <- as.factor(credit_data$Citizen)
credit_data$Approved <- as.factor(credit_data$Approved)

# Print the unique values for binary features
unique(credit_data$Gender) # Has missing value 
unique(credit_data$PriorDefault)
unique(credit_data$Employed)
unique(credit_data$DriversLicense)
unique(credit_data$MaritalStatus)
unique(credit_data$BankCustomer)
unique(credit_data$EducationLevel)
unique(credit_data$Ethnicity)
unique(credit_data$Citizen)
unique(credit_data$Approved)

# Map 'Gender', 'PriorDefault', 'Employed', and 'Approved' to binary values
credit_data <- credit_data %>%
  mutate(
    Gender = recode(Gender, 'b' = 0, 'a' = 1),
    PriorDefault = recode(PriorDefault, 'f' = 0, 't' = 1),
    Employed = recode(Employed, 'f' = 0, 't' = 1),
    DriversLicense = recode(DriversLicense, 'f' = 0, 't' = 1),
    Approved = recode(Approved, '-' = 0, '+' = 1)
  )


# Print the unique values for binary features to check
unique(credit_data$Gender) # Has missing value 
unique(credit_data$PriorDefault)
unique(credit_data$Employed)
unique(credit_data$DriversLicense)
unique(credit_data$Approved)

# Check the structure of the dataframe after transformations
str(credit_data)
datatable(credit_data)

# Compute correlation matrix for numeric columns
cor_matrix <- credit_data %>% 
  select_if(is.numeric) %>%
  cor(use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Generate corrplot
corrplot(cor_matrix, method = "color", type="upper", order="hclust", 
         tl.col="black", tl.srt=45,addCoef.col = "black", number.cex = 0.7)

### Missing Data Treatment ###

# Summarize missing values by computing the percentage of missing data for each column
miss_summary <- credit_data %>% 
  summarise(across(everything(), ~sum(is.na(.))/length(.) * 100))

print(miss_summary)

# Visualize missing data using vis_miss
vis_miss(credit_data)

# Remove rows with missing values
completed_data <- na.omit(credit_data)

# Check the structure of the completed data and ensure there are no more NAs
str(completed_data)
summary(completed_data)


# Perform a general check for any remaining missing values
total_na <- sum(sapply(completed_data, function(x) sum(is.na(x))))
cat("Total missing values after cleaning: ", total_na, "\n")

# visualize the cleaned data again 
vis_miss(completed_data)

# Convert certain columns back to factors for clarity and preparation for machine learning
completed_data <- completed_data %>%
  mutate(
    Gender = as.factor(Gender),
    BankCustomer = as.factor(BankCustomer),
    EducationLevel = as.factor(EducationLevel),
    Ethnicity = as.factor(Ethnicity),
    PriorDefault = as.factor(PriorDefault),
    Employed = as.factor(Employed),
    DriversLicense = as.factor(DriversLicense),
    Approved = as.factor(Approved)
  )

# Check the structure after transformations
datatable(completed_data)
str(completed_data)

### Data Visualization ###

## Continuous Features ##
cont_vars <- c("YearsEmployed", "CreditScore", "Age", "Debt", "Income")

# Plotting histograms using ggplot2
for(var in cont_vars){
  print(
    ggplot(completed_data, aes_string(x = var)) +
      geom_histogram(aes(fill = ..count..), bins = 30, color = "black") +
      scale_fill_viridis_c() +
      labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
      theme_minimal()
  )
}

# Plotting boxplots using ggplot2
for(var in cont_vars){
  print(
    ggplot(completed_data, aes_string(y = var)) +
      geom_boxplot(aes(fill = var), color = "black") +
      labs(title = paste("Boxplot of", var), x = "", y = var) +
      theme_minimal() +
      coord_flip()
  )
}

# Plotting horizontal boxplots by 'Approved' target feature
for(var in cont_vars){
  print(
    ggplot(completed_data, aes_string(x = "Approved", y = var)) +
      geom_boxplot(aes(fill = Approved), color = "black") +
      labs(title = paste("Boxplot of", var, "by Approved status"), x = "Approved Status", y = var) +
      theme_minimal() +
      coord_flip()
  )
}

## Categorical Features ##
cat_vars <- c("Gender", "MaritalStatus", "ZipCode", "EducationLevel",
              "Ethnicity", "BankCustomer", "DriversLicense", "Citizen",
              "PriorDefault", "Employed")

# Plotting stacked bar plots for 'Approved' by each categorical variable using ggplot2
for(var in cat_vars){
  print(
    ggplot(completed_data, aes_string(x = var, fill = "Approved")) +
      geom_bar(position = "fill") +
      labs(title = paste("Approved by", var), x = var, y = "Proportion") +
      theme_minimal()
  )
}

# Bar plot of "ZipCode"
zip_plot <- ggplot(completed_data, aes(x = ZipCode)) +
  geom_bar(aes(fill = ZipCode), color = "black") +
  labs(title = "Distribution of ZipCode", x = "ZipCode", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

print(zip_plot)

### Dimention reduction
# zipcode has many 0 values with it does not exist 
# in addition, logicly speaking zip code does not have any significance
# Removing the zipcode feature
completed_data <- completed_data %>% select(-ZipCode)

str(completed_data)
datatable((completed_data))

cat_vars <- c("Gender", "MaritalStatus", "ZipCode", "EducationLevel",
              "Ethnicity", "BankCustomer", "DriversLicense", "Citizen",
              "PriorDefault", "Employed")

# Display unique values for each categorical variable
for(v in cat_vars){
  cat("Unique values for", v, ":\n")
  print(unique(completed_data[[v]]))
  cat("\n")
}


# Apply log1p transformation to the specified columns
completed_data$AgeLog <- log1p(completed_data$Age)
completed_data$DebtLog <- log1p(completed_data$Debt)
completed_data$CreditScoreLog <- log1p(completed_data$CreditScore)
completed_data$IncomeLog <- log1p(completed_data$Income)
completed_data$YearsEmployedLog <- log1p(completed_data$YearsEmployed)

# Optionally, you can remove the original columns after transformation
completed_data <- completed_data %>% select(-Age, -Debt, -CreditScore, -Income, -YearsEmployed)

# Display the structure of the transformed dataframe
str(completed_data)

## Continuous Features ##
cont_vars_log <- c("YearsEmployedLog", "CreditScoreLog", "AgeLog", "DebtLog", "IncomeLog")

# Check the Log transformed data
head(completed_data[cont_vars_log])


# Z-score transformation
completed_data[cont_vars_log] <- scale(completed_data[cont_vars_log])

# Check the transformed data
head(completed_data[cont_vars_log])

### Data Visualization after transformation ###
# Generate histograms using ggplot2
hist_plots <- lapply(cont_vars_log, function(var) {
  ggplot(completed_data, aes_string(x = var)) +
    geom_histogram(aes(fill = ..count..), bins = 30, color = "black") +
    scale_fill_viridis_c() +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal()
})

# Generate boxplots using ggplot2
box_plots <- lapply(cont_vars_log, function(var) {
  ggplot(completed_data, aes_string(y = var)) +
    geom_boxplot(aes(fill = var), color = "black") +
    labs(title = paste("Boxplot of", var), x = "", y = var) +
    theme_minimal() +
    coord_flip()
})

# Generate horizontal boxplots by 'Approved' target feature
box_by_approval_plots <- lapply(cont_vars_log, function(var) {
  ggplot(completed_data, aes_string(x = "Approved", y = var)) +
    geom_boxplot(aes(fill = Approved), color = "black") +
    labs(title = paste("Boxplot of", var, "by Approved status"), x = "Approved Status", y = var) +
    theme_minimal() +
    coord_flip()
})

# Display histograms
hist_grid <- ggarrange(plotlist = hist_plots, ncol = 2, nrow = 3)
print(hist_grid)

# Display boxplots
box_grid <- ggarrange(plotlist = box_plots, ncol = 2, nrow = 3)
print(box_grid)

# Display horizontal boxplots by 'Approved' status
box_by_approval_grid <- ggarrange(plotlist = box_by_approval_plots, ncol = 2, nrow = 3)
print(box_by_approval_grid)

# Violin plot for CreditScore by Approved
ggplot(completed_data, aes(x = Approved, y = CreditScoreLog, fill = Approved)) +
  geom_violin() +
  labs(title = "Violin plot of CreditScore by Approved status", x = "Approved Status", y = "CreditScore") +
  theme_minimal()

# Violin plot for Income by Approved
ggplot(completed_data, aes(x = Approved, y = IncomeLog, fill = Approved)) +
  geom_violin() +
  labs(title = "Violin plot of Income by Approved status", x = "Approved Status", y = "Income") +
  theme_minimal()

head(completed_data)

datatable(completed_data)
# Display the structure of the transformed dataframe
str(completed_data)

###### Research Question ######
###############################
# Do Age, Income, Credit Score, and Debt levels relate to credit approval? 
# Can we use this relationship to predict credit approval? 
# And does it show good risk management?

###### Model ######
##################

## Logistic regression
# Set seed for reproducibility
set.seed(123)

# Create an index of training data
train_index <- createDataPartition(completed_data$Approved, p = 0.75, list = FALSE)

# Split the data into training and test sets
train_data <- completed_data[train_index, ]
test_data <- completed_data[-train_index, ]

# Check the dimensions of the training and test sets
dim(train_data)
dim(test_data)

table(train_data$Approved)

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Apply logistic regression using glm
logistic_model <- glm(Approved ~ PriorDefault + CreditScoreLog + YearsEmployedLog + Employed + AgeLog + DebtLog + IncomeLog, 
                      data = train_data, 
                      family = binomial())

# Summarize the model
summary(logistic_model)

# Predicted probabilities on the training set
predicted_probs_train <- predict(logistic_model, newdata = train_data, type = "response")

# Predicted classes based on the threshold of 0.5
predicted_classes_train <- ifelse(predicted_probs_train > 0.5, 1, 0)

# Actual classes
actual_classes_train <- train_data$Approved

# Create a confusion matrix
conf_matrix_train <- confusionMatrix(factor(predicted_classes_train, levels = c(0, 1)), 
                                     factor(actual_classes_train, levels = c(0, 1)))

# Print the confusion matrix
print(conf_matrix_train)

### Revised model
# Apply logistic regression using glm
logistic_model <- glm(Approved ~ PriorDefault + CreditScoreLog + IncomeLog, 
                      data = train_data, 
                      family = binomial())

# Summarize the model
summary(logistic_model)

# Predicted probabilities on the training set
predicted_probs_train <- predict(logistic_model, newdata = train_data, type = "response")

# Predicted classes based on the threshold of 0.5
predicted_classes_train <- ifelse(predicted_probs_train > 0.5, 1, 0)

# Actual classes
actual_classes_train <- train_data$Approved

# Create a confusion matrix
conf_matrix_train <- confusionMatrix(factor(predicted_classes_train, levels = c(0, 1)), 
                                     factor(actual_classes_train, levels = c(0, 1)))

# Print the confusion matrix
print(conf_matrix_train)

# Generate predicted probabilities on the test set
predicted_probs <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions using a threshold (e.g., 0.5)
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Convert the 'Approved' column to a numeric type 
test_data$Approved <- as.numeric(as.character(test_data$Approved))

# Compute the confusion matrix
cm <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$Approved))
print(cm)

# Create the confusion matrix using the table function
cm_table <- table(Predicted = predicted_classes, Actual = test_data$Approved)

# Print the confusion matrix
print(cm_table)

# the confusionMatrix object from the previous step
conf_matrix_train <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$Approved))

# Misclassification error
misclassification_error <- 1 - conf_matrix_train$overall['Accuracy']

# Print the misclassification error
print(misclassification_error*100)

# Generate ROC curve object
roc_obj <- roc(test_data$Approved, predicted_probs)

# Calculate the AUC
roc_auc <- round(auc(roc_obj), 4)

# Print the AUC with a message
cat('Area Under the Curve:', roc_auc, '\n')
# Use ggroc to make a ggplot-compatible object
roc_curve <- ggroc(roc_obj)
roc_curve

###### Model 2 ######
##################
# Fit a CART model
cart_model <- train(Approved ~ ., 
                    data = train_data, 
                    method = "rpart")

# Summarize the model
print(cart_model)

# Predicted classes on the training set
predicted_classes_train <- predict(cart_model, newdata = train_data)

# Actual classes
actual_classes_train <- train_data$Approved

# Create a confusion matrix
conf_matrix_train <- confusionMatrix(predicted_classes_train, actual_classes_train)

# Print the confusion matrix
print(conf_matrix_train)

# Predicted classes on the test set
predicted_classes <- predict(cart_model, newdata = test_data)

# Convert the 'Approved' column to a numeric type if it's not already
test_data$Approved <- as.numeric(as.character(test_data$Approved))

# Compute the confusion matrix
cm <- confusionMatrix(predicted_classes, as.factor(test_data$Approved))
print(cm)


# Extract the rpart model from the caret object
rpart_model <- cart_model$finalModel

# Plot the rpart model
rpart.plot(rpart_model, main="CART Model Visualization", 
           extra=102,  # Display splits with variable names and cutoffs
           under=TRUE,  # Display the number of observations in the node below the node label
           faclen=0)  # Factor length to abbreviate factor levels

#### Reficed model
# Fit a CART model
cart_model <- train(Approved ~ PriorDefault + CreditScoreLog + IncomeLog, 
                    data = train_data, 
                    method = "rpart")

# Summarize the model
print(cart_model)

# Predicted classes on the training set
predicted_classes_train <- predict(cart_model, newdata = train_data)

# Actual classes
actual_classes_train <- train_data$Approved

# Create a confusion matrix
conf_matrix_train <- confusionMatrix(predicted_classes_train, actual_classes_train)

# Print the confusion matrix
print(conf_matrix_train)

# Predicted classes on the test set
predicted_classes <- predict(cart_model, newdata = test_data)

# Convert the 'Approved' column to a numeric type if it's not already
test_data$Approved <- as.numeric(as.character(test_data$Approved))

# Compute the confusion matrix
cm <- confusionMatrix(predicted_classes, as.factor(test_data$Approved))
print(cm)

# Extract the rpart model from the caret object
rpart_model <- cart_model$finalModel

# Plot the rpart model
rpart.plot(rpart_model, main="CART Model Visualization", 
           extra=102,  # Display splits with variable names and cutoffs
           under=TRUE,  # Display the number of observations in the node below the node label
           faclen=0)  # Factor length to abbreviate factor levels

# Those are the features that give the best accuracy
# Approved ~ PriorDefault + CreditScoreLog + YearsEmployedLog + Employed + AgeLog + DebtLog + IncomeLog