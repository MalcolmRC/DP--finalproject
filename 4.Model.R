library(sf)
library(tidyverse)
library(readxl)
library(glmnet)
library(caret)


### Read and Prepare data For model ----
PATH <- "C:/Users/nakei/Desktop/UChicago MPP/5th quarter/Data and Programming II/Final Project" 
setwd("C:/Users/nakei/Desktop/UChicago MPP/5th quarter/Data and Programming II/Final Project")

## Read data ----
# shapefile and demographic variables
df_shp <- st_read(file.path(PATH, "manzanas_MEVAL.shp"))[, 1:106] %>%
  st_drop_geometry() 

# crime data
df_crime <- read_xlsx(file.path(PATH, "Data_Manzana_MDE.xlsx"))[, c(4, 37:68)]


## label the variables to make them readable -----
add_labels <- function(df, dict_filename, path = PATH){
  
  # load library
  library(labelled)
  
  # read dictionary
  dict <- read.csv(file.path(PATH, dict_filename))
  colnames(dict) <- c('colname','label')
  # create named list
  nlist <- dict$label
  
  names(nlist) <- dict$colname
  if ('geometry' %in% colnames(df)) {
    nlist <- c(nlist, geometry = 'Geometry')
  }
  
  # label variables
  var_label(df) <- nlist
  
  return(df)
}
df_shp <- add_labels(df_shp, "shapefile_labels.csv") %>%
  select(1, 20, 23:106)
df_crime <- add_labels(df_crime,'crime_labels.csv')
 
# get sum of crimes and merge -----
df_crime <- df_crime %>%
  mutate(crimes_sum = select(., hom2012:hmot2019) %>% rowSums(na.rm = TRUE))
colnames(df_crime)[1] <- colnames(df_shp)[1]
df_full <- right_join(df_shp, df_crime[, c(1,34)], by = "COD_DANE_A") %>%
  select(-1)

# explore character variables

df_char <- df_full %>% 
  select(where(is.character))

str(df_char)

df_full <- df_full %>% 
  select(-TP_LC_CM) # drop description of locality as it takes on a single value "comuna" 


# Log crimes_sum to obtain a more normal distribution
df_model <- df_full %>% 
  mutate(log_crimes = log(crimes_sum + 1)) %>% 
  select(-crimes_sum) %>% 
  select(log_crimes, everything())


# Drop columns that only have zeros and variable with communa name
df_model <- df_model %>% 
  select_if(~ is.character(.) | max(.) > 0) %>% # code adapted from https://stackoverflow.com/questions/53078088/select-or-subset-variables-whose-column-sums-are-not-zero
  select(-NMB_LC_CM) %>% 
  mutate(CD_LC_CM = as.factor(CD_LC_CM))



### Model ----

# Run full regression ----
reg_full <- glm(log_crimes ~ ., 
                 data = df_model, 
                 family = "gaussian")
summary(reg_full)

# The model is overfit and many coefficients are NA due to colinearity between variables.


## Fit regression model using the Lasso ----
df_no_na <- na.omit(df_model) # model.matrix drops NA observations, so we drop NAs in data frames for model fitting

# Turn dataframe into matrix for glmnet fit

matrix_data <- model.matrix(reg_full, df_no_na)
matrix_data <- matrix_data[,-1] # drop intercept

# Fit model using the lasso
cv_lasso <- cv.glmnet(
  x       = matrix_data, 
  y       = df_no_na$log_crimes,
  family  = "gaussian", 
  alpha   = 1,   
  nfolds  = 10)

# Plot out of sample error as a function of lambda penalization ----

plot(cv_lasso, sign.lambda = -1) 

# x values on the left have larger lambda (higher penalization) and less variables. The vertical lines represent the lambdas that 
# 1. minimize cross validation error, and 2. the largest value of lambda with error within 1 standard error of the minimum.  

# Variables selected using lambda within 1 se of minimum error ----
betas_1se <- coefficients(cv_lasso, s = "lambda.1se")
model_1se <- which(betas_1se[-1] != 0)
(vars_1se <- colnames(matrix_data[, model_1se]))


# Variables selected by lasso for minimum error ----
betas_min <- coefficients(cv_lasso, s = "lambda.min")
model_min <- which(betas_min[-1] != 0)
(vars_min <- colnames(matrix_data[, model_min]))


## Predict crimes per quadrant ----

# split data into train and test to predict crimes per quadrant ----
set.seed(4) 

training <- sample(1:nrow(df_no_na), 0.8*nrow(df_no_na))

matrix_train <- matrix_data[training,]
matrix_train <- matrix_train[,-1] # remove intercept
matrix_test <- matrix_data[-training,]
matrix_test <- matrix_test[,-1]

df_train <- df_no_na[training,]
df_test <- df_no_na[-training,]


# Train a model to predict crime per quadrant ----
lasso_train <- cv.glmnet(
  x       = matrix_train, 
  y       = df_train$log_crimes,
  family  = "gaussian", 
  alpha   = 1,   
  nfolds  = 10)


train_fit <- lasso_train$glmnet.fit

# Predictions for lambda with minimum error ----
pred_test_min <- predict(train_fit,
                         newx = matrix_test,
                         s = lasso_train$lambda.min)


# Compare prediction to real value ----

df_min <- tibble(prediction = pred_test_min[,1], actual = df_test$log_crimes)

#RMSE
sqrt(mean(df_min$actual - df_min$prediction)^2)



# Predictions for largest lambda within 1 se of minimum error
pred_test_1se <- predict(train_fit,
                     newx = matrix_test,
                     s = lasso_train$lambda.1se)

# Compare prediction to real value ----
df_1se <- tibble(prediction = pred_test_1se[,1], actual = df_test$log_crimes)

#RMSE
sqrt(mean(df_1se$actual - df_1se$prediction)^2)


