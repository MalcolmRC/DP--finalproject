#fITTING A MODEL

## DOWNLOAD RAW DATA AT: https://drive.google.com/drive/u/1/folders/1ZySqbCo1fZRxs9H0lDMnxjlcyE5hNRMy


library(sf)
library(tidyverse)
library(readxl)
library(glmnet)
library(caret)


### Read and Prepare data For model ----
PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/Data"
#PATH <- "C:/Users/nakei/Desktop/UChicago MPP/5th quarter/Data and Programming II/Final Project" 
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

df_full <- df_full %>% 
  select(-TP_LC_CM) # drop description of locality as it takes on a single value "comuna" 


### Model ----

# Run full regression ----
reg_full <- glm(crimes_sum ~ ., 
                 data = df_full, 
                 family = "gaussian")
summary(reg_full)

# The model is overfit and many coefficients are NA due to colinearity between variables.


## Fit regression model using the Lasso ----
df_no_na <- na.omit(df_full) # model.matrix drops NA observations, so we drop NAs in data frames for model fitting

# Turn dataframe into matrix for glmnet fit

matrix_data <- model.matrix(reg_full, df_no_na)
matrix_data <- matrix_data[,-1] # drop intercept

# Fit model using the lasso
cv_lasso <- cv.glmnet(
  x       = matrix_data, 
  y       = df_no_na$crimes_sum,
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

# Prints the variables selected for model with largest lambda within 1 se of minimum error 

# Variables selected by lasso for minimum error ----
betas_min <- coefficients(cv_lasso, s = "lambda.min")
model_min <- which(betas_min[-1] != 0)
(vars_min <- colnames(matrix_data[, model_min]))

# Prints the variables selected for minimum error model 



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
  y       = df_train$crimes_sum,
  family  = "gaussian", 
  alpha   = 1,   
  nfolds  = 10)


train_fit <- lasso_train$glmnet.fit

# Predictions for lambda with minimum error ----
pred_test_min <- predict(train_fit,
                         newx = matrix_test,
                         s = lasso_train$lambda.min)

# Compare prediction to real value ----
df_min <- tibble(prediction = pred_test_min[,1], actual = df_test$crimes_sum)

# RMSE
sqrt((mean(df_min$actual - df_min$prediction)^2))



# Predictions for largest lambda within 1 se of minimum error
pred_test_1se <- predict(train_fit,
                     newx = matrix_test,
                     s = lasso_train$lambda.1se)

# Compare prediction to real value ----
df_1se <- data_frame(prediction = pred_test_1se[,1], actual = df_test$crimes_sum)

# MSE
sqrt((mean(df_1se$actual - df_1se$prediction)^2))


