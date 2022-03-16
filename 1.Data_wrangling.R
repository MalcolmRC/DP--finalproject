# 1. Data Wrangling -------

## DOWNLOAD RAW DATA AT: https://drive.google.com/drive/u/1/folders/1ZySqbCo1fZRxs9H0lDMnxjlcyE5hNRMy

### Block-level data--------

PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/Data" 

## load libraries####
library(tidyverse)
library(ggplot2)
#install.packages("maps")
#install.packages("mapdata")
library(maps)
library(mapdata)
#install.packages("sf")
library(sf)
library(readr)
library(tidyr)
#install.packages("vtable")
library(vtable)
library(dplyr)
library(haven)
library(readxl)
library(lubridate)
#install.packages("cowplot")
library(cowplot)
#install.packages("expss")
library(expss)
#install.packages("reshape2")
library(reshape2)
#library(plyr)                          ## load this library if things don't run
library(lubridate)
#install.packages("leaflet")
library(leaflet)
library(ggeasy)

options(scipen = 999)

# Read Data ####

## crime data ####
df_crime_full <- read_xlsx(file.path(PATH, "Data_Manzana_MDE.xlsx"))
df_crime <- read_xlsx(file.path(PATH, "Data_Manzana_MDE.xlsx"))[, c(4, 37:68)]

## shape file ####
df_shp <- st_read(file.path(PATH, "manzanas_MEVAL.shp"))[, 1:106]

# Clean Data ####
## add labels ####
# create function
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

# run function
df_shp <- add_labels(df_shp, "shapefile_labels.csv")
df_crime <- add_labels(df_crime,'crime_labels.csv')

## merge data####

# drop useless cols
select_cols <- function(df, dict_filename, path=PATH){
  dict_colname <- read_excel(file.path(path, dict_filename))
  df <- df[, dict_colname$Variable]
}

df_shp <- select_cols(df_shp,'Selected data dictionary.xlsx')

# rename cols
colnames(df_crime)[1] <- colnames(df_shp)[1]

# merge
df_main <- right_join(df_shp, df_crime, by = "COD_DANE_A")

# save df
st_write(df_main, 'C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Data/df_main.shp', append = F, delete_layer = T)
