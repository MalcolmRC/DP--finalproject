library(sf)
library(tidyverse)


## Path ------
PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/Data" 

## Read data-----
df_shp <- st_read(file.path(PATH, "manzanas_MEVAL.shp"))[, 1:106] %>%
  st_drop_geometry() 

## label the variables to make them readable ------
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
  select(20, 23:106)


