library(sf)
library(tidyverse)


## Path ------
PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/Data" 

## Read data-----
# shapefile and demographic variables
df_shp <- st_read(file.path(PATH, "manzanas_MEVAL.shp"))[, 1:106] %>%
  st_drop_geometry() 

#crime data
df_crime <- read_xlsx(file.path(PATH, "Data_Manzana_MDE.xlsx"))[, c(4, 37:68)]


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
  select(1, 20, 23:106)
df_crime <- add_labels(df_crime,'crime_labels.csv')

#get sum of crimes and merge -----
df_crime <- df_crime %>%
  mutate(crimes_sum = select(., hom2012:hmot2019) %>% rowSums(na.rm = TRUE))
colnames(df_crime)[1] <- colnames(df_shp)[1]
df_full <- right_join(df_shp, df_crime[, c(1,34)], by = "COD_DANE_A") %>%
  select(-1)


## Run model -----

reg.simple <- lm(crimes_sum ~ ., data = df_full)
summary(reg.simple)
