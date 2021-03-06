# quadrant data -------
## run separately from 1.Data_wrangling, where we load block-level data, due to overlapping libraries. Here we have data at the quadrant-shift level. 

## DOWNLOAD RAW DATA AT: https://drive.google.com/drive/u/1/folders/1ZySqbCo1fZRxs9H0lDMnxjlcyE5hNRMy

options(scipen = 999)

PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/Data"

## load libraries####
library(readxl)
library(sf)
library(ggplot2)
#install.packages("reshape2")
library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyverse)
#install.packages("leaflet")
library(leaflet)
library(ggeasy)



# Data Cleaning ####
## Pre-processing Crime Data ####
clean_crime_data <- function(df_crime){
  df_crime$lat <- str_replace(df_crime$lat, ',' , '.')
  df_crime$lon <- str_replace(df_crime$lon, ',' , '.')
  return(df_crime)
}

read_crime_data <- function(fname, sname, path = PATH){
  df <- read_xlsx(file.path(PATH, fname), sheet = sname)
  df <- clean_crime_data(df) %>%
    mutate(crime_type = sname) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    select(c(crime_type, fecha, hora_24, geometry))  
}


merge_crime_data <- function(fname, path = PATH){
  names <- excel_sheets(file.path(path, fname))[c(2,3,4)]
  df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(df) <- c('crime_type', 'fecha', 'hora_24', 'geometry')
  for (name in names) {
    df_temp <- read_crime_data(fname, name)
    df <- rbind(df, df_temp)
  }
  # rename crime types
  df$crime_type <- case_when(df$crime_type == "Homicidios" ~ "homicide",
                             df$crime_type == "H.Automotores" ~ "car theft",
                             df$crime_type == "H.Personas" ~ "robbery")
  
  
  # turn time into shifts
  df$shift <- ifelse(df$hora_24 %in% c(5,6,7,8,9,10,11,12),'5-13',
                     ifelse(df$hora_24 %in% c(13,14,15,16,17,18,19,20),
                            '13-21','21-5'))
  
  #rename columns into English
  df <- df %>%
    dplyr::rename(date = fecha,
                  hour = hora_24)
  
  #get years
  df <- df %>%
    mutate(year = year(date),
           month = month(date))
  
  return(df)
}
df_crime_yrs <- merge_crime_data('Crimes_MDE_V3.xlsx')

# create subsets of crime df by year ----
df_crime_18 <- df_crime_yrs %>%
  filter(year == 2018)
df_crime_19 <- df_crime_yrs %>%
  filter(year == 2019)
df_crime_20 <- df_crime_yrs %>%
  filter(year == 2020)
df_crime_21 <- df_crime_yrs %>%
  filter(year == 2021)
df_crime_22 <- df_crime_yrs %>%
  filter(year == 2022)

## Pre-processing Shapefile ####

clean_shp <- function(df_shp){
  df_shp <- df_shp[!substr(as.character(df_shp$NRO_CUADRA), 13, 13) %in% c('6', '7'),]
  df_shp <- df_shp[is.na(df_shp$SUBESTACIO),]
  df_shp <- df_shp[which(df_shp$ESTACION!='SAN ANTONIO DE PRADO'),]
  df_shp <- df_shp[-which(df_shp$NRO_CUADRA %in% c('MEVALMNVCCD03E03C03000008',
                                                   'MEVALMNVCCD04E02C03000028')),]
  # df_shp <- df_shp[which(df_shp$CAI!='CAI LAS PALMAS'),]
  colnames(df_shp)[which(names(df_shp) == "NRO_CUADRA")] <- 'region'
  return(df_shp)
}

p2p <- function(df_crime, df_shp){
  pnts <- data.frame('x' = unlist(map(df_crime$geometry, 1)),
                     'y' = unlist(map(df_crime$geometry, 2)))
  
  # create a points collection
  pnts_sf <- do.call("st_sfc", c(lapply(1:nrow(pnts), 
                                        function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 
  
  # apply transformation to pnts sf
  pnts_trans <- st_transform(pnts_sf, 2163) 
  tt1_trans <- st_transform(df_shp, 2163)
  
  
  pnts$region <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                       function(col) { 
                         tt1_trans[which(col), ]$region
                       })
  
  df_crime$region <- as.character(pnts$region)
  return(df_crime)
}

df_shp_raw <- st_read(file.path(PATH, '07_Cuadrantes'))
df_shp_stations <- df_shp_raw %>%
  st_drop_geometry() %>%
  select(NRO_CUADRA, ESTACION)

df_shp <- clean_shp(st_read(file.path(PATH, '07_Cuadrantes')))
df_geoms <- df_shp %>%
  select(region, geometry)

df_crime_18 <- p2p(df_crime_18, df_shp)
df_crime_19 <- p2p(df_crime_19, df_shp)
df_crime_20 <- p2p(df_crime_20, df_shp)
df_crime_21 <- p2p(df_crime_21, df_shp)

change_to_shift <- function(df_crime, shp = df_shp){
  df_quad <- data.frame(region = rep(df_shp$region, 3),
                        shift = rep(c("21-5", "5-13", "13-21"), each = nrow(df_shp)))
  df_temp <- df_crime %>% 
    group_by(region, shift) %>% 
    dplyr::summarise(homicide = sum(crime_type == 'homicide'),
                     theft = sum(crime_type == 'robbery'),
                     vehicle_theft= sum(crime_type == 'car theft'),
                     sum = n())
  df_quad <- merge(df_quad, df_temp, by = c('region','shift'), all.x = T)
  df_quad[is.na(df_quad)] <- 0
  df_quad <- df_quad[,-7]
  df_quad <- merge(df_quad, shp[, c(2, 13)], all = T)
  df_quad <- df_quad %>%
    left_join(df_shp_stations, by = c("region" = "NRO_CUADRA"))
  df_quad <- st_as_sf(df_quad)
  return(df_quad)
}

### create dfs for yearly crime per quad_shift -----
df_shift_18 <- change_to_shift(df_crime_18)
df_shift_19 <- change_to_shift(df_crime_19)
df_shift_20 <- change_to_shift(df_crime_20)
df_shift_21 <- change_to_shift(df_crime_21)

df_shifts <- rbind(df_shift_18, df_shift_19, df_shift_20, df_shift_21)

df_shifts_avg <- df_shifts %>%
  st_drop_geometry() %>%
  group_by(region, ESTACION, shift) %>%
  dplyr::summarise(homicide = mean(homicide),
                   theft = mean(theft),
                   vehicle_theft = mean(vehicle_theft),
                   sum = mean(sum)) %>%
  left_join(df_geoms, by = "region") %>%
  st_as_sf()

# Redistribution optimal formula --------
redistribute <- function(df, colname){
  df$temp <- (nrow(df) * st_drop_geometry(df)[,colname] / sum(st_drop_geometry(df)[,colname])) + 1
  df$temp <- round(df$temp)
  return(df$temp)
}
# crimes per police calculation
crime_per_police <- function(df, crime_type, n_of_police = ''){
  df <- st_drop_geometry(df)
  if (n_of_police == '') {
    df$temp <- df[,crime_type] / 2
  }
  else {
    df$temp <- df[,crime_type] / df[,n_of_police]
  }
  return(df$temp)
}


# number of police in the quad shift
df_shifts_avg$n_of_police <- 2
df_shifts_avg$rn_of_police <- as.numeric(unlist(redistribute(df_shifts_avg, "sum")))


# number of crimes per police
df_shifts_avg$cpp <- as.numeric(unlist(crime_per_police(df_shifts_avg, 'sum')))
df_shifts_avg$rcpp <- as.numeric(unlist(crime_per_police(df_shifts_avg, 'sum', 'rn_of_police')))

df_shifts_avg <- df_shifts_avg %>%
  dplyr::rename(station = ESTACION)

#save df into folder outside of repo due to its size
st_write(df_shifts_avg, 'C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Data/df_shifts_avg.shp', append = F, delete_layer = T)
