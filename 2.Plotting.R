# Plotting -----

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

PATH <- "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/Data" 
df_shift <- st_read(file.path(PATH, "df_shifts_avg.shp")) 

plot_cpp <- function(df, cpp, rcpp){
  # prepare df for plotting
  df <- st_drop_geometry(df)
  df_temp <- as.data.frame(df[,c('region', 'shift', cpp, rcpp)])
  df_long <- melt(df, id.vars = c("region", "shift"),
                  measure.vars = c(cpp, rcpp),
                  variable.name = "distribution",
                  value.name = "crime_per_police")
  levels(df_long$distribution) <- c('before','after')
  group_mean <- ddply(df_long, "distribution", summarise, 
                      grp.mean=mean(crime_per_police, na.rm = T))
  
  
  # plot
  p <- ggplot(df_long, aes(x = crime_per_police, color = distribution, fill = distribution)) +
    geom_histogram(aes(y=..density..), alpha = 0.05, position = "identity", binwidth = 1)+
    geom_vline(data = group_mean, aes(xintercept = grp.mean, color = distribution),
               size = 1.25)+
    geom_density(alpha = .2)+
    ggtitle('Crime per officer: before and after redistribution') +
    xlab(label = "Crimes per officer by quadrant-shift") +
    theme(plot.title = element_text(hjust = 0.5, size = 15)) +
    ylim(0, 0.07)
  #print(p)
  #return(p)
}

## Optimal reallocation - proportional -----------
p_cpp <- plot_cpp(df_shift, 'cpp', 'rcpp') +
  labs(subtitle = "Proportional redistribution according to crime per quadrant") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 10))
p_cpp
ggsave(filename = "hist_crimes_p_officer.png",
       plot = p_cpp,
       path = PATH)


## Different re-allocation strategies: Quintiles ----------

# Send one officer from lowest 20% to highest 20% quad-shifts
quantile(df_shift$cpp, probs = seq(.1, .9, by = .1))
df_shift$cpp_rank <- case_when(df_shift$cpp < 3.875 ~ 1,
                               df_shift$cpp >= 3.875 & df_shift$cpp < 8.750 ~ 2,
                               df_shift$cpp >= 8.750 & df_shift$cpp < 14.775 ~ 3,
                               df_shift$cpp >= 14.775 & df_shift$cpp < 24.700 ~ 4,
                               df_shift$cpp > 24.700 ~ 5)
df_shift$rn_police_quintile <- case_when(df_shift$cpp_rank == 1 ~ df_shift$n_f_plc - 1,
                                         df_shift$cpp_rank > 1 & df_shift$cpp_rank < 5  ~ df_shift$n_f_plc,
                                         df_shift$cpp_rank == 5 ~ df_shift$n_f_plc + 1)
df_shift$rcpp_quintile <- crime_per_police(df_shift, 'sum', 'rn_police_quintile')
p_cpp_quintile <- plot_cpp(df_shift, 'cpp', 'rcpp_quintile') +
  labs(subtitle = "One officer from 20% lowest crime quadrants to 20% highest crime quadrants") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 10))
p_cpp_quintile
ggsave(filename = "p_cpp_quintile.png",
       plot = p_cpp_quintile,
       path = PATH)

# Send one officer from lowest 40% to highest 40% quad-shifts
df_shift$rn_police_2quintile <- case_when(df_shift$cpp_rank < 3 ~ df_shift$n_f_plc - 1,
                                          df_shift$cpp_rank == 3  ~ df_shift$n_f_plc,
                                          df_shift$cpp_rank > 3 ~ df_shift$n_f_plc + 1)
df_shift$rcpp_2quintile <- crime_per_police(df_shift, 'sum', 'rn_police_2quintile')
p_cpp_2quintile <- plot_cpp(df_shift, 'cpp', 'rcpp_2quintile') +
  labs(subtitle = "One officer from 40% lowest crime quadrants to 40% highest crime quadrants") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 10))
p_cpp_2quintile
ggsave(filename = "p_cpp_2quintile.png",
       plot = p_cpp_2quintile,
       path = PATH)

# Send one officer from bottom 40% to top 20%
df_shift$rn_police_3quintile <- case_when(df_shift$cpp_rank < 3 ~ df_shift$n_f_plc - 1,
                                          df_shift$cpp_rank == 3 | df_shift$cpp_rank == 4  ~ df_shift$n_f_plc,
                                          df_shift$cpp_rank > 4 ~ df_shift$n_f_plc + 2)
df_shift$rcpp_3quintile <- crime_per_police(df_shift, 'sum', 'rn_police_3quintile')
p_cpp_3quintile <- plot_cpp(df_shift, 'cpp', 'rcpp_3quintile') +
  labs(subtitle = "One officer from 40% lowest crime quadrants to 20% highest crime quadrants") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 10))
p_cpp_3quintile
ggsave(filename = "p_cpp_3quintile.png",
       plot = p_cpp_3quintile,
       path = PATH)


## Maps ----------------------------------------------------------------------------
map_simple <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = sum)) +
  labs(fill = "Crimes",
       color = "Crimes",
       title = "Crimes per quadrant in Medellin",
       subtitle = "Average over 2018-2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 170)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 170)) 
ggsave(filename = "map_simple.png",
       plot = map_simple,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")


map_crimes_quad_2019_morn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = sum)) +
  labs(title = "Morning shift (5:00 - 13:00)",
       fill = "Crimes",
       color = "Crimes") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 170)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 170)) 
map_crimes_quad_2019_morn
ggsave(filename = "map_crimes_quad_2019_morn.png",
       plot = map_crimes_quad_2019_morn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")

map_crimes_quad_2019_aftn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='13-21',],
          aes(fill = sum)) +
  labs(title = "Afternoon shift (13:00 - 21:00)",
       fill = "Crimes",
       color = "Crimes") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 170)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 170)) 
map_crimes_quad_2019_aftn
ggsave(filename = "map_crimes_quad_2019_aftn.png",
       plot = map_crimes_quad_2019_aftn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")

map_crimes_quad_2019_nght <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          aes(fill = sum)) +
  labs(title = "Night shift (21:00 - 5:00)",
       fill = "Crimes",
       color = "Crimes") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 170)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 170)) 
map_crimes_quad_2019_nght
ggsave(filename = "map_crimes_quad_2019_nght.png",
       plot = map_crimes_quad_2019_nght,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")
plot_grid(map_crimes_quad_2019_morn, map_crimes_quad_2019_aftn, map_crimes_quad_2019_nght,
          labels = c("Morning Shift", "Afternoon Shift", "Night Shift"),
          ncol = 3)


## Maps of crimes per officer by quadrant --------------

## Status quo
map_crimes_officer_quad_2019_morn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = cpp)) +
  labs(title = "Morning shift (5:00 - 13:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
map_crimes_officer_quad_2019_morn
ggsave(filename = "map_crimes_officer_quad_2019_morn.png",
       plot = map_crimes_officer_quad_2019_morn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")
map_crimes_officer_quad_2019_aftn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='13-21',],
          aes(fill = cpp)) +
  labs(title = "Afternoon shift (13:00 - 21:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
map_crimes_officer_quad_2019_aftn
ggsave(filename = "map_crimes_officer_quad_2019_aftn.png",
       plot = map_crimes_officer_quad_2019_aftn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")
map_crimes_officer_quad_2019_nght <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          aes(fill = cpp)) +
  labs(title = "Night shift (21:00 - 5:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
map_crimes_officer_quad_2019_nght
ggsave(filename = "map_crimes_officer_quad_2019_nght.png",
       plot = map_crimes_officer_quad_2019_nght,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")

## Redistributed
map_redis_crimes_officer_quad_2019_morn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = rcpp)) +
  labs(title = "Morning shift (5:00 - 13:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
map_redis_crimes_officer_quad_2019_morn
ggsave(filename = "map_redis_crimes_officer_quad_2019_morn.png",
       plot = map_redis_crimes_officer_quad_2019_morn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")
map_redis_crimes_officer_quad_2019_aftn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='13-21',],
          aes(fill = rcpp)) +
  labs(title = "Afternoon shift (13:00 - 21:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
map_redis_crimes_officer_quad_2019_aftn
ggsave(filename = "map_redis_crimes_officer_quad_2019_aftn.png",
       plot = map_redis_crimes_officer_quad_2019_aftn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")
map_redis_crimes_officer_quad_2019_nght <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          aes(fill = rcpp)) +
  labs(title = "Night shift (21:00 - 5:00)",
       fill = "Crimes per Officer",
       color = "Crimes per Officer") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
map_redis_crimes_officer_quad_2019_nght
ggsave(filename = "map_redis_crimes_officer_quad_2019_nght.png",
       plot = map_redis_crimes_officer_quad_2019_nght,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")


### Officers per quadrant -----------------

## status quo
map_officers_quad_2019 <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          #aes(fill = n_of_police)) +
          aes(fill = n_f_plc)) +
  labs(title = "Officers per Quadrant",
       subtitle = "Uniform distribution - 2 per quadrant",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8))
map_officers_quad_2019
ggsave(filename = "map_officers_quad_2019.png",
       plot = map_officers_quad_2019,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")

### redistribution
map_redis_officers_quad_2019_morn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          #aes(fill = rn_of_police)) +
          aes(fill = rn_f_pl)) +
  labs(title = "Officers per Quadrant - Redistribution",
       subtitle = "Morning Shift",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
map_redis_officers_quad_2019_morn
ggsave(filename = "map_redis_officers_quad_2019_morn.png",
       plot = map_redis_officers_quad_2019_morn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")

map_redis_officers_quad_2019_aftn <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='13-21',],
          #aes(fill = rn_of_police)) +
          aes(fill = rn_f_pl)) +
  labs(title = "Officers per Quadrant - Redistribution",
       subtitle = "Afternoon Shift",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
map_redis_officers_quad_2019_aftn
ggsave(filename = "map_redis_officers_quad_2019_aftn.png",
       plot = map_redis_officers_quad_2019_aftn,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")

map_redis_officers_quad_2019_nght <- ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='21-5',],
          #aes(fill = rn_of_police)) +
          aes(fill = rn_f_pl)) +
  labs(title = "Officers per Quadrant - Redistribution",
       subtitle = "Night Shift",
       fill = "Officers per Quadrant") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5, size = 15)) +
  scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
map_redis_officers_quad_2019_nght
ggsave(filename = "map_redis_officers_quad_2019_nght.png",
       plot = map_redis_officers_quad_2019_nght,
       path = "C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/DP--finalproject/Images")


