library(tidyverse)
library(shiny)
library(plotly)
library(lubridate)
library(sf)
library(ggplot2)
#install.packages("ggmap")
library(ggmap)
#install.packages('rsconnect')
library(rsconnect)

setwd("C:/Users/52322/OneDrive - The University of Chicago/Documents/Harris/2022 Winter/Data and Programming II/Final Project/Data")

ui <- fluidPage(
  fluidRow(
    column(width = 12,
           align = "center",
           tags$h1("Fighting Crime in Medellin, COL"),
           tags$hr()
    )
  ),
  fluidRow(
    column(width = 12,
           align = "center",
           tags$h4("Improving police patrol allocation to combat crime"),
           tags$h6("2018-2021"),
    )
  ),
  fluidRow(
    column(width = 6,
           align = "center",
           tags$h4("Police patrol distribution"),
           tags$hr()),
    column(width = 6,
           align = "center",
           tags$h4("Crime distribution"),
           tags$hr())
  ),
  fluidRow(
    column(width = 3,
           align = "center",
           selectInput(inputId = "shift_pol",
                       label = "Select a shift",
                       choices = c("morning", "afternoon", "night"))),
    column(width = 3,
           align = "center",
           radioButtons(inputId = "time_pol",
                        label = "Before or after redistribution?",
                        choices = c("Before", "After"),
                        selected = "Before")),
    column(width = 3,
           align = "center",
           selectInput(inputId = "shift_cri",
                        label = "Select a shift",
                        choices = c("morning", "afternoon", "night"))),
    column(width = 3,
           align = "center",
           radioButtons(inputId = "time_cri",
                        label = "Before or after redistribution?",
                        choices = c("Before", "After"),
                        selected = "Before"))
  ),
  fluidRow(
    column(width = 6,
           align = "center",
           plotlyOutput(outputId = "police")),
      column(width = 6,
             align = "center",
             plotlyOutput(outputId = "crimes"))
  )
)

server <- function(input, output) {
  df_shift <- st_read("df_shifts_avg.shp")
  
  output$police <- renderPlotly({
    ifelse(input$time_pol == "Before",
           plt <- ggplot() +
             geom_sf(data = df_shift[df_shift$shift == input$shift_pol,],
                     #aes(fill = n_of_police)) +
                     aes(fill = n_f_plc)) +
             labs(title = "Officers per Quadrant",
                  subtitle = "Uniform distribution - 2 per quadrant",
                  fill = "Officers per Quadrant") +
             theme(plot.title = element_text(hjust = 0.5, size = 15),
                   plot.subtitle = element_text(hjust = 0.5, size = 10)) +
             scale_fill_viridis_c(option = "mako", limits = c(1, 8)),
           plt <- ggplot() +
             geom_sf(data = df_shift[df_shift$shift == input$shift_pol,],
                     #aes(fill = rn_of_police)) +
                     aes(fill = rn_f_pl)) +
             labs(title = "Officers per Quadrant",
                  fill = "Officers per Quadrant") +
             theme(plot.title = element_text(hjust = 0.5, size = 15),
                   plot.subtitle = element_text(hjust = 0.5, size = 10)) +
             scale_fill_viridis_c(option = "mako", limits = c(1, 8)) 
           )
    ggplotly(plt)
  
  })
  
  output$crimes <- renderPlotly({
    ifelse(input$time_cri == "Before",
           plt <- ggplot() +
             geom_sf(data = df_shift[df_shift$shift == ifelse(input$shift_cri == "morning", "5-13", ifelse(input$shift_cri == "afternoon", "13-21", "21-5")),],
                     aes(fill = cpp)) +
             labs(title = "Crimes per Officer",
                  fill = "Crimes per Officer",
                  color = "Crimes per Officer") +
             theme(plot.title = element_text(hjust = 0.5, size = 15)) +
             scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
             scale_color_viridis_c(option = "inferno", limits = c(0, 80)) ,
           plt <-  ggplot() +
             geom_sf(data = df_shift[df_shift$shift == ifelse(input$shift_cri == "morning", "5-13", ifelse(input$shift_cri == "afternoon", "13-21", "21-5")),],
                     aes(fill = rcpp)) +
             labs(title = "Crimes per Officer",
                  fill = "Crimes per Officer",
                  color = "Crimes per Officer") +
             theme(plot.title = element_text(hjust = 0.5, size = 15)) +
             scale_fill_viridis_c(option = "inferno", limits = c(0, 80)) +
             scale_color_viridis_c(option = "inferno", limits = c(0, 80)) 
    )
    ggplotly(plt)
    
  })
  
}
shinyApp(ui = ui, server = server)



ggplot() +
  geom_sf(data = df_shift[df_shift$shift=='5-13',],
          aes(fill = sum)) +
  labs(title = "Morning shift (5:00 - 13:00)",
       fill = "Crimes",
       color = "Crimes") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_fill_viridis_c(option = "inferno", limits = c(0, 170)) +
  scale_color_viridis_c(option = "inferno", limits = c(0, 170)) 