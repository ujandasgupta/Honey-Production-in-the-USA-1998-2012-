#Libraries----------
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(tidyverse)
library(scales)
library(tidyquant)
library(forecast)

# Data ----------
dat <- read.csv("honeyproduction.csv", header = TRUE)

metric_choices <- colnames(dat)[3:ncol(dat)-1]
metric_names <- gsub("_", " ", metric_choices)
metric_names <- paste0(toupper(substr(metric_names,1,1)), substr(metric_names,2,nchar(metric_names)))

metric_list <- as.list(metric_choices)
names(metric_list) <- metric_names

# Forecast ----
create_forecast <- function(dat, num_forecasts){
  name_state <- unique(dat$state)
  auto_forecast <- forecast(auto.arima(dat$metric),num_forecasts)$mean
  max_year = max(dat$year)
  new_years = max_year + c(1:num_forecasts)
  new_forecast <- tibble(state = name_state, year = new_years, metric = as.vector(auto_forecast), forecast = 1)
  return(new_forecast)
}

# UI-----------
ui <- dashboardPage(
  
  skin = "red",
  
  # Header -----
  dashboardHeader(
    title = "Honey Production in the USA (1998-2012)",
    titleWidth = 350
  ),
  
  # Sidebar ------
  dashboardSidebar(
    
    shinyjs::useShinyjs(),
    
    width = 350,
    br(),
    h4("Select Your Inputs Here: ", style = "padding-left: 20px"),
    uiOutput("sidebar")
  ),
  
  # Body -----
  dashboardBody(
    h1("Abstract"),
    p("In this project we have taken a dataset containing data on the Honey Production
in the USA from 1998 to 2012.We use visualization techniques in R to evaluate
the change in the production of honey over the years in the US."),
    h1("Introduction"),
    p("In 2006, global concern was raised over the rapid decline in the honeybee population, an integral component to American honey agriculture. Large numbers
of hives were lost to Colony Collapse Disorder, a phenomenon of disappearing
worker bees causing the remaining hive colony to collapse. Speculation to the
cause of this disorder points to hive diseases and pesticides harming the pollinators, though no overall consensus has been reached. Twelve years later, some
industries are observing recovery but the American honey industry is still largely
struggling. The U.S. used to locally produce over half the honey it consumes
per year. Now, honey mostly comes from overseas, with 350 of the 400 million
pounds of honey consumed every year originating from imports. This dataset
provides insight into honey production supply and demand in America by state
from 1998 to 2012."),
    h1("Definition of Key Variables"),
    p("numcol: Number of honey producing colonies. Honey producing colonies
are the maximum number of colonies from which honey was taken during
the year. It is possible to take honey from colonies which did not survive
the entire year"),
    p("yieldpercol: Honey yield per colony. Unit is pounds"),
    p("totalprod: Total production (numcol x yieldpercol). Unit is pounds"),
    p("stocks: Refers to stocks held by producers. Unit is pounds"),
    p("priceperlb: Refers to average price per pound based on expanded sales.
Unit is dollars"),
    p("prodvalue: Value of production (totalprod x priceperlb). Unit is dollars"),
    tabsetPanel(
      type = "tabs",
      id = "tab_selected",
      tabPanel(
        title = "State View",
        plotOutput("state_plot"),
        uiOutput("forecast_panel")
      )
    )
  )
)



# Server ------------
server<- function(input, output){
  
  make_forecast <- reactiveValues(value = 0)
  
  # Data Cleaning Function
  clean_data_state <- reactive({
    clean_dat <- dat %>% 
      filter(state %in% input$state) %>%  
      group_by(state, year) %>% 
      select(state, year, input$metric) %>% 
      set_names(c("state","year","metric")) %>% 
      arrange(year)
  })
  
  # Plotting Data -----
  plot_data_state <- function(data){
    ma_years <- ifelse(input$moving_average == T, ma_years(), 0)
   ggplot(data = clean_data_state(), aes(y = metric, x = year, color = state))+
      geom_line(size = 1.5)+
      geom_ma(n = ma_years, size = 1)+
      ylab(metric_names[which(metric_choices == input$metric)])+
      xlab("Year")+
      labs(color = "State")+
      scale_y_continuous(label = comma)+
      theme(
        panel.background = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 25)
      )+
      ggtitle(metric_names[which(metric_choices == input$metric)])
  }
  
  plot_data_state_forecast <- function(data){
    ma_years <- ifelse(input$moving_average == T, ma_years(), 0)
    ggplot(data = forecast_data() %>% filter(forecast == 0), aes(y = metric, x = year, color = state))+
      geom_line(size = 1.5)+
      geom_ma(n = ma_years, size = 1)+
      geom_line(data = forecast_data() %>% filter(forecast == 1), size = 2.5, linetype = 7, alpha = 0.25)+
      ylab(metric_names[which(metric_choices == input$metric)])+
      xlab("Year")+
      scale_y_continuous(label = comma)+
      theme_bw()+
      theme(
        panel.background = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 25)
      )+
      geom_vline(xintercept = forecast_data() %>% filter(forecast == 0) %>% pull(year) %>% max,
                 linetype = "dotdash", size = 0.5)
  }
  # Render Plots -----
  output$state_plot <- renderPlot({
    req(input$state)
    ifelse(make_forecast$value == 0, return(plot_data_state(clean_data_state())), return(plot_data_state_forecast(clean_data_state())))
  })
  
  # Buttons, Toggles and Functions --------
  
  # ma_years(years in MA) -------
  ma_years <- eventReactive(input$moving_average_bttn,{
    req(input$moving_average_years)
    input$moving_average_years
  }, ignoreNULL = FALSE)
  
  # forecast_bttn ----
  observeEvent(input$forecast_bttn,{
    make_forecast$value = 1 
  })
  
  # remove_forecast_bttn ----
  observeEvent(input$remove_forecast_bttn, {
    make_forecast$value = 0
  })
  
  forecast_years <- eventReactive(input$forecast_bttn,{
    input$forecast
  })
  
  # Toggle MA --------
  observeEvent(input$moving_average,{
    if(input$moving_average == TRUE)
      shinyjs::show(id = "moving_average_years", anim = TRUE, animType = "slide")
    else{
      shinyjs::hide(id = "moving_average_years", anim = TRUE, animType = "fade")
    }
  })
  
  # Predictions by State ------
  predictions_by_state <- reactive({
    clean_data_state() %>% 
      group_map(~ create_forecast(.x, num_forecasts = forecast_years()), .keep = T)
  })
  
  #Forecast Data ------
  forecast_data <- reactive({
    unforecasted_data <- clean_data_state()
    unforecasted_data$forecast <- 0
    forecasted_data <- predictions_by_state()
    forecasted_data <- do.call(rbind, forecasted_data)
    rbind(unforecasted_data, forecasted_data)
  })
  
  # SELECT Inputs -----
  
 output$metric <- renderUI({
    # metric Input
    selectInput(
      inputId = "metric",
      label = strong("Select Metric"),
      choices = metric_list,
      selected = metric_list[1]
    )
  })
  
  output$state <- renderUI({
    #State Input ----
    selectInput(
      inputId = "state",
      multiple = TRUE,
      label = strong("Select States to Compare"),
      choices = sort(unique(dat$state)),
      selected = c("ND", "NY", "MN")
    )
  })
  
  # Choice for Moving Average Input -----
  output$moving_average <- renderUI({
    checkboxInput(
      inputId = "moving_average",
      label = div("Include Moving Average", style = "font-size: 12pt"),
      value = FALSE
    )
  })
  
  # Choice of time period for Moving Average
  output$moving_average_years <- renderUI({
    div(
      numericInput(
        inputId = "moving_average_years",
        label = "Number of Years for Moving Average",
        value = 2,
        min = 0,
        max = 10,
        step = 1
      ),
      actionButton(inputId = "moving_average_bttn",
                   label = "Update MA",
                   class = "btn-sucess"
      )
    )
  })
  
  # Forecast -----
  output$forecast <- renderUI({
    numericInput(
      inputId = "forecast",
      label = "Number of Years to Forecast",
      value = 2,
      min = 0,
      max = 10,
      step = 1
    )
  })
  
  #Forecast Button ----
  output$forecast_bttn <- renderUI({
    actionButton(inputId = "forecast_bttn",
                 icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color: white;",
                 label = "Make a Forecast!",
                 class = "btn btn-lg btn-primary"
                 )
  })
  
  # Remove Forecast Button -------
  output$remove_forecast_bttn <- renderUI({
    actionButton(inputId = "remove_forecast_bttn",
                 style = "color: white;",
                 label = "Remove",
                 class = "btn btn-lg btn-danger"
    )
  })
  
  # Forecast Panel ------
  output$forecast_panel <- renderUI({
    div(
      class = "jumbotron",
      div(
      class = "container bg-danger",
      h2("Forecast"),
      p("Select the Number of Years You'd Like to Forecast using ", code("R Shiny")),
      
      uiOutput("forecast"),
      uiOutput("forecast_bttn"),
      uiOutput("remove_forecast_bttn")
      )
    )
  })
  
  # UI Sidebar Output -----
  output$sidebar <- renderUI({
    div(
      uiOutput("metric"),
      uiOutput("state"),
      uiOutput("moving_average"),
      uiOutput("moving_average_years") %>% hidden()
    )
  })
}



shinyApp(ui, server)