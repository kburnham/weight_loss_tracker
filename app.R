#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(zoo)
library(tidyverse)
library(googlesheets)
library(lubridate)

initial_weight <- 257.6
target_weight <- 195.0

# read_log <- function() {
#   log <- "weight_loss_log" %>% gs_title() %>% gs_read_csv()
#   }

read_log <- function() readRDS("log.rds")

compute_ws <- function(log, days_to_average = 7) {
  log %>% 
    mutate(Source = "scale") %>% 
    right_join(data.frame(Date = seq(from = min(log$Date), to = max(log$Date), "days"))) %>% 
    arrange(desc(Date)) %>% 
    mutate(Source = ifelse(is.na(Source), "fill", "scale"),
           Weight = na.approx(Weight),
           MovingAverage = rollmean(Weight, days_to_average, fill = "extend", align = "left") %>% round(2),
           DailyWeightLoss = (MovingAverage - lead(MovingAverage)) * -1,
           WeightLossPast7Days = rollsum(DailyWeightLoss, 7, fill = "extend", align = "left") %>% round(2)
           )
  
  
}


daily_line <- function() {
  geom_line(aes(x = Date, y = Weight), color = "red")
}


# ws <- compute_ws(read_log())


ui <- fluidPage(
  
  title = "Weight Loss Tracker",
  
  plotOutput("plot"),
  
  hr(),
  
  fluidRow(
    
    column(3, 
           h6(paste0("Initial Weight: ", initial_weight)),
           h6(paste0("Target Weight: ", target_weight)),
           h6("Current Weight: "),
           textOutput("current_weight"),
           h6("Weight Loss To Date: "),
           textOutput("current_weight_loss"),
           h6("Average Daily Weight Loss (past 30 days): "), 
           textOutput("avg_daily_weight_loss"),
           h6("Projected target acquired date: "), 
           textOutput("projected_finish")
    ),
    
    
    column(4, offset = 1,
           h4("Add Weight"),
           dateInput("date_added", "Date to Add", value = Sys.Date()),
           numericInput("weight", "Weight", value = NA, step = .1),
           actionButton("add_weight", label = "Add Weight")
           ),
    column(4, 
           checkboxInput("include_dailies", "Show daily readings?", value = T),
           sliderInput("days_to_average", label = "Days to average", min = 1, max = 30, value = 7),
           sliderInput("date_range", "Date Range", min = as.Date("2017-07-01"),
                       max = as.Date("2018-12-31"), value = c(as.Date("2017-07-01"), Sys.Date())),
           sliderInput("weight_range", "Weight Range", min = 185, max = 270,
                       value = c(min, max))
           )
    
    ),

dataTableOutput("log")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  log <- eventReactive(input$add_weight, {
    if (!is.na(input$weight)) {
      dat <- data.frame(Date = input$date_added, Weight = as.numeric(input$weight))
      new <- bind_rows(read_log(), dat)
      saveRDS(new, "log.rds")
      return(new)
    } else {
      return(read_log())
    }
    
  }, ignoreNULL = FALSE)
  
  ws <- reactive({compute_ws(log(), days_to_average = input$days_to_average)})

  output$log <- renderDataTable({ws() %>% filter(Date >= input$date_range[1] &
                                                   Date <= input$date_range[2]) %>% arrange(desc(Date))})
   
   avg_daily_weight_loss <- reactive({mean(ws()$DailyWeightLoss[1:30]) %>% round(2)})
   
   output$projected_finish <- renderText({
     days_to_target <- (ws()$MovingAverage[1] - target_weight) / avg_daily_weight_loss()
     date <- Sys.Date() + days(days_to_target %>% ceiling)
     format(date, format = "%d %B %Y")
   })
   
   output$avg_daily_weight_loss <- renderText(avg_daily_weight_loss())
   output$current_weight <- renderText(ws()$MovingAverage[1])
   output$current_weight_loss <- renderText(initial_weight - ws()$MovingAverage[1])

   output$plot <- renderPlot({
     p <- ggplot(ws(), aes(x = Date, y = MovingAverage)) + 
       geom_line() +
       #geom_line(aes(x = Date, y = Weight), color = "red") +
       coord_cartesian(xlim = c(input$date_range[1], input$date_range[2]),
                       ylim = c(input$weight_range[1], input$weight_range[2])) +
       theme_bw()
     
     if (input$include_dailies) p <- p + daily_line()
     p
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

