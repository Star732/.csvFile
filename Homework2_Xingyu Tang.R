library(tidyverse)
library(shiny)
library(lubridate)

# Import data
df <- read_csv("https://raw.githubusercontent.com/Star732/.csvFile/main/all_stocks_5yr.csv")

# Create indicators
RSI = function(price, ndays = 14){
  N = length(price)
  U = rep(0, N)
  D = rep(0, N)
  
  for (i in 2:N){
    flag = price[i] - price[i-1]
    if (flag > 0){
      U[i] = flag
    }else{
      D[i] = -flag
    }
  }
  
  U.mean = c(); U.mean[ndays+1] = mean(U[2:(ndays+1)])
  D.mean = c(); D.mean[ndays+1] = mean(D[2:(ndays+1)])
  RS = c(); RS[ndays+1] = U.mean[ndays+1] / D.mean[ndays+1]
  RSI = c(); RSI[ndays+1] = 100 - 100/(1+RS[ndays+1])
  
  for (i in (ndays+2):N){
    U.mean[i] = ((ndays-1)*U.mean[i-1] + U[i]) / ndays
    D.mean[i] = ((ndays-1)*D.mean[i-1] + D[i]) / ndays
    RS[i] = U.mean[i] / D.mean[i]
    RSI[i] = 100 - 100/(1+RS[i])
  }
  
  return(RSI)
}

# Shiny APP
ui <- fluidPage(
  titlePanel("Visualization of Som S&P500 Index Stocks"),
  
  selectInput("stock", "Stock", choices = df$Name, multiple = FALSE),
  
  dateRangeInput("time", "Enter the Time Period", start = "2013-02-08", end = "2018-02-07", min = "2013-02-08", max = "2018-02-07", format = "yyyy-mm-dd"),
  
  checkboxGroupInput("indicator", "Algorithms", choices = c("close", "RSI(6 days)", "RSI(12 days)", "RSI(14 days)", "RSI(24 days)")),
  
  mainPanel(
    plotOutput("pic"),
    dataTableOutput("table")
  )
)

server <- function(input, output){
  output$pic <- renderPlot({
    df_subset <- df %>%
      filter(Name == input$stock & mdy(date) >= input$time[1] & mdy(date) <= input$time[2])
    
    algorithms <- data.frame(matrix(c(df_subset$date, df_subset$close, RSI(df_subset$close, ndays = 6), RSI(df_subset$close, ndays = 12), RSI(df_subset$close, ndays = 14), RSI(df_subset$close, ndays = 24)), ncol = 6))
    
    colnames(algorithms) <- c("date", "close", "RSI(6 days)", "RSI(12 days)", "RSI(14 days)", "RSI(24 days)")
    
    usedf <- algorithms %>%
      pivot_longer(!date, names_to = "algorithm", values_to = "value") %>%
      filter(algorithm %in% input$indicator)
    
    usedf$value <- as.numeric(usedf$value)
    
    ggplot(usedf) +
      geom_line(aes(x = 1:length(date), y = value, col = algorithm)) +
      labs(x = "Days", y = "Price", col = "Algorithm") +
      theme_bw()
  })
  
  output$table <- renderDataTable({
    df_subset <- df %>%
      filter(Name == input$stock & mdy(date) >= input$time[1] & mdy(date) <= input$time[2])
    
    algorithms <- data.frame(matrix(c(df_subset$date, df_subset$close, RSI(df_subset$close, ndays = 6), RSI(df_subset$close, ndays = 12), RSI(df_subset$close, ndays = 14), RSI(df_subset$close, ndays = 24)), ncol = 6))
    
    colnames(algorithms) <- c("date", "close", "RSI(6 days)", "RSI(12 days)", "RSI(14 days)", "RSI(24 days)")
    
    algorithms %>%
      pivot_longer(!date, names_to = "algorithm", values_to = "value") %>%
      filter(algorithm %in% input$indicator) %>%
      pivot_wider(names_from = algorithm, values_from = value)
  })
}

shinyApp(ui, server)


