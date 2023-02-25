library(shiny)
library('TTR')
library('quantmod')
library(highcharter)
library(xts)
library(forecast)
library(fpp)

# Define server logic required to plot selected stock prices
shinyServer(function(input, output) {

  fiyat <- reactive({
    getSymbols(paste0(input$sembol,".IS"),src='yahoo',auto.assign=FALSE)
  })
  
  
  output$fiyatPlot <- renderHighchart({
    
      highchart(type = "stock") %>% 
        hc_add_series(fiyat()) %>% 
        hc_add_theme(hc_theme_flat()) %>% 
        hc_rangeSelector(selected = 1)
      # chartSeries(fiyat(), subset = "last 6 months")
      
    })
  
  ####ACF PACF GRAPHS####
  diff1 <- reactive({diff(fiyat())})
  diff2 <- reactive({diff(diff1())})
  diff3 <- reactive({diff(diff2())})
  
  acf0 <- reactive({Acf(tail(fiyat()[,6], n=180), lag.max=42, lwd=3, ylim= c(-1,1), plot = F)})
  pacf0<- reactive({Pacf(tail(fiyat()[,6], n=180), lag.max=42,  lwd=3, ylim= c(-1,1), plot = F)})
  acf1 <- reactive({Acf(tail(diff1()[,6], n=180), lag.max=42,  lwd=3)})
  pacf1<- reactive({Pacf(tail(diff1()[,6], n=180), lag.max=42,  lwd=3)})
  acf2 <- reactive({Acf(tail(diff2()[,6], n=180), lag.max=42,  lwd=3)})
  pacf2<- reactive({Pacf(tail(diff2()[,6], n=180), lag.max=42,  lwd=3)})
  acf3 <- reactive({Acf(tail(diff3()[,6], n=180), lag.max=42,  lwd=3)})
  pacf3<- reactive({Pacf(tail(diff3()[,6], n=180), lag.max=42,  lwd=3)})
  
  
  ## 0 Difference ACF PACF ###
  output$acfPlot0 <- renderHighchart({
    hchart(acf0()) %>%
      hc_add_theme(hc_theme_flat())
  })
  output$pacfPlot0 <- renderHighchart({
    hchart(pacf0()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  
  ## 1 Difference ACF PACF ###
  output$acfPlot1 <- renderHighchart({
    hchart(acf1()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  output$pacfPlot1 <- renderHighchart({
    hchart(pacf1()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  
  ## 2 Difference ACF PACF ###
  
  output$acfPlot2 <- renderHighchart({
    hchart(acf2()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  output$pacfPlot2 <- renderHighchart({
    hchart(pacf2()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  
  ## 3 Difference ACF PACF ###
  
  output$acfPlot3 <- renderHighchart({
    hchart(acf3()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  output$pacfPlot3 <- renderHighchart({
    hchart(pacf3()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  

  
  
  
  
  
  
  ### ARIMA ####
  priceForecast1825 <- reactive({
    forecast(auto.arima(tail(fiyat()[,6], n=1825)))
  })

  priceForecast365 <- reactive({
    forecast(auto.arima(tail(fiyat()[,6], n=365)))
  })
  priceForecast180 <- reactive({
    forecast(auto.arima(tail(fiyat()[,6], n=180)))
  })
  priceForecast90 <- reactive({
    forecast(auto.arima(tail(fiyat()[,6], n=90)))
  })
  priceForecast30 <- reactive({
    forecast(auto.arima(tail(fiyat()[,6], n=30)))
  })
  priceForecast7 <- reactive({
    forecast(auto.arima(tail(fiyat()[,6], n=7)))
  })
  output$arimaPlot1825 <- renderHighchart({
    hchart(priceForecast1825()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  output$arimaPlot365 <- renderHighchart({
    hchart(priceForecast365()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  output$arimaPlot180 <- renderHighchart({
    hchart(priceForecast180()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  output$arimaPlot90 <- renderHighchart({
    hchart(priceForecast90()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  output$arimaPlot30 <- renderHighchart({
    hchart(priceForecast30()) %>% 
      hc_add_theme(hc_theme_flat())
  })
  output$arimaPlot7 <- renderHighchart({
    hchart(priceForecast7()) %>% 
      hc_add_theme(hc_theme_flat())
  })

  
  
  
  
  })
