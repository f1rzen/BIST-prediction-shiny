library(shiny)
library(bslib)
library(thematic)
library(highcharter)

liste <- read.delim('https://raw.githubusercontent.com/f1rzen/halkbank-skorkart-scraper/main/semboller.txt', header = F)

# Define UI for application that plots selected stock prices
thematic::thematic_shiny()
shinyUI(fluidPage(
  theme = bs_theme(bg = "#ECF0F1", fg = "#000", primary = "#9B59B6",
                   base_font = font_google("Fira Sans"),
                   code_font = font_google("Space Mono")),
  

  # Application title
        selectInput("sembol", "Sembol Seçiniz:", c(liste)),
    
    # Show a plot of the selected stock prices
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",highchartOutput("fiyatPlot", width = "100%")),
        tabPanel("ACF PACF", tabsetPanel(
          tabPanel("0",fluidRow(
            column(6,highchartOutput("acfPlot0")),
            column(6,highchartOutput("pacfPlot0")),
            )),
          tabPanel("diff1",fluidRow(
            column(6,highchartOutput("acfPlot1")),
            column(6,highchartOutput("pacfPlot1")),
          )),
          tabPanel("diff2",fluidRow(
            column(6,highchartOutput("acfPlot2")),
            column(6,highchartOutput("pacfPlot2")),
          )),
          tabPanel("diff3",fluidRow(
            column(6,highchartOutput("acfPlot3")),
            column(6,highchartOutput("pacfPlot3")),
          ))
        )),
        tabPanel("ARIMA", fluidRow(column(6, highchartOutput("arimaPlot1825")),
                                   column(6, highchartOutput("arimaPlot365")),
                                   column(6, highchartOutput("arimaPlot180")),
                                   column(6, highchartOutput("arimaPlot90")),
                                   column(6, highchartOutput("arimaPlot30")),
                                   column(6, highchartOutput("arimaPlot7"))
                                   )),
        
      ),
      width = "100%"
    ),
      
  
    )
  )
