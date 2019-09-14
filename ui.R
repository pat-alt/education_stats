ui = function(request) {
  
  fluidPage(
    
    useShinyjs(),
    
    titlePanel("Education in numbers"),
    
    br(),
    
    h4("This simple app can be used to acces the World Bank Development Indicator (WDI) data base and visualize relationships between selected variables in real-time. WDI data is public and accessed through its API."),
    
    br(),
    
    h3("1. Make your variable choices here"), 
    br(),
    
    fluidRow(
      
      column(3,
             uiOutput("eduSearch")
      ),
      
      column(6,
             uiOutput("otherSearch")
      ),
      
      column(3,
             h4("Confirm your choices"),
             helpText("Confirm your choices below. This will trigger an API request to the World Bank Development Indicator data base."),
             wellPanel(
               textOutput("yChoice"),
               textOutput("xChoice"),
               br(),
               actionButton("confirm", "Confirm choice")
             )
      )
      
    ),
    
    hr(), 
    
    h3("2. Your chart will appear below:"), 
    br(),
    selectInput("chartType", "Choose chart type",
                choices = c("Scatter plot" = "scatter",
                            "Time series chart" = "ts"),
                selected = "scatter"),
    br(),
    uiOutput("chartUI")
    
  )
  
}  
  
