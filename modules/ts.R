
tsUI = function(id, label) {
  
  # Create a namespace function using the provided id
  ns = NS(id)
  
  tagList(
    
    h4("Time series plots show plot normalized variables over time against each other. Variable are normalized to account for the fact in many cases they will be measured in different units."),
    br(),
    
    fluidRow(
      
      column(3,
             helpText("Once data has been retrieved you will be able to specify the year you want to look at below. For some years, data is only available for one of the two variables, so no scatter plot will be shown."),
             uiOutput(ns("country"))
      ),
      column(9,
             plotlyOutput(ns("ts"),
                          height = "100%",
                          width = "100%")
      )
    )
    
  )
  
}

ts = function(input, output, session, data) {
  
  ns = session$ns
  
  output$country = renderUI({
    
    countries = data[,sort(unique(country))]
    
    selectInput(ns("country"), label = "Choose a country",
                choices = countries)
    
  })
  
  # 1) Plot ----
  
  output$ts = renderPlotly({
    
    req(input$country)
    
    chartData = data[country == input$country]
    
    eduVar = chartData[category == "Education", unique(indicator)]
    eduLevel = chartData[category == "Education", unique(level)]
    eduGender = chartData[category == "Education", unique(gender)]
    eduVarName = sprintf("Attained at least %s education (%% %s population)",
                         tolower(eduLevel),
                         tolower(eduGender))
    chartData[category == "Education", name:= eduVarName]
    
    normalize = function(x) {
      
      z = sapply(1:length(x), function(i) {
        
        z = (x[i] - mean(x)) / sd(x)
        
        return(z)
        
      })
      
      return(z)
      
    }
    
    chartData[,value:=normalize(value), by=indicator]
    
    plot = ggplot(data = chartData) +
      theme_bw() +
      geom_line(aes(x=year, y=value, colour=name, text=name),
                 alpha=0.5,
                 size=0.75) +
      xlab("Year") +
      ylab("Value")
    
    plot = ggplotly(plot, tooltip = "country")
    
    plot
    
  })
  
}