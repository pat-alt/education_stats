
scatterUI = function(id, label) {
  
  # Create a namespace function using the provided id
  ns = NS(id)
  
  tagList(
    
    h4("Scatter plots and the added regression lines show relationships between variables. They cannot be used for causal inference. For example, even though life expectancy positive increases with the average level of education attained, other factors covarying with education - for example income - may also be at play."),
    br(),
    
    fluidRow(
      
      column(3,
             helpText("Once data has been retrieved you will be able to specify the year you want to look at below. For some years, data is only available for one of the two variables, so no scatter plot will be shown."),
             uiOutput(ns("year"))
      ),
      column(9,
             plotlyOutput(ns("scatter"),
                          height = "100%",
                          width = "100%")
      )
    )
    
  )
  
}

scatter = function(input, output, session, data) {
  
  ns = session$ns
  
  # 1) UI ----
  output$year = renderUI({
    
    ns = session$ns
    
    years = data[,sort(unique(year), decreasing = T)]
    selectInput(ns("year"), "Year", choices = years, selected = 2010)
    
  })
  
  # 2) Plot ----
  output$scatter = renderPlotly({
    
    req(input$year)
    
    validate(
      need(data[year==input$year, length(unique(indicator))>1],
           "Not enough data")
    )
    
    eduVar = data[category == "Education", unique(indicator)]
    eduLevel = data[category == "Education", unique(level)]
    eduGender = data[category == "Education", unique(gender)]
    eduVarName = sprintf("Attained at least %s education (%% %s population)", 
                         tolower(eduLevel), 
                         tolower(eduGender))
    otherVar = data[category != "Education", unique(indicator)]
    otherVarName = data[category != "Education", unique(name)]
    
    chartData = dcast(data[year==input$year], year + country ~ indicator, value.var = "value")
    
    
    chartData = chartData[!is.na(get(eduVar)) & !is.na(get(otherVar))]
    
    # Regression line:
    coeff = summary(lm(get(eduVar) ~ get(otherVar), data = chartData))$coefficients
    
    plot = ggplot(data = chartData) +
      theme_bw() +
      geom_point(aes(x=get(otherVar), y=get(eduVar), text=country),
                 colour="blue",
                 alpha=0.5,
                 size=1.5) +
      geom_abline(intercept = coeff[1,1], slope = coeff[2,1], colour=alpha("blue",0.5)) +
      xlab(otherVarName) +
      ylab(eduVarName)
    
    plot = ggplotly(plot, tooltip = "country")
    
    plot
    
  })
  
}