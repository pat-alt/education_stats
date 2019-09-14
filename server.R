server = function(input, output, session) {
  
  # 1.) Setup ----
  shinyjs::runjs('toggleCodePosition();')
  
  # 2.) UI search engine ----
  output$eduSearch = renderUI({
    
    educationSubset = searchDatabase[category=="Education"]
    
    tagList(
      
      h4("Educational attainment statistics"),
      helpText("Below you can specify the level of educational attainment you would like to look at. You may also choose to divide the population by gender."),
      selectInput("level", label="Level of education",
                  choices = educationSubset[,unique(level)],
                  multiple = F, selected = "primary"),
      selectInput("gender", label = "Gender",
                  choices = educationSubset[,unique(gender)],
                  selected = "Total")
      
    )
    
  })
  
  # 3.) Other search enginge ----
  output$otherSearch = renderUI({
    
    req(input$gender)
    
    otherSubset = searchDatabase[category!="Education"]
    otherSubset = otherSubset[!(category=="Health" & !(gender %in% input$gender))]
    Category = otherSubset
    
    tagList(
      
      h4("Comparison measures"),
      helpText("Using the tree diagram below you can choose a development indicator you want to compare to the chosen educational attainment. Just click on the final node and the choice will appear in the confirmation box."),
      collapsibleTree(
        inputId = "otherSearch",
        Category,
        hierarchy = c("category", "name"),
        width = "100%",
        zoomable = FALSE
      )
      
    )
    
  })
  
  # 4.) Text output of choices ----
  output$yChoice = renderText({
    
    req(input$level, input$gender)
    sprintf("Education: %s, %s", input$level, input$gender)
    
  })
  output$xChoice = renderText({
    
    message = "Choose a comparison variable in the tree diagram above."
    
    if (!is.null(input$otherSearch$name)) {
      
      message = input$otherSearch$name
      
    }
    
    out = sprintf("Comparison: %s", message)
    
    return(out)
    
  })
  
  # 5.) Data ----
  dt = eventReactive(input$confirm,{
    
    req(input$level, input$gender)
    validate(need(
      !is.null(input$otherSearch$name),
      message = "Please select comparison data."
    ))
    
    # Indicators
    education = searchDatabase[category=="Education" & level==input$level & gender==input$gender,indicator]
    other = searchDatabase[category==input$otherSearch$category & name==input$otherSearch$name,indicator]
    chosenIndicators = c(education, other)
    
    withProgress(value = 1, message = "Loading and pre-processing data from World Bank", expr = {
      
      output = data.table(WDI(indicator = chosenIndicators))
      output[,iso2c:=NULL]
      output = melt(output, id.vars = c("country", "year"), variable.name="indicator")
      output = output[!is.na(value)]
      
      # Remove countries for which only one variable exists:
      countriesIntersect = Reduce(intersect, output[, .(list(unique(country))), by=indicator]$V1)
      output = output[country %in% countriesIntersect]
      
      # Merge with searchDatabase data:
      setkey(output, "indicator")
      output = searchDatabase[output]
      
    })
    
    return(output)
    
  })
  
  # 6.) Charts ----
  output$chartUI = renderUI({
    
    if (input$chartType == "scatter") {
      
      scatterUI("scatter")
      
    } else {
      
      tsUI("ts")
      
    }
    
  })
  
  observe({
    
    req(!is.null(dt()))
    req(input$chartType=="scatter")
  
    dt = dt()
    
    callModule(scatter, "scatter", data=dt)
    
  })
  
  observe({
    
    req(!is.null(dt()))
    req(input$chartType=="ts")
    
    dt = dt()
    
    callModule(ts, "ts", data=dt)
    
  })
  
  
}