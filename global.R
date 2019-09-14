# Load packages:
library("shiny")
library("data.table")
library("WDI")
library("collapsibleTree")
library("ggplot2")
library("plotly")
library("shinyjs")

# Load modules:
invisible(lapply(list.files("modules"), function(i) source(sprintf("modules/%s", i))))

# 1.) Building a mini search data base ----
# Using WDIsearch from the WDI package the below filters for a subset of World Bank Development Indicators
# relevant for the app and comiles them in a small data.table.

# 1.1) Educational attainment data: ----
education = data.table(WDIsearch("Educational attainment"))
education = education[grepl("SE.",indicator)]
education[, category:="Education"]

# 1.2) Inequality: ----
gini = data.table(WDIsearch("Gini Coefficient"))
gini = gini[name=="Gini Coefficient"]
gini[, category:="Inequality"]

# 1.3) Poverty: ----
poverty = data.table(WDIsearch("Poverty gap at"))
poverty = poverty[grepl("PPP",name)]
poverty = poverty[!grepl("Increase", name)]
poverty = poverty[!grepl("GP", indicator)]
poverty[, category:="Poverty"]

# 1.4) Health ----
health = data.table(WDIsearch("Prevalence of undernourishment"))
health = rbind(health, WDIsearch("Life expectancy at birth"))
health[,category:="Health"]

# 2.) Merge different subjects ----
searchDatabase = rbindlist(list(education,
                                gini,
                                poverty,
                                health))

# 3.) Extract information from names ----

# 3.1) Gender ----
searchDatabase[,gender:="Total"]
searchDatabase[grepl("male", name,ignore.case = T),gender:="Male"]
searchDatabase[grepl("female", name,ignore.case = T),gender:="Female"]

# 3.2) Education level ----
searchDatabase[category=="Education" & grepl("primary", name), level:="Primary"]
searchDatabase[category=="Education" & grepl("lower secondary", name), level:="Lower secondary"]
searchDatabase[category=="Education" & grepl("upper secondary.", name), level:="Upper secondary"]
searchDatabase[category=="Education" & grepl("post-secondary", name), level:="Post-secondary"]
searchDatabase[category=="Education" & grepl("tertiary", name), level:="Tertiary"]
searchDatabase[category=="Education" & grepl("Bachelor", name), level:="Bachelor"]
searchDatabase[category=="Education" & grepl("Master", name), level:="Master"]
searchDatabase[category=="Education" & grepl("Doctor", name), level:="Doctor"]

setkey(searchDatabase, "indicator") # setting keys speeds up data.table calculations
