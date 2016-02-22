# Function takes in class survey data and returns a scatter plot of
# the classes of Students grouped by the number of countries they 
# have visited before.

library(dplyr)
library(plotly)
library(graphics)#for layout
blob <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv", stringsAsFactors = FALSE)

class_travel <- function(data){
  data$class <- data$What.is.your.current.class.standing.
  data$countries <- data$How.many.countries.have.you.visited.in.your.life.
  
  Country_Pref <- data %>% 
    group_by(countries)%>%
    select(countries, class)%>%
    summarise(
      Freshman = length(class[class== "Freshman" ]),
      Sophomore = length(class[class== "Sophomore" ]),
      Junior = length(class[class== "Junior" ]),
      Senior = length(class[class== "Senior" ])
    )
  
  chart <- Country_Pref %>%
           plot_ly(x = countries, 
                   y = Freshman, 
                   name = 'Freshman',
                   mode = "markers", 
                   marker = list(color = "rgb(153, 153, 153)", 
                                 symbol = 'square',
                                 size = 10))
  
  chart <- Country_Pref %>%
           add_trace(x = countries, 
                     y = Sophomore, 
                     name = 'Sophomore',
                     mode = "markers", 
                     marker = list(color = "rgb(216, 217, 218)", 
                                   symbol = 'diamond',
                                   size = 10))
  
  chart <- Country_Pref %>%
    add_trace(x = countries, 
              y = Junior, 
              name = 'Junior',
              mode = "markers", 
              marker = list(color = "rgb(232, 211, 162)", 
                            symbol = 'circle',
                            size = 10))
  
  chart <- Country_Pref %>%
    add_trace(x = countries, 
              y = Senior, 
              name = 'Senior',
              mode = "markers", 
              marker = list(color = "rgb(50, 0, 111)", 
                            symbol = 'cross',
                            size = 10)) %>%
              layout(
                yaxis = list(title = "Number of Students"),
                xaxis = list(title = "Countries visited")
              )
  return(chart)
}


