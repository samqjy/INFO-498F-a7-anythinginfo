# Function takes in class survey data and returns bar chart of
# the classes of Students grouped by if they said yes, no, N/A, or
# maybe when asked if they wanted to apply to informatics

library(dplyr)
library(plotly)
library(graphics)#for layout
blob <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv", stringsAsFactors = FALSE)

class_major <- function(data){
  #setting class and major preference data to a shorter name
  data$class <- data$What.is.your.current.class.standing.
  data$major <- data$Are.you.interested.in.applying.to.the.Informatics.major.
 
# getting the number of each class in eachof the major preferences category
  Major_Pref <- data%>% 
    group_by(major)%>%
    select(major,class)%>%
    summarise(
      Freshmen = length(class[class== "Freshman" ]),
      Sophomore = length(class[class== "Sophomore" ]),
      Junior = length(class[class== "Junior" ]),
      Senior = length(class[class== "Senior" ])
    )
  
  # creating a bar chart
    CHART <-Major_Pref%>%
      plot_ly(
      x = major,
      y = Freshmen,
      name = "Freshman",
      type = "bar",
      marker = list(color = "rgb(216, 217, 218)")
    )
    
    # adding bar charts one by one to use UW colors
    CHART <- Major_Pref%>%
      add_trace(
      x = major,
      y = Sophomore,
      name = "Sophomore",
      type = "bar",
      marker = list(color = "rgb(153, 153, 153)")
    )
  
    CHART <- Major_Pref%>%
      add_trace(
      x = major,
      y = Junior,
      name = "Junior",
      type = "bar",
      marker = list(color = "rgb(232, 211, 162)")
    )
    
    CHART <- Major_Pref%>%
      add_trace(
      x = major,
      y = Senior,
      name = "Senior",
      type = "bar",
      marker = list(color = "rgb(51, 0, 111)")
    )%>%
      layout(
        yaxis= list(title = "Number of Students")#setting yaxis title
      )
    
  return(CHART)
  
}


    