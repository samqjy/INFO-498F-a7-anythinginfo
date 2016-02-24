# Function that takes in data from the class survey and returns a list of information

library(dplyr)
data <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv", stringsAsFactors = FALSE)

info_summary <- function(data) {
  
  # Renaming column names
  data <- rename(data, 
                 class_standing = What.is.your.current.class.standing., 
                 informatics = Are.you.interested.in.applying.to.the.Informatics.major.,
                 rprogram = What.is.your.familiarity.with..Using.the.R.programming.language,
                 program_ex = What.is.your.programming.experience.,
                 countries = How.many.countries.have.you.visited.in.your.life.
  )
  
  info_list <- list()
  
  # Number of students in each class standing
  freshman <- sum((data$class_standing) == "Freshman")
  sophomore <- sum((data$class_standing) == "Sophomore")
  junior <- sum((data$class_standing) == "Junior")
  senior <- sum((data$class_standing) == "Senior")
  
  # Number of students applying to the Informatics program
  intended <- sum((data$informatics) == "Yes")  
  
  # Students familiar with programming
  experience <- sum((data$rprogram) == "Have used it a few times") %>%
    sum((data$rprogram) == "Intermediate user")
  
  no_exp <- sum((data$program_ex) == "Never written code")
  
  # Max number of countries visited by student
  traveled <- filter(data,
                     countries %in% max(countries)) %>%
    select(countries)
  
  # List of information about dataset
  info_list <- list(freshman = freshman, sophomore = sophomore, junior = junior, senior = senior,
                    total_students = sum(freshman, sophomore, junior, senior), applying = intended,
                    r_experience = experience, no_exp = no_exp, 
                    most_countries = traveled)
  
  return(info_list)
  
}