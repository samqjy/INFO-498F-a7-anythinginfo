# Function that takes in data from the class survey and returns a list of information

library(dplyr)
info_data <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv", stringsAsFactors = FALSE)

info_summary <- function(dataset) {
  # Renaming column names
  new_info <- rename(info_data, 
                     standing = What.is.your.current.class.standing., 
                     informatics = Are.you.interested.in.applying.to.the.Informatics.major.,
                     os = What.operating.system.do.you.typically.use.,
                     rprogram = What.is.your.familiarity.with..Using.the.R.programming.language,
                     travel = How.many.countries.have.you.visited.in.your.life.,
                     pets = Do.you.consider.yourself.
                     )
  # List of information
  #info_list <- list()
  
  # Calculating number of students in each class standing
    class_standing <- new_info %>%
      group_by(standing) %>%
      summarise(
        standing_count = length(standing)) %>%
      select(standing_count)
  
  # Number of people interested in the Informatics program
    interested <- new_info %>%
      group_by(informatics) %>%
      summarise(
        interest_count = length(informatics)
      ) %>%
      select(interest_count)
    
  # What type of operating system students used
    operating <- new_info %>%
      group_by(os) %>%
      summarise(
        os_count = length(os)
      ) %>%
      select(os_count)
    
  # Familiarity with R programming language
    rlanguage <- new_info %>%
      group_by(rprogram) %>%
      summarise(
        r_count = length(rprogram)
      ) %>%
      select(r_count)
    
  # How many countries has students traveled to
    countries <- filter(new_info, 
                          travel %in% max(travel))
}  



