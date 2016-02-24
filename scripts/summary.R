# Function that takes in data from the class survey and returns a list of information

library(dplyr)

info_summary <- function(dataset) {
  
  # List of information
  info_list <- list()
  
  # Calculate the number of students in class
  info_list$num <- nrow(dataset)
  
  # Calculate number of students in each class standing
  info_list$numFreshmen <- nrow(filter(dataset, What.is.your.current.class.standing. == "Freshman"))
  info_list$numSophomore <- nrow(filter(dataset, What.is.your.current.class.standing. == "Sophomore"))
  info_list$numJunior <- nrow(filter(dataset, What.is.your.current.class.standing. == "Junior"))
  info_list$numSenior <- nrow(filter(dataset, What.is.your.current.class.standing. == "Senior"))
  
  # Calculate the number of Windows and Mac users
  info_list$numWindows <- nrow(filter(dataset, What.operating.system.do.you.typically.use. == "Windows"))
  info_list$numMac <- nrow(filter(dataset, What.operating.system.do.you.typically.use. == "Mac"))
    
  return(info_list)
}  



