path = 'D:/RH_T.csv'

read_dataframe <- function(path) {
  dataframe <- read.csv(path)
  return(dataframe)
}

add_dayName <- function(dataframe){
  col <- weekdays(as.Date(dataframe[, 8]))
  dataframe$dayName <- col
  return(dataframe)
}

find_min_max_temp <- function(dataframe){
  all_wednesdays <- dataframe[ which (dataframe$dayName == 'среда'),]
  all_thursdays <- dataframe[ which (dataframe$dayName == 'четверг'),]
  min_index <- which.min(all_wednesdays[,10])
  max_index <- which.max(all_thursdays[,10])
  cat("Самая холодная среда была",  all_wednesdays[min_index, 8],"средняя температура равнялась", all_wednesdays[min_index, 10],'\n')
  cat("Самый тёплый четверг был", all_thursdays[max_index, 8], "средняя температура равнялась", all_thursdays[max_index, 10])
}

main <- function(){
  data <- read_dataframe(path)
  new_data <- add_dayName(data)
  find_min_max_temp(new_data)
}

main()