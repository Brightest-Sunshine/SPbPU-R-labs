path = 'D:/RH_T.csv'

read_dataframe <- function(path) {
  dataframe <- read.csv(path)
  return(dataframe)
}

select_necessary_columns <- function(dataframe){
  new_dataframe <-  dataframe[, c('YEAR', 'MM', 'DD', 'YYYYMMDD', 'T2M')]
  return(new_dataframe)
}

change_month_code <- function(dataframe){
  old <- 1:12
  new <- c(1, 4, 4, 0, 2, 5, 0, 3, 6, 1, 4, 6)
  dataframe$MM[dataframe$MM %in% old] <-new[match(dataframe$MM, old, nomatch = 0)]
  return(dataframe)
}

change_years_code <- function(dataframe){
  old <- c(2020, 2021)
  code_for_2020 <- (6 + (2020 %% 100) + ((2020 %% 100) %/% 4)) %% 7
  code_for_2021 <- (6 + (2021 %% 100) + ((2021 %% 100) %/% 4)) %% 7
  new <- c(code_for_2020, code_for_2021)
  dataframe$YEAR[dataframe$YEAR %in% old] <-new[match(dataframe$YEAR, old, nomatch = 0)]
  return(dataframe)
}

solve_day_of_week <- function(string){
  if(string[[4]] >= '2020-01-01' & string[[4]] <= '2020-02-29'){
    day_of_week <- ((as.numeric(string[[3]]) + as.numeric(string[[2]]) + as.numeric(string[[1]])- as.numeric(1)) %% 7) 
    if (day_of_week == -1){
      day_of_week <- 6
    }
  }
  else
    day_of_week <- (as.numeric(string[[3]]) + as.numeric(string[[2]]) + as.numeric(string[[1]])) %% 7
  return(day_of_week)
}

add_day_of_week <- function(dataframe){
  NameDay <- apply(dataframe, 1, solve_day_of_week)
  dataframe$NameDay <- NameDay
  return(dataframe)
}

select_all_wednesdays <- function(dataframe){
  df_of_wednesdays <- dataframe[which (dataframe$NameDay == 4),]
  return(df_of_wednesdays)
}

select_all_thursday <- function(dataframe){
  df_of_thursdays <- dataframe[which (dataframe$NameDay == 5), ]
  return(df_of_thursdays)
}

find_min_temp <- function(dataframe){
  index <- which.min(dataframe[,5])
  cat("Самая холодная среда была", dataframe[index, 4],"средняя температура равнялась", dataframe[index, 5],'\n')
}

find_max_temp <- function(dataframe){
  index <- which.max(dataframe[,5])
  cat("Самый тёплый четверг был", dataframe[index, 4], "средняя температура равнялась", dataframe[index, 5])
}

main <- function(){
  data <- read_dataframe(path)
  data <- select_necessary_columns(data)
  data <- change_month_code(data)
  data <- change_years_code(data)
  data <- add_day_of_week(data)
  wednesdays <- select_all_wednesdays(data)
  thursdays <- select_all_thursday(data)
  find_min_temp(wednesdays)
  find_max_temp(thursdays)
}

main()
