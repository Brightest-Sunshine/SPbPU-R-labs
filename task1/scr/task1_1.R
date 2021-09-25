path = 'D:/test_data.csv'

read_dataframe <- function(path) {
  dataframe <- read.csv(path)
  return(dataframe)
}

fix_column <- function(column){
  new_column <- sub(' ', '', column)
  new_column <- as.numeric(new_column)
  if(any(is.na(new_column))){
    return(column)
  }
  return(new_column)
}

fix_data <- function(dataframe){
  fix_dataframe <- lapply(dataframe, fix_column)
  fix_dataframe <- data.frame(fix_dataframe)
  return(fix_dataframe)
}

main <- function(){
  dataframe <- read_dataframe(path)
  fix_dataframe <- fix_data(dataframe)
  print(fix_dataframe)
}

main()
