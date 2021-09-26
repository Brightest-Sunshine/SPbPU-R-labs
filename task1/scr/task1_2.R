path = 'D:/data.RData'

merge_dataframes <- function(dataframes){
   gluing <- Reduce(function(x, y) merge(x, y, by='id'), dataframes)
   return(gluing)
}

get_id <- function(dataframes){
  new_dataframe <- merge_dataframes(dataframes)
  fix_dataframe <- new_dataframe[, -1]
  means <- rowMeans(fix_dataframe)
  result <- data.frame(id=new_dataframe$id, mean_temp=means)
  return(result)
}

main <- function(){
  load(path)
  result_table <- get_id(data)
  print(result_table)
}

main()