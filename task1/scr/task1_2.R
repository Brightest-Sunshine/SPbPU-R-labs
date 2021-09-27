path = 'D:/data.RData'

merge_dataframes <- function(dataframes){
   gluing <- Reduce(function(x, y) merge(x, y, by='id'), dataframes)
   return(gluing)
}

get_id <- function(dataframes){
  new_dataframe <- merge_dataframes(dataframes)
  dataframe_without_id <- new_dataframe[, -1]
  result <- data.frame(id=new_dataframe$id, mean_temp=rowMeans(dataframe_without_id))
  return(result)

main <- function(){
  load(path)
  result_table <- get_id(data)
  print(result_table)
}

main()
