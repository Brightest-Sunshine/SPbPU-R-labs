library(ggplot2)

path = 'D:/data_tula.csv'

read_dataframe <- function(path) {
  dataframe <- read.csv(path, sep=";", encoding="UTF-8")
  return(dataframe)
}

change_names <- function(dataframe){
  colnames(dataframe) <- c("Breeds", "Name_indicator", "Unit", "Value_indicator")
  return(dataframe)
}

select_data <- function(dataframe){
  new_table <- subset(dataframe, Name_indicator == "Площадь земель, занятых лесными насаждениями (покрытых лесной растительностью), всего")
  return(new_table)
}

plot_graphics <- function(dataframe){
  gg1 <- ggplot(data = dataframe, mapping = aes(x = Breeds, y = Value_indicator, fill = Breeds)) +
    geom_col() + coord_flip() +
    xlab("Породы") + ylab("Значение, тыс. га") +
    ggtitle('Столбчатая диаграмма') + 
    geom_text(aes(label = Value_indicator), vjust = 0.5, colour = "gray32", fontface = 'bold')
  
  gg1 <- gg1 + guides( fill = guide_legend(title = "Породы", override.aes = aes(label = "")))
  print(gg1)
  
  gg2 <- ggplot(data = dataframe, mapping = aes(x = '', y = Value_indicator, fill = Breeds)) +
    geom_col() + coord_polar(theta = 'y') +
    ggtitle('Круговая диаграмма')
  gg2 <- gg2 + guides(fill = guide_legend(title = "Породы", override.aes = aes(label = "")))
  print(gg2)
  
}

data <- read_dataframe(path)
data <- change_names(data)
new_data <- select_data(data)
new_data[[4]] <- sub(',', '.', new_data[[4]])
new_data[[4]] <- as.numeric(new_data[[4]])
plot_graphics(new_data)

