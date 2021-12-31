library(ggplot2)

path = 'D:/trades.RData'
export = 'Exports in million of ECU/EURO'
import = 'Imports in million of ECU/EURO'
share_export = 'Share of exports by partner (%)'
MIN = 10000


merge_trades <- function(trades) {
  c = trades[[1]]
  for (i in 2:length(trades)) {
    c = mapply(c, c, trades[[i]], SIMPLIFY=FALSE)
  }
  return(subset(as.data.frame(c), select = -geo ))
}


get_export <- function(table_trades) {
  return(table_trades[table_trades$indic_et == export, ])
}


get_import <- function(table_trades) {
  return(table_trades[table_trades$indic_et == import, ])
}


get_share_export <- function(table_trades){
  return(table_trades[table_trades$indic_et == share_export, ])
}


plot_graphics <- function(export_data, leaders){
   gg1 <- ggplot(export_data, aes(x = partner, y = values, fill = sitc06)) +
          geom_col(position = "dodge") +
          xlab('Страны-партнеры') + 
          ylab('Объем экспорта') +
          ggtitle('Общий объем экспорта в Европе за 2008-2019 гг') + 
          scale_fill_discrete(name = 'Экспортируемые товары') + 
          coord_flip()
   print(gg1)
   
   gg2 <- ggplot(leaders, aes(x = partner, y = values, fill = sitc06)) +
          geom_col(position = "dodge") +
          xlab('Страны-партнеры') + 
          ylab('Объем экспорта') +
          ggtitle('Ведущие страны по экспорту') + 
          scale_fill_discrete(name = 'Экспортируемые товары') + 
          coord_flip()
   print(gg2)
}


plot_graphic <- function(export_data, str_){
  gg <- ggplot(export_data, aes(x = partner, y = values, group = sitc06)) + 
        geom_col(aes(fill = sitc06)) +
        xlab('Страны-партнеры') +
        ylab('Объем экспорта') +
        ggtitle(paste0('Ведущие экспортеры в Европе за ', str_, ' г.')) +
        scale_fill_discrete(name = 'Экспортируемые товары') + coord_flip() +
        geom_text(aes(label = share), position = position_stack(vjust = 0.5))
  print(gg)
}


main <- function(){
  load(path)
  table_trades <- merge_trades(trades)
  import_data <- get_import(table_trades)
  export_data <- get_export(table_trades)
  union_data <- bind_rows(export_data, import_data)
  leaders <- subset(export_data, export_data$value >= MIN) 
  
  plot_graphics(export_data, leaders)
  
  share_export_data <- get_share_export(table_trades)
  share_data <- cbind(export_data, share = share_export_data$values)
  dates <- unique(share_data$time)
  lapply(dates, 
         function(date) {
           export_data = subset(share_data, share_data$time == date & share_data$value >= MIN)
           plot_graphic(export_data, substring(date, 1, 4))
         }
  )
  
}

main()
