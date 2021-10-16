library("lubridate")


solve_weeks <- function(weeks_in_year = 52){
  today <- today()
  number_of_week <- week(today)
  answer <- weeks_in_year - number_of_week
  cat('До нового года осталось', answer, "недель", '\n')
}
  
cheak_for_leap_years <- function(){
  today <- today()
  answer <- leap_year(today)
  if (answer)
    cat("Год не високосный")
  else
    cat("Год високосный")
}


solve_weeks()
cheak_for_leap_years()
