#' @title Convert decimal date to yyyy-mm-dd
#' 
#' @description Convert decimal date to yyyy-mm-dd
#' 
#' @param time Numeric. Time in decimal format (e.g. 2005.258)
#' 
#' @return Object of class \code{Date}
#' 
#' @examples
#' time2date(2005.259)
#' 
#' @export

time2date <- function(time){
  year <- floor(time)
  jday <- round((time - year) * 365 + 1)
  jday <- formatC(jday, width = 3, flag = 0)
  dates <- as.Date(paste(year, jday, sep=""), format="%Y%j")
  return(dates)
}