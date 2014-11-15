#' @title Harmonic and trend predictions from a bfastmonitor object
#' 
#' @description Produce a new data.frame from bfastmonitor objects including harmonic and trend predictions (where applicable). Multiple bfastmonitor objects can be used (see details).
#' 
#' @param bfm Object (or list of objects) of type \code{bfastmonitor}
#' @param type Character. Type of time series on which \code{bfm} is based. Either ``irregular" or ``16-day" (see \code{\link{bfastts}}).
#' @param plotlabs Character. Vector of labels corresponding to objects in \code{bfm}. Can be omitted (automatic labels will be added in that case if \code{bfm} is a list of bfastmonitor objects.)
#' 
#' @return \code{data.frame} with model parameters and predictions according to bfastmontior parameters in \code{bfm}. See details for more info.
#' 
#' @details The main purpose of this function is to prepare a \code{data.frame} to be entered into \code{ggplot}. Rather than simply using the \code{tspp} part of the \code{bfastmonitor} output to plot predicted and observed values, this function 'regularizes' the \code{tspp} output to produce a smooth predicted curve. See \code{\link{bfmPlot}} for a shortcut to creating ggplot objects from bfastmonitor outputs.
#' 
#' @author Ben DeVries and Jan Verbesselt
#' 
#' @examples
#' data(tura_ts3)
#' plot(tura_ts3)
#' 
#' library(bfast)
#' # make tura_ts3 into a 'regular' time series (with NA's)
#' tts <- bfastts(tura_ts3, dates = time2date(time(tura_ts3)), type = "irregular")
#' # run bfastmonitor
#' bfm <- bfastmonitor(tts, start = c(2005, 1), formula = response~harmon, order = 1, history = "all")
#' 
#' # default plot
#' plot(bfm)
#' ## predicted values (blue curve) are only shown where (irregular) observations occur on the time axis
#' 
#' # make a data.frame with 'regular' predictions
#' bfmpred <- bfmPredict(bfm)
#' head(bfmpred)
#' 
#' # simple ggplot
#' library(ggplot2)
#' p <- ggplot(data = bfmpred, aes(x = time))
#' p <- p + geom_line(aes(y = prediction), col = "blue")
#' p <- p + geom_point(aes(y = response), na.rm = TRUE)
#' p <- p + theme_bw()
#' print(p)
#' 
#' # label start of monitoring period with a vertical line
#' # and breakpoint with a red dashed line
#' p <- p + geom_vline(aes(xintercept = start))
#' p <- p + geom_vline(aes(xintercept = breakpoint), col = "red", lty = 2)
#' print(p)
#' 
#' # bfmPlot() is a shortcut to creating such plot objects
#' ?bfmPlot
#' 
#' @import bfast
#' @export

bfmPredict <- function(bfm, type = "irregular", plotlabs = NULL) {
  
  # bfm can either be a bfastmonitor object or a list of them. Check:
  if(class(bfm) == "list") {
    if(!all(sapply(bfm, class) == "bfastmonitor"))
      stop("bfm must be an object or list of objects of class \'bfastmonitor\'.")
  } else if(class(bfm) == "bfastmonitor") {
    bfm <- list(bfm) # convenient to treat everything as a list, regardless of number of bfm objects...
  } else {
    stop("bfm must be an object or list of objects of class \'bfastmonitor\'.")
  }
  
  # facet plot labels
  if(is.null(plotlabs)) {
    plotlabs <- paste("BFM", c(1:length(bfm)), sep="")
  } else if(length(plotlabs) > length(bfm)){
    warning("plotlabs has length greater than length(bfm). Using first ", length(bfm), " elements of plotlabs only.")
    plotlabs <- plotlabs[c(1:length(bfm))]
  }
  
  # make plotlabs into factor
  plotlabs <- factor(plotlabs, levels = plotlabs)
  
  # function to prepare predict data.frame for each object in bfm
  preparedf <- function(xx, pl) {
    
    # reformat dates
    dates <- time2date(xx$tspp$time)
    
    # get values from tspp
    vi <- xx$tspp$response
    
    # make response vectors into a 'regular' time series; also make time vectors from these
    vi <- bfastts(vi, dates, type = type)
    tts <- time(vi)
    
    # determine model order (= # of sine or cosine columns)
    order <- ncol(xx$tspp$harmon) / 2
    
    # define 'new' tspp vectors -- can use bfastpp() or bfastmonitor()$tspp
    # but bfastpp() is simpler - just be sure that order is the same as in original bfm object!
    nd <- bfastpp(tts, order = order)
    
    # predict new values based on bfm lm's
    nd$prediction <- predict(xx$model, nd)
    
    # add additional prediction using only trend component
    nd$harmon[] <- 0
    nd$predictionTrend <- predict(xx$model, nd)
  
    # add breakpoints and magnitude column
    nd$breakpoint <- xx$breakpoint
    nd$magnitude <- xx$magnitude
    
    # add a label column (for facet plotting)
    nd$lab <- pl
    
    # add a column indicating the start of the monitoring period
    nd$start <- xx$monitor[1]
    
    # add original response vectors back to data.frames
    # Note: using the original ts class will cause an error when running rbind() below
    res <- bfastts(xx$tspp$response, dates, type = type)
    nd$response <- as.numeric(res)
    
    # limit columns in nd to only the necessary ones
    nd <- subset(nd, select = -c(trend, season, harmon))
   
    return(nd)
  }
  
  nd <- mapply(bfm, plotlabs, FUN=function(x,y) preparedf(x,y), SIMPLIFY = FALSE)
  allData <- do.call("rbind", nd)
  
  return(allData)
  
}