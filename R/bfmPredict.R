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