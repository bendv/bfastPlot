#' @title Harmonic and trend predictions for bfast-derived ts segments
#' 
#' @description Produce a new data.frame from bfast-dervied segments including harmonic and trend predictions (where applicable) for each segment.
#' 
#' @param x Numeric. Numeric response time series vector
#' @param dates Date. Acquisition dates of \code{x}
#' @param type Character. Type of time series ("irregular" for Landsat-type times series or "16-day" for 16-day MODIS time series). See \code{\link{bfastts}}
#' @param ... Arguments to be passed to \code{\link{breakpoints}}
#' 
#' @return \code{data.frame} with model parameters and predictions per segment. See details for more info.
#' 
#' @details 
#' 
#' @author Ben DeVries
#' 
#' @import strucchange
#' 

bfastSegPredict <- function(x, dates, ...) {
  
  #### Pre-processing
  
  # strip x to a vector (e.g. in case a zoo is supplied)
  x <- as.numeric(x)
  
  # reformat dates if needed (should be in yyyy-mm-dd for bfastts)
  if(is.numeric(dates)) {
    year <- floor(dates)
    jday <- round((dates - year) * 365, 0)
    dates <- as.Date(paste(year, jday, sep = ''), format = '%Y%j')
  }
  
  # make regular bfast-type time series
  bts <- bfastts(x, dates, type = type)
  
  # bfast pre-processing
  bpp <- bfastpp(bts, order=order)
  
  
  #### Breakpoints
  
  # compute breakpoints if necessary and assign segments
  if(is.null(bp)) {
    
    bp <- breakpoints(response ~ time, data = bpp, ...)
    bpp$segment <- breakfactor(bp)
    bd <- bpp$time[bp$breakpoints]
    
  } else if(bp[1] != 0) {
    
    # reformat bp to numeric if needed
    if(!is.null(bp) & class(bp) != 'numeric') {
      bp <- as.numeric(format(bp, format = '%Y')) + as.numeric(format(bp, format = "%j"))/365
      bd <- bp
    } else if(class(bp) == 'numeric' & bp[1] != 0) {
      bd <- bp
    }
    
    seg <- paste('segment', c(1:(length(bp) + 1)), sep = '')
    bpp$segment <- seg[1]
    for(i in 2:length(seg)) {
      bpp$segment[bpp$time > bp[i-1]] <- seg[i]
    }
    
  } else if(bp[1] == 0) {
    
    bpp$segment <- 'segment1'
    bp <- NULL
    bd <- NULL
    
  }
  
  
  #### Models
  
  # OLS or robust lm
  model <- model[1]
  
  if(length(unique(bpp$segment)) > 1) {
    
    # check formula (only response ~ harmon or response ~ harmon + trend allowed)
    if(all(as.character(formula) == c("~", "response", "harmon"))) {
      formula <- response ~ segment/harmon
    } else if(all(as.character(formula) == c("~", "response", "harmon + trend")) | all(as.character(formula) == c("~", "response", "trend + harmon"))) {
      formula <- response ~ segment/(harmon+trend)
    } else {
      stop('only harmon and harmon+trend models supported at this time.')
    }
    
    # check fit (only lm or rlm allowed)
    if(model == 'lm') {
      m <- lm(formula, data=bpp)
    } else if(model == 'rlm') {
      m <- rlm(formula, data=bpp)
    } else {
      stop("model must be either \'lm\' or \'rlm\'.")
    }
    
  } else {
    
    if(model == 'lm') {
      m <- lm(formula, data=bpp)
    } else if(model == 'rlm') {
      m <- rlm(formula, data=bpp)
    } else {
      stop("model must be either \'lm\' or \'rlm\'.")
    }
    
  }
  
}