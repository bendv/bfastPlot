#' @title ggplot bfastmonitor
#' 
#' @description Generate a ggplot object from a (list of) bfastmonitor object(s)
#' 
#' @import ggplot2
#' @export
#' 
#' @examples
#' # adapted from help page of bfastmonitor
#' 
#' library(bfast)
#' library(ggplot2)
#' 
#' NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))
#' plot(NDVIa)
#' ## apply the bfast monitor function on the data 
#' ## start of the monitoring period is c(2010, 13) 
#' ## and the ROC method is used as a method to automatically identify a stable history
#' mona1 <- bfastmonitor(NDVIa, start = c(2010, 13), formula = response ~ harmon, order = 3)
#' class(mona1)
#' 
#' # regular plot
#' plot(mona1)
#' # ggplot of the same 
#' p <- bfmPlot(mona1)
#' p
#' 
#' ## the advantage of ggplot is that is is object based
#' ## additional layers can simply be added
#' # change to black/white
#' p2 <- p + theme_bw()
#' p2
#' 
#' ## combine several bfastmonitor objects into one facet plot
#' mona2 <- bfastmonitor(NDVIa, start = c(2010, 13), formula = response~harmon, order=2)
#' mona3 <- bfastmonitor(NDVIa, start = c(2010, 13), formula = response~harmon, order=1)
#' p3 <- bfmPlot(list(mona1, mona2, mona3), plotlabs = c("order = 3", "order = 2", "order = 1"))
#' p3 + theme_bw()
#' 
#' # it's not necessary to show the trend when there is none
#' p4 <- bfmPlot(list(mona1, mona2, mona3), 
#' plotlabs = c("order = 3", "order = 2", "order = 1"), 
#' displayTrend = FALSE)
#' p4 + theme_bw()

bfmPlot <- function(bfm, plotlabs = NULL, ncols = 1, rescale = 1, ylab = "response", displayMagn = TRUE, magn_ypos = 0.3, magn_xoffset = -0.45, magn_digits = 3, displayTrend = TRUE, displayResiduals = c("none", "all", "monperiod", "history"), type = "irregular") {
  
  # get predict vector from bfm
  allData <- bfmPredict(bfm, type = type, plotlabs = plotlabs)
  
  # x-axis breaks (yearly integer labels)
  xbks <- c(floor(min(allData$time)):ceiling(max(allData$time)))
  
  # pass data.frame to ggplot()
  p <- ggplot(data=allData, aes(x=time, y=response)) +
    geom_point(na.rm=TRUE) + 
    geom_line(aes(y=prediction), col="blue", na.rm=TRUE) +
    labs(y=ylab) +
    scale_x_continuous(breaks=xbks) +
    geom_vline(aes(xintercept=start), na.rm=TRUE)
    #theme_bw()
  
  if(length(levels(allData$lab) > 1))
    p <- p + facet_wrap(~ lab, ncol=ncols)
    

  if(!all(is.na(unique(allData$breakpoint)))){
    p <- p + geom_vline(aes(xintercept=breakpoint), na.rm=TRUE, col="red", lty=2)
  }
  
  # optional: display linear component
  if(displayTrend){
    p <- p + geom_line(aes(y = predictionTrend), col = "blue", lty = 2, na.rm=TRUE)
  }
  
  # optional: annotate with magnitude on each plot
  if(displayMagn){
    # determine y-position of the label
    magn_ypos <- min(allData$response, na.rm=TRUE) + magn_ypos * diff(range(allData$response, na.rm=TRUE))
    # determine x-position of the label (based on monitoring period)
    magns <- unique(allData$magnitude)
    xpos <- unique(allData$start) + magn_xoffset
    # get magnitude values from all models
    magn <- data.frame(magn=round(magns * rescale, magn_digits),
                       x=xpos,
                       y=magn_ypos,
                       lab=unique(allData$lab))
    
    p <- p + geom_text(data=magn, aes(x=x, y=y, label=paste('m = ', magn, sep=""), group=NULL), 
                       size=5)
  }
  
  # optional: display residuals in monperiod
  if(displayResiduals[1] != "none" & ("monperiod" %in% displayResiduals | "all" %in% displayResiduals)){
    p <- p + geom_segment(data = allData[allData$time >= allData$start, ],
                          aes(x = time, xend = time, y = response, yend = prediction), 
                          col="grey", lty=5, na.rm=TRUE)
  }
  
  # optional: display residuals in history period
  if(displayResiduals[1] != "none" & ("history" %in% displayResiduals | "all" %in% displayResiduals)){
    p <- p + geom_segment(data = allData[allData$time < allData$start, ],
                          aes(x = time, xend = time, y = response, yend = prediction), 
                          col="grey", lty=5, na.rm=TRUE)
  }
  
  return(p)
}