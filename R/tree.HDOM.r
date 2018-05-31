#' Calculate dominant height for a plot based on tree-level data.
#'
#' \code{tree.HDOM} Calculate the dominant height for a plot based on tree-level data based on the top 25th percentile
#' of the tree heights. The provided vector of heights should be complete without missing data.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param HT Vector of tree heights (m) for a given plot (must be complete).
#'
#' @return A value with Dominant Height (HDOM, m) for the plot.
#'
#' @examples
#' HT <- tree.HT(DBH=treedata$DBH, HT=treedata$HT, method=2)$HTFIN
#' tree.HDOM(HT=HT)


tree.HDOM  <-  function(HT=NA){

  if(sum(is.na(HT))==0){

    HT <- sort(HT, decreasing = TRUE)
    H75 <- as.vector(stats::quantile(HT, 0.75))
    HT <- subset(HT,HT>=H75)
    HDOM <- mean(HT)

  } else {

    stop("Error: Not enought height measurements to fit model.")

  }

  return(HDOM=HDOM)

}

