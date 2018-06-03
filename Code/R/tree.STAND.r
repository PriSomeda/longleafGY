#' Calculates the basal area, number of trees and mean quadratic diameter for a plot based on tree-level data.
#'
#' \code{tree.STAND} Calculates the basal area, number of trees and mean quadratic diameter for a plot based on tree-level data.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param DBH   Vector of diameter at breast height (DBH, cm). Must be complete
#' @param AREA  Numeric value of the size of the plot (m2).
#'
#' @return A list containing the parameters:
#' \itemize{
#' \item \code{BA} Basal Area (m2/ha).
#' \item \code{N}  Number of trees per hectare.
#' \item \code{QD} Mean Quadratic Diameter (cm).
#' }
#'
#' @examples
#' # Example - Stand information from inventory data
#' DBH <- treedata$DBH[!is.na(treedata$DBH)]
#' tree.STAND(DBH=DBH, AREA=301)


tree.STAND  <-  function(DBH, AREA){

  if(AREA!=0){

    EF <- 10000/AREA   # Expansion factor
    BA <- (sum(((pi/4)*(DBH)^2)*0.0001))*EF
    N <- length(DBH)*EF
    QD <- (sqrt((4/pi)*(BA/N))*100)

  } else {
    stop("Error: Not enought height measurements to fit model.")
  }

  return(list(BA=BA, N=N, QD=QD))

}
