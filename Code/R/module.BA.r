#' Predicts and projects stand basal area based on stand-level information.
#'
#' \code{module.BA} Predicts and projects stand basal area based on number of trees per hectare and
#' dominant height at the stand-level using the equation: ln(BA) = c1 + c2 x ln(N) + c3 x ln(HDOM)
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param N0          Numeric value of number of trees per hectare at age 0 (or initial age).
#' @param HDOM0       Numeric value of Dominant Height (m) at age 0.
#' @param projection  If TRUE then model projection from provided BA0 is executed for a 1 year increment. Default: FALSE.
#' @param BA0         Numeric value of Basal Area (m2/ha) at age 0 (required for model projection).
#' @param N1          Numeric value of number of trees per hectare at age 1 for projection.
#' @param HDOM1       Numeric value of Dominant Height (m) at age 1 for projection.
#'
#' @return A list containing:
#' \itemize{
#' \item \code{BA0} Predicted Basal Area at age 0 (m2/ha).
#' \item \code{BA1} Projected Basal Area at age 1 (m2/ha).
#' }
#'
#' @references
#' Gonzalez-Benecke et al. (2012) Modeling Survival, Yield, Volume Partitioning and Their Response to Thinning for Longleaf Pine Plantations.
#' Forests 3(4), 1104-1132; doi:10.3390/f3041104
#'
#' @examples
#' # Example 1 - Predicting BA
#' module.BA(N0=1200, HDOM0=17.7)$BA0
#'
#' # Example 2 - Projecting BA
#' module.BA(N0=1200, HDOM0=17.7, projection=TRUE, BA0=24, N1=1182, HDOM1=18.9)$BA1
#'
#' # Example 3 - Varifying projection with prediction
#' sim.test <- module.input(TYPE='PLOT', BA0=17.63402, HDOM=14, AGE0=17, N0=1200, AGEF=28)
#' module.simulation(stand=sim.test)$sim.stand                   # All projections
#' module.BA(N0=1200, HDOM0=14, projection=FALSE)$BA0            # Prediction Age 17
#' module.BA(N0=1185.363, HDOM0=14.75542, projection=FALSE)$BA0  # Prediction Age 18
#' module.BA(N0=1019.661, HDOM0=21.26901, projection=FALSE)$BA0  # Prediction Age 28

module.BA  <-  function(N0=NA, HDOM0=NA, projection=FALSE, BA0=NA, N1=NA, HDOM1=NA){

  # ln(BA)=c1+c2*ln(N)+c3*ln(Hdom)
  c1 <- -4.6484039
  c2 <- 0.4452486
  c3 <- 1.6526307

  if(is.na(N0)==TRUE && is.na(HDOM0)==TRUE |
     is.na(N0)==TRUE && is.na(HDOM0)==FALSE |
     is.na(N0)==FALSE && is.na(HDOM0)==TRUE){
    stop("Warning - Please provide information required.")
  }

  # Prediction
  else if (projection==FALSE){

    BA0 <- exp(c1+c2*log(N0)+c3*log(HDOM0))
    BA1 <- NA

  }

  # Projection
  else if (projection==TRUE){

    if(is.na(BA0)==TRUE | is.na(N1)==TRUE | is.na(HDOM1)==TRUE){
      stop("Warning - Please provide information required for projection.")
    }

    BA1 <- BA0*(1+c2*(N1-N0)/N0+c3*(HDOM1-HDOM0)/HDOM0)   # Linear model with approx. deriv.
    #BA1 <- BA0*exp(c2*(N1-N0)/N0+c3*(HDOM1-HDOM0)/HDOM0) # Exp. model with approx. deriv.

  }

  return(list(BA0=BA0, BA1=BA1))

}
