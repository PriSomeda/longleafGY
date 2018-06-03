#' Predicts the number of trees (mortality) at next age based on stand-level information.
#'
#' \code{module.N} Estimates the number of trees (mortality) from initial age (AGE0) to next age (AGE1) based on stand level
#' information based on the equation:
#' N1 = N0 x exp((c1x(HDOM0/100)+c2xSDIR) x ((AGE1^c3)-(AGE0^c3)))
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param N0     Numeric value of number of trees per hectare at age 0.
#' @param HDOM0  Numeric value of Dominant Height (m) at age 0.
#' @param SDIR0  Numeric value of Relative Stand Density Index for Longleaf (\%) at age 0.
#' @param AGE0   Numeric value of intial stand age or age 0 (years).
#' @param AGE1   Numeric value of final stand age or age 1 for prediction (years).
#'
#' @return A value with the number of trees per hectare at age 1 (N1).
#'
#' @references
#' Gonzalez-Benecke et al. (2012) - Modeling Survival, Yield, Volume Partitioning and Their Response to Thinning for Longleaf Pine Plantations.
#' Forests, 3(4), 1104-1132; doi:10.3390/f3041104
#'
#' @examples
#' module.N(N0=2500, HDOM0=14, SDIR0=45, AGE0=24, AGE1=25)$N1


module.N  <-  function(N0=NA, HDOM0=NA, SDIR0=NA, AGE0=NA, AGE1=NA){

  # N1 = N0*exp((b1*HDOM0)((AGE1^b2)-(AGE0^b2)))  # Simpler alternative model
  b1 <- -0.0015002
  b2 <- 0.8635401

  # N1 = N0*exp((c1*(HDOM0/100)-c2*SDIR)*((AGE1^c3)-(AGE0^c3)))
  c1 <- 0.0087247
  c2 <- -0.0117265
  c3 <- 1.2543404

  # 1. If all informations are provided
  if(is.na(N0)==FALSE && is.na(HDOM0)==FALSE && is.na(AGE0)==FALSE &&
     is.na(AGE1)==FALSE && is.na(SDIR0)==FALSE){

    #N1 <- N0*exp((b1*HDOM0)*((AGE1^b2)-(AGE0^b2)))  # Alternative model
    N1 <- N0*exp((c1*(HDOM0/100)+c2*SDIR0/100)*((AGE1^c3)-(AGE0^c3)))

  } else {
    stop("Warning - Please provide information required.")

  }

  return(list(N1=N1))

}
