#' Calculates the total stand-level volume based on stand-level information.
#'
#' \code{module.VOL} Calculates the total stand-level volume inside and outside bark for a plot
#' based on the equation: ln(VOL) = d1 + d2xln(N) + d3xln(BA) + d4xln(BA/AGE) + d5xln(SI)
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param N     Numeric value of number of trees per hectare.
#' @param BA    Numeric value of Basal Area of the plot (m2/ha).
#' @param AGE   Numeric value of Age of the plot (in years).
#' @param SI    Numeric value of Site Index (m) (Dominant Height of the plot at age 50 years).
#'
#' @return A list containing the parameters:
#' \itemize{
#' \item \code{VOL_OB} Total stand-level volume outside bark (m3/ha).
#' \item \code{VOL_IB} Total stand-level volume inside bark (m3/ha).
#' }
#'
#' @references
#' Gonzalez-Benecke et al. (2012) - Modeling Survival, Yield, Volume Partitioning and Their Response to Thinning for Longleaf Pine Plantations.
#' Forests, 3(4), 1104-1132; doi:10.3390/f3041104
#'
#' @examples
#' module.VOL(N=2500, BA=30, AGE=25, SI=15)


module.VOL  <-  function(N=NA, BA=NA, AGE=NA, SI=NA){

  # ln(VOL) = d1+d2*ln(N)+d3*ln(BA)+d4*ln(BA/AGE)+d5*ln(SI)

  # Outside bark estimated parameters
  d1_OB <- 3.1110579
  d2_OB <- -0.1406022
  d3_OB <- 1.1826310
  d4_OB <- -2.4435259
  d5_OB <- -0.0782880

  # Inside bark estimated parameters
  d1_IB <- 3.0888853
  d2_IB <- -0.1943861
  d3_IB <- 1.2580580
  d4_IB <- -3.1281571
  d5_IB <- -0.098259

  # 1. If all informations are provided OR 2. else
  if(is.na(N)==FALSE && is.na(BA)==FALSE && is.na(AGE)==FALSE && is.na(SI)==FALSE){
    VOL_OB <- exp(d1_OB+d2_OB*log(N)+d3_OB*log(BA)+d4_OB*(log(BA)/AGE)+d5_OB*log(SI))
    VOL_IB <- exp(d1_IB+d2_IB*log(N)+d3_IB*log(BA)+d4_IB*(log(BA)/AGE)+d5_IB*log(SI))
  } else {
    VOL_OB <- NA
    VOL_IB <- NA
    stop("Warning - Please provide information required.")
  }

  return(list(VOL_OB=VOL_OB, VOL_IB=VOL_IB))

}
