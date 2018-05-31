#' Calculates relative stand density index for Longleaf Pine (\emph{Pinus palustris})
#'
#' \code{stand.SDI} Calculates the relative stand density index (RSDI, \%) for Longleaf in a single plot using the
#' expression: RSDI = 100xNx(QD/25.4)^1.605/SDImax. For Longleaf pine (\emph{Pinus palustris}) SDImax is 1200 trees/ha.
#' At least two parameters are required to complete the missing.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param N   Numeric value of the number of trees per hectare.
#' @param QD  Numeric value of mean quadratic diameter (cm).
#'
#' @return A value for the relative stand density index (RSDI, \%).
#'
#' @examples
#' stand.SDI(QD=18, N=1660)


stand.SDI <- function(N=NA, QD=NA){

  # If the minimum informations are provided
  if(is.na(N)==TRUE && is.na(QD)==TRUE |
     is.na(N)==FALSE && is.na(QD)==TRUE |
     is.na(N)==TRUE && is.na(QD)==FALSE){
    stop("Warning - Please provide information required.")
  }

  SDImax <- 1200  # trees/ha for longleaf
  SDI <- N*(QD/25.4)^1.605
  SDIR <- 100*SDI/SDImax

  return(SDIR=SDIR)
}
