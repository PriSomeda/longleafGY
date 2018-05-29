#' Estimates the total height of trees for a single plot.
#'
#' \code{tree.HT} Estimates the total height of trees that have missing height from tree-level data.
#' For the missing trees there are two methods to use: 1) Estimates heights according
#' to a parametrized DBH-height model, or 2) Estimates heights by fitting a simple DBH-height model that requires at least
#' 10 measurements. Missing values are indentified as 'NA'.
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param DBH    Vector of diameter at breast height (DBH, cm). Must be complete and have the same size and order as TREEID.
#' @param HT     Vector of total height (m). Must be of the same size and order as TREEID.
#' @param AREA   Numeric value of area of the inventory plot (m2).
#' @param AGE    Numeric value of stand age (years). Required if method = 1.
#' @param BA     Numeric value of Basal Area (m2/ha). Required if method = 1.
#' @param method Numeric value that identifies the method to estimate missing heights. 1: parametrized DBH-height model
#' that requires DBH, BA and AGE, 2: fits a simple DBH-height model from available measurements using the equation:
#' ln(HT) = b0 + b1/DBH. Default method = 2.
#'
#' @return A list containing the following:
#' \itemize{
#' \item \code{HTFIN} A vector of final tree heigths (m), replacing the missing values for estimated heigths and retaining the observed heights.
#' \item \code{r2} A value with the coefficient of determination from the fitting the DBH-height model when method = 2.
#' }
#'
#' @references
#' Gonzalez-Benecke et al. (2013) - Individual Tree Diameter, Height, and Volume Functions for Longleaf Pine.
#' Forest Science 60(1):43-56; doi:10.5849/forsci.12-074
#'
#' @examples
#' # Example 1 - Method 1 - Parametrized DBH-height model
#' DBH <- c(9.3,11.1,15.5,9,14.8,27.3,11.4,6.6,12.6,17.5,6.3,7.2,11.5,13.6,7.3,12,11.9,8.1,7.6,5)
#' HT <- c(11.8,12.3,NA,NA,15.3,18,12,NA,14.5,NA,NA,NA,NA,NA,10.3,14.6,NA,NA,NA,NA)
#' tree.HT(DBH=DBH, HT=HT, AREA=200, AGE=47, method=1)
#'
#' # Example 2 - Method 2 - Simple DBH-height model
#' DBH <- c(9.3,11.1,15.5,9,14.8,27.3,11.4,6.6,12.6,17.5,6.3,7.2,11.5,13.6,7.3,12,11.9,8.1,7.6,5)
#' HT <- c(11.8,12.3,NA,NA,15.3,18,12,NA,14.5,NA,NA,NA,NA,NA,10.3,14.6,NA,NA,NA,NA)
#' tree.HT(DBH=DBH, HT=HT, method=2)


tree.HT  <-  function(DBH, HT, AREA=NA, AGE=NA, BA=NA, method=2){

                        if(length(DBH)==length(HT) & sum(is.na(DBH))==0){

                          if(method==1){

                            if(is.na(AGE)==T | is.na(AREA)==T && is.na(BA)==T){
                              stop("Warning - Incomplete information. Please check input.")
                            }

                            if(is.na(AGE)==F){

                              if(is.na(AREA)==F & is.na(BA)==T){
                                BA <- tree.STAND(DBH=DBH, AREA=AREA)$BA
                              }

                              a1 <- 0.059425
                              a2 <- -10.803775
                              a3 <- -1.127503
                              a4 <- 0.150532
                              a5 <- 0.121239
                              aux <- data.frame(DBH=DBH, HT=HT, HTEST=NA, HTFIN=NA)
                              aux$HTEST <- exp(a1 + (a2*(aux$DBH^a3)) + (AGE^a4) + (BA^a5))
                              aux$HTFIN <- ifelse(is.na(aux$HT),aux$HTEST,aux$HT)
                              r2 <- NA

                            }

                          }

                          if(method==2){

                            aux <- data.frame(DBH=DBH, HT=HT)
                            aux2 <-subset(aux, HT!='NA')
                            model <- stats::lm(log(aux2$HT)~I(1/aux2$DBH),na.action='na.omit')
                            r2 <- summary(model)$r.squared
                            HTEST <- exp(model$coefficients[1]+model$coefficients[2]/DBH)
                            aux$HTFIN <- ifelse(is.na(aux$HT),HTEST,aux$HT)

                          }

                        } else {

                          stop("Warning - Incomplete information. Please check input.")

                          }

                  return(list(HTFIN=aux$HTFIN, r2=r2))

}
