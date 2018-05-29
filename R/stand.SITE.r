#' Calculates dominant height, site index and stand age for a plot.
#'
#' \code{stand.SITE} Calculates dominant height, site index (at reference age 50 year) or stand age for a plot 
#' based on the equation following dominant height equation: 
#' HDOM = SI x ((1-exp(a1 x Age)) / (1-exp(a1 x 50)))^a2
#' The definition of HDOM is based on the top 25th percentile of the tree heights. 
#' At least two parameters are required to calculate the thrid missing parameter.   
#' 
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#' 
#' @param HDOM Numeric value of mean Dominant Height (m).
#' @param SI   Numeric value of Site Index (m) (Dominant Height of the plot at age 50 years).
#' @param AGE  Numeric value of stand Age (years).
#'
#' @return A list containing the parameters: 
#' \itemize{
#' \item \code{HDOM} Dominant Height (m).
#' \item \code{SI}   Site Index (m).
#' \item \code{AGE}  Age (years).
#' }
#' 
#' @references
#' Gonzalez-Benecke et al. (2012) - Modeling Survival, Yield, Volume Partitioning and Their Response to Thinning for Longleaf Pine Plantations.
#' Forests, 3(4), 1104-1132; doi:10.3390/f3041104  
#'
#' @examples
#' stand.SITE(SI=30,AGE=40)$HDOM     # Missing HDOM
#' stand.SITE(HDOM=27.5,AGE=40)$SI   # Missing SI
#' stand.SITE(HDOM=27.5,SI=33.3)$AGE # Missing AGE


stand.SITE  <-  function(HDOM=NA, SI=NA, AGE=NA){
                      
                      # Hdom=SI*((1-exp(a1*Age))/(1-exp(a1*50)))^a2
                      a1 <- -0.0369815
                      a2 <- 1.2928702 
  
                      # 1. If all informations are provided
                      if(is.na(HDOM)==F && is.na(SI)==F && is.na(AGE)==F){
                        print("Warning - No parameteres to estimated. You already have all information.")
                      } 
  
                      # 2. If HDOM is missing
                      if(is.na(HDOM)==T && is.na(SI)==F && is.na(AGE)==F){
                        HDOM <- SI*((1-exp(a1*AGE))/(1-exp(a1*50)))^a2 
                      } 
                      
                      # 3. If SI is missing
                      if(is.na(HDOM)==F && is.na(SI)==T && is.na(AGE)==F){
                        SI <- (HDOM/((1-exp(a1*AGE))/(1-exp(a1*50)))^a2)
                      } 
                      
                      # 4. If AGE is missing
                      if(is.na(HDOM)==F && is.na(SI)==F && is.na(AGE)==T){
                        min.error <- 10000
                        aux <- data.frame(x=seq(1,100,0.01), error=NA)
                        for(j in 1:length(aux$x)){
                          aux$error[j] <- abs(HDOM-(SI*((1-exp(a1*aux$x[j]))/(1-exp(a1*50)))^a2))
                          if(aux$error[j] < min.error){
                              min.error <- aux$error[j]
                              AGE <- aux$x[j]
                            } 
                          }
                      } 
                    
                      # 5. If the minimum informations are provided
                      if(is.na(HDOM)==T && is.na(SI)==T && is.na(AGE)==T | 
                         is.na(HDOM)==F && is.na(SI)==T && is.na(AGE)==T |
                         is.na(HDOM)==T && is.na(SI)==F && is.na(AGE)==T |
                         is.na(HDOM)==T && is.na(SI)==T && is.na(AGE)==F){
                        stop("Warning - Please provide information required.")
                      } 
                      
                      return(list(HDOM=HDOM, SI=SI, AGE=AGE))
                      
               } 
