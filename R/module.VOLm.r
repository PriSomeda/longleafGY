#' Calculates the stand-level merchantable volume based on stand-level information.
#' 
#' \code{module.VOLm} Calculates the stand-level merchantable volume inside and outside bark fro a plot 
#' based on the equation:  VOLm = VOL x exp(m1 x ((t/QD)^m2) + m3 x (N^m4) x (d/QD)^m5)
#' 
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#' 
#' @param N       Numeric value of number of trees per hectare. 
#' @param QD      Numeric value of mean Quadratic Diameter (cm).
#' @param t       Numeric value top stem diameter outside bark for merchantability limit (cm).
#' @param d       Numeric value of a DBH threshold limit for merchantable trees (cm).
#' @param VOL_OB  Total stand-level volume outside bark (m3/ha).
#' @param VOL_IB  Total stand-level volume inside bark (m3/ha).
#'
#' @return A list containing the parameters: 
#' \itemize{
#' \item \code{VOLm_OB} Merchantable stand-level volume outside bark (m3/ha).
#' \item \code{VOLm_IB} Merchantable stand-level volume inside bark (m3/ha).
#' }
#' 
#' @seealso 
#' \code{\link{module.VOL}}
#' 
#' @references 
#' Gonzalez-Benecke et al. (2012) - Modeling Survival, Yield, Volume Partitioning and Their Response to Thinning for Longleaf Pine Plantations.
#' Forests, 3(4), 1104-1132; doi:10.3390/f3041104  
#'
#' @examples
#' VOL_OB<-module.VOL(N=2500, BA=30, AGE=25, SI=15)$VOL_OB
#' VOL_IB<-module.VOL(N=2500, BA=30, AGE=25, SI=15)$VOL_IB
#' QD<-stand.STAND(BA=30, N=2500)$QD
#' module.VOLm(N=2500, QD=QD, t=3, d=5, VOL_OB=VOL_OB, VOL_IB=VOL_IB)


module.VOLm  <-  function(N=NA, QD=NA, t=NA, d=NA, VOL_OB=NA, VOL_IB=NA){
                      
                      # VOLm = VOL*exp(m1*((t/QD)^m2)+m3*(N^m4)*(d/QD)^m5)
                    
                      # Ouside bark estimated parameters
                      m1_OB <- -1.0385828 
                      m2_OB <- 4.2526170
                      m3_OB <- -0.6266850
                      m4_OB <- -0.1246646
                      m5_OB <- 9.1649608
                      
                      # Inside bark estimated parameters  
                      m1_IB <- -1.0537628 
                      m2_IB <- 4.2527499
                      m3_IB <- -0.6545719
                      m4_IB <- -0.1365633
                      m5_IB <- 9.3108306
                      
                      # 1. If all informations are provided
                      if(is.na(N)==F && is.na(QD)==F && is.na(t)==F && 
                         is.na(d)==F && is.na(VOL_OB)==F && is.na(VOL_IB)==F){
                      
                        VOLm_OB <- VOL_OB*exp(m1_OB*((t/QD)^m2_OB)+m3_OB*(N^m4_OB)*(d/QD)^m5_OB)
                        VOLm_IB <- VOL_IB*exp(m1_IB*((t/QD)^m2_IB)+m3_IB*(N^m4_IB)*(d/QD)^m5_IB)
                      
                      } 
                      
                      # 2. If only volume outside of bark is provided
                      else if(is.na(N)==F && is.na(QD)==F && is.na(t)==F &&
                              is.na(d)==F && is.na(VOL_OB)==F && is.na(VOL_IB)==T){
                      
                        VOLm_OB <- VOL_OB*exp(m1_OB*((t/QD)^m2_OB)+m3_OB*(N^m4_OB)*(d/QD)^m5_OB)
                        VOLm_IB <- NA
                        print("Warning - Only total volume outside bark provided.")
                        
                      } 
                      
                      # 3. If only volume inside of bark is provided
                      else if(is.na(N)==F && is.na(QD)==F && is.na(t)==F && 
                              is.na(d)==F && is.na(VOL_OB)==T && is.na(VOL_IB)==F){
                        
                        VOLm_IB <- VOL_IB*exp(m1_IB*((t/QD)^m2_IB)+m3_IB*(N^m4_IB)*(d/QD)^m5_IB)
                        VOLm_OB <- NA
                        print("Warning - Only total volume inside bark provided.")

                      } 
                      
                      # 4. If any informations is missing
                      else {               
                        stop("Please provide information required for calculating merchantable volume.")
                      } 
                      
                 return(list(VOLm_OB=VOLm_OB, VOLm_IB=VOLm_IB))
                      
} 