#' Module of input tree- or stand-level data to prepare it for further simulations.
#'
#' \code{module.input} Prepares tree- or stand-level data from a single plot, checks and completes missing values, and 
#' calculates several stand-level parameters including total volume. It also reads required information for further 
#' simulations including simulation age and details of future thinning. Some information is only traspassed to other modules.
#' Note that form tree-level data individual tree (complete or incomplete) information is required. 
#'
#' @export
#' @author Priscila Someda-Dias, Salvador A. Gezan
#'
#' @param TYPE      Character for type of input data. PLOT: stand-level data information, TREE: tree-level information. Default is PLOT
#' @param TREEDATA  Data frame with tree-level information with columns: PLOTID, TREEID, DBH, HT (these should be identical names).
#' @param AREA      Numeric value of size of the inventory plot (m2). Required for TYPE='TREE'. 
#' @param SI        Numeric value of Site Index (m) (Dominant Height of the plot at age 50 years).
#' @param HDOM0     Numeric value of Dominant Height (m) at initial age (or age 0).  
#' @param AGE0      Numeric value of initial stand age or age 0 (years).
#' @param BA0       Numeric value of Basal Area (m2/ha) at age 0 (required for model projection).
#' @param N0        Numeric value of number of trees per hectare at age 0.  
#' @param AGEF      Numeric value of final stand age (in years) of simulation. Default is 50.
#' @param THINNING  If TRUE then a thinning is implemented according to AGET and BAR. Default is FALSE.
#' @param AGET      Numeric value of stand age (in years) where thinning is planned.
#' @param BAR       Numeric value of Relative Basal Area (\%, 0-1) to be removed when thinning at age AGET.
#' @param t         Numeric value top stem diameter outside bark for merchantability limit (cm).
#' @param d         Numeric value of a DBH threshold limit for merchantable trees (cm).
#' @param method    Numeric value that identifies the method to estimate missing heights from TYPE='TREE'.
#' 1: parametrized DBH-height model that requires DBH, BA and AGE, 2: fits a simple DBH-height model from 
#' available measurements using the equation: ln(Ht) = b0 + b1/DBH. Default method=2.
#'  
#' @return A list containing the following: 
#' \itemize{   
#' \item \code{SI}       Site Index (m).
#' \item \code{AGE0}     Initial stand age or age 0 (years).
#' \item \code{HDOM0}    Dominant Height (m) at initial age (or age 0). 
#' \item \code{BA0}      Basal Area (m2/ha) at age 0.
#' \item \code{N0}       Number of trees per hectare at age 0. 
#' \item \code{QD}       Mean quadratic diameter (cm) at age 0.
#' \item \code{SDIR0}    Relative stand density index (\%) at age 0.
#' \item \code{VOL_OB0}  Total stand-level volume outside bark (m3/ha) at age 0.
#' \item \code{VOL_IB0}  Total stand-level volume inside bark (m3/ha) at age 0.
#' \item \code{VOLm_OB0} Merchantable stand-level volume outside bark (m3/ha) at age 0.
#' \item \code{VOLm_IB0} Merchantable stand-level volume inside bark (m3/ha) at age 0.
#' \item \code{AGEF}     Final stand age (in years) of simulation.
#' \item \code{THINNING} Logical that indicates if thinning is implemented according to AGET and BAR.
#' \item \code{AGET}     Stand age (in years) where thinning is planned.
#' \item \code{BAR}      Relative Basal Area (\%, 0-1) to be removed when thinning at age AGET.
#' \item \code{t}        Top stem diameter outside bark for merchantability limit (cm).
#' \item \code{d}        DBH threshold limit for merchantable trees (cm).
#' \item \code{method}   Selection of the method to estimate missing heights from TYPE='TREE'.
#' }
#'                          
#' @examples
#' # Example 1 - Input stand-level data
#' module.input(TYPE='PLOT', BA0=17.3, SI=30, AGE0=17, N0=1200, AGEF=18)
#' module.input(TYPE='PLOT', BA0=17.3, HDOM0=16, AGE0=17, N0=1200, AGEF=18)
#' module.input(TYPE='PLOT', HDOM0=16, AGE0=17, N0=1200, AGEF=18)  # BA obtained by prediction
#' 
#' # Example 2 - Input with individual tree data
#' module.input(TYPE='TREE', TREEDATA=treedata, AREA=500, AGE0=23, AGEF=32)
        
     
module.input <-  function(TYPE='PLOT', TREEDATA=NA, AREA=NA, 
                          SI=NA, HDOM0=NA, AGE0=NA, BA0=NA, N0=NA, 
                          AGEF=50, THINNING='FALSE', AGET=NA, BAR=NA, t=5, d=15, method=2){
  
                  # Gathering stand-level information.
                  if(TYPE=='TREE'){
    
                      tree <- prepare.tree(TREEID=TREEDATA$TREEID, DBH=TREEDATA$DBH, HT=TREEDATA$HT, AREA=AREA, AGE=AGE0)
                      BA0 <- tree$BA
                      N0 <- tree$N
                      HDOM0 <- tree$HDOM
                      
                      if(is.na(SI)==T){
                        SI <- stand.SITE(HDOM=HDOM0,AGE=AGE0)$SI    # Missing SI
                      }
                      if(is.na(AGE0)==T){
                        AGE0 <- stand.SITE(HDOM=HDOM0,SI=SI)$AGE     # Missing AGE
                      }
                    
                      # If SI is missing and AGE is provided.
                        if(is.na(SI)==T &&  is.na(AGE0)==F){
                          SI <- tree$SI
                        } 
                      
                      # If AGE is missing and SI is provided.
                        if(is.na(SI)==F &&  is.na(AGE0)==T){
                          AGE0 <- tree$AGE
                        } 
                        
                      # Completing QD and Volume
                      QD0 <- stand.STAND(BA=BA0, N=N0)$QD   # Missing QD (this is always missing)
                      SDIR0 <- stand.SDI(QD=QD0, N=N0)
                      
                      VOL_OB0 <- module.VOL(N=N0, BA=BA0, AGE=AGE0, SI=SI)$VOL_OB
                      VOL_IB0 <- module.VOL(N=N0, BA=BA0, AGE=AGE0, SI=SI)$VOL_IB
                      VOLm_OB0 <- module.VOLm(N=N0, QD=QD0, t=t, d=d, VOL_OB=VOL_OB0, VOL_IB=VOL_IB0)$VOLm_OB
                      VOLm_IB0 <- module.VOLm(N=N0, QD=QD0, t=t, d=d, VOL_OB=VOL_OB0, VOL_IB=VOL_IB0)$VOLm_IB 
                      
                  }
                  
                  # Completing stand-level information
                  if (TYPE=='PLOT'){
    
                        # Completing Site information
                        if(is.na(HDOM0)==T){
                           HDOM0 <- stand.SITE(SI=SI,AGE=AGE0)$HDOM     # Missing HDOM
                        }
                        if(is.na(SI)==T){
                           SI <- stand.SITE(HDOM=HDOM0,AGE=AGE0)$SI    # Missing SI
                        }
                        if(is.na(AGE0)==T){
                           AGE0 <- stand.SITE(HDOM=HDOM0,SI=SI)$AGE     # Missing AGE
                        }
    
                        # Predicting BA0 by model (if not provided)
                        if(is.na(BA0)==T){
                            BA0 <- module.BA(N0=N0, HDOM0=HDOM0, projection=FALSE)$BA0
                        }                    
    
                        # Completing QD and Volume
                        QD0 <- stand.STAND(BA=BA0, N=N0)$QD   # Missing QD (this is always missing)
                        SDIR0 <- stand.SDI(QD=QD0, N=N0)
                        
                        VOL_OB0 <- module.VOL(N=N0, BA=BA0, AGE=AGE0, SI=SI)$VOL_OB
                        VOL_IB0 <- module.VOL(N=N0, BA=BA0, AGE=AGE0, SI=SI)$VOL_IB
                        VOLm_OB0 <- module.VOLm(N=N0, QD=QD0, t=t, d=d, VOL_OB=VOL_OB0, VOL_IB=VOL_IB0)$VOLm_OB
                        VOLm_IB0 <- module.VOLm(N=N0, QD=QD0, t=t, d=d, VOL_OB=VOL_OB0, VOL_IB=VOL_IB0)$VOLm_IB 
                                                
  }
    
  return(list(SI=SI, HDOM0=HDOM0, AGE0=AGE0, BA0=BA0, N0=N0,
              QD0=QD0, SDIR0=SDIR0, VOL_OB0=VOL_OB0, VOL_IB0=VOL_IB0,
              VOLm_OB0=VOLm_OB0, VOLm_IB0=VOLm_IB0, 
              AGEF=AGEF, THINNING=THINNING, AGET=AGET, BAR=BAR, 
              t=t, d=d, method=method))
  
}
