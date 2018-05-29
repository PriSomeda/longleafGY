#' Data from inventory at tree-level to test
#'
#' A dataset containing the measurements at tree-level of 122 trees in the same plot.
#'
#' @format A data frame with 122 observations and 5 variables:
#' \describe{
#' \itemize{
#' \item \code{PLOTID} Unique plot identification. For a single plot, all trees should have the same PLOTID.
#' \item \code{TREEID} Unique tree identification. Must be complete.
#' \item \code{DBH} Diameter at breast height (DBH, in). Must ve the same size and order as TREEID.
#' \item \code{HT} Total height (ft). Must be of the same size and order as TREEID.
#' \item \code{OBS} Aditional information about the tree.
#' }}
#'
#' @source \url{http://www.sfrc.ufl.edu/CFGRP/}

"treedata"
