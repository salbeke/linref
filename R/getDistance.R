#' Biased cross-validation bandwitdh matrix selector for bivariate data.
#'
#' A simple wrapper for the ks::Hbcv function.
#'
#' @param p1 The first point
#' @param p2 The second point
#' @return A numeric vector of estimated x and y bandwidths. Must subset your data if you wish to obtain group specific bandwidths.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#' @export
#' @examples
#' data("rodents")
#' # Subset the data for a single species
#' spec1<- rodents[rodents$Species == "Species1", ]
#' # Calculate the bandwidth
#' bw_hbcv(as.matrix(spec1[, c("Ave_C", "Ave_N")]))


getDistance<- function(p1, p2){
  if(!inherits(x, "matrix"))
    stop("x must be a 2-d numeric matrix")
  if(!is.numeric(x))
    stop("x must be a 2-d numeric matrix")
  if(dim(x)[2] != 2)
    stop("x must be a 2-d numeric matrix")
  return(sqrt(abs(p2[0]-p1[0])**2 + abs(p2[1]-p1[1])**2))
}