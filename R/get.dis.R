#' Calculate distance between reference sample and predicated samples 
#'
#' @param beta regression object
#'
#' @return vector of distance to reference samples
#' @export
#'
#' @examples
get.dis <- function(lm.obj) {
    # y_hat <- lm.obj$fitted.values
    dis <- as.matrix(dist(t(cbind(lm.obj$fitted.values, devcortex.Smodule2))))[1, -1]
    dis <- dis/sum(dis)
    dis
}
