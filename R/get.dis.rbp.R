#' Calculate distance between reference sample and predicated samples with specifc RBP binding exons
#'
#' @param beta regression object
#'
#' @return vector of distance to reference samples
#' @export
#'
#' @examples
get.dis.rbp <- function(lm.obj, devcortex.rbp) {
    dis <- as.matrix(dist(t(cbind(lm.obj$fitted.values, devcortex.rbp))))[1, -1]
    dis <- dis/sum(dis)
    dis
}
