#' Replace NA values with mean of reference sample 
#'
#' @param module exon splicing matrix
#'
#' @return module exon splicing matrix with NA replaced
#' @export
#'
#' @examples
na.replace.datamean <- function(data) {
    
    rep <- function(x) {
        idx <- which(is.na(x))
        #x[idx] <- data.Smodulemean[idx]
        x[idx] <- mean(x, na.rm = T)
	x
    }
    data.narm <- t(apply(as.matrix(data), 1, rep))
    
    data.narm
}
