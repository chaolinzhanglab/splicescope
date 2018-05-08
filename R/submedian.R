
#' Get matrix subtracted by second matrix's median
#'
#' @param data 
#' @param median.data 
#'
#' @return matrix
#' @export
#'
#' @examples
submedian <- function(data, median.data) {
    options(warn = -1)
    median.data <- data.matrix(median.data)
    ref.median <- apply(median.data, 1, function(x) median(x, na.rm = T))
    ref.median <- ref.median[match(rownames(data), rownames(median.data))]
    data <- data.matrix(data)
    return(data - ref.median)
}
