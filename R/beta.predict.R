#' predict neuronal maturation with all module exons using beta regression methods
#'
#' @param data processed splicing matrix with 'NA' and extreme value removed
#'
#' @return data.frame with Predicated maturation stage and Predication confidence score
#' @export
#'
#' @examples
beta.predict <- function(data) {
    library(betareg)
    res <- data.frame(matrix("", ncol = 2, nrow = ncol(data)), stringsAsFactors = F)
    rownames(res) <- colnames(data)
    colnames(res) <- c("Maturation", "ConfidenceScore")
    
    for (i in 1:ncol(data)) {
        beta.res <- betareg(data[, i] ~ ., as.data.frame(devcortex.Smodule2))
        beta.Sdis <- get.dis(beta.res)
        knn.res <- knn.flex.reg(beta.Sdis, 1)
        confi.score <- 1 - min(beta.Sdis)/max(beta.Sdis)
        beta.class <- unlist(sapply(knn.res, function(x) return(Score[[x]])))
        res[i, 1] <- beta.class
        res[i, 2] <- confi.score
    }
    
    res <- as.data.frame(lapply(res, as.numeric))
    rownames(res) <- colnames(data)
    res
}
