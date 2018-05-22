#' Pipeline function for neuronal prediction with RBP sepcific exons
#'
#' @param data input splicing matrix with NA and extreme value replaced
#' @return data.frame with Predicated maturation stage and Predication confidence score
#' @export
#'
#' @examples
rbp.predict <- function(data) {
    rbp <- 6:9
    res <- data.frame(matrix("", ncol = 0, nrow = ncol(data)), stringsAsFactors = F)
    rownames(res) <- colnames(data)
    for (idx in rbp) {
        rbp.bn <- subset(rbpAll.bn, rbpAll.bn[, idx] != 0)
        rbp.bn <- rbp.bn[order(rbp.bn[, idx]), ]
        rbp.tissue.Smodule <- data[match(rbp.bn$name, rownames(data), nomatch = 0), ]
        devcortex.rbp.Smodule2 <- devcortex.Smodule2[match(rbp.bn$name, rownames(devcortex.Smodule2), 
            nomatch = 0), ]
        # rbp.tissue.Smodule2 <- na.replace(rbp.tissue.Smodule)
        tmp.res <- rbp.beta.predict(rbp.tissue.Smodule, devcortex.rbp.Smodule2)
        colnames(tmp.res) <- c(paste(colnames(rbpAll.bn)[idx], "Maturation", sep = ""), paste(colnames(rbpAll.bn)[idx], 
            "ConfidenceScore", sep = ""))
        res <- cbind(res, tmp.res)
        
    }
    res <- as.data.frame(lapply(res, as.numeric))
    rownames(res) <- colnames(data) 
    newcolnames<- c("PtbpMaturation", "PtbpConfidenceScore", "NovaMaturation", "NovaConfidenceScore", "RbfoxMaturation", "RbfoxConfidenceScore", "MbnlMaturation", "MbnlConfidenceScore")
    res <- res[, match(newcolnames, colnames(res))]
    res
}
