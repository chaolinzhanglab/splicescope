#' Output module exons matrix with or without subtraction of reference sample exon median
#'
#' @param original splicing matrix
#'
#' @return output two files for following visulization
#' @export
#'
#' @examples
get.modulemat <- function(data) {
    test.Smodule <- prefilter(data)
    test.Smodule1 <- submedian(test.Smodule, devcortex.Smodule)
    output1 <- cbind(rownames(test.Smodule1), test.Smodule1)
    colnames(output1)[1] <- "Name"
    write.table(output1, file = "data.Smodule.median.txt", quote = F, sep = "\t", row.names = T, col.names = T)
    write.table(test.Smodule, file = "data.Smodule.txt", quote = F, sep = "\t", row.names = T, col.names = T)
}
