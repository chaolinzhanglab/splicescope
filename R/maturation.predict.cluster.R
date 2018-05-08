#' Pipeline function for neuronal prediction
#'
#' @param test.data input splicing matrix
#' @param outfile  output file for prediction result
#' @param plot default is TRUE to plot PCA for all the data
#'
#' @return
#' @export
#'
#' @examples
maturation.predict.cluster <- function(test.data) {
    test.Smodule <- prefilter(test.data)
    test.Smodule2 <- na.replace(test.Smodule)
    test.Smodule3 <- extreme.replace(test.Smodule2)
    test.betareg <- beta.predict(test.Smodule3)
    test.rbp.betareg <- rbp.predict(test.Smodule3)
    test.pca <- as.data.frame(predict(devcortex.pca, t(test.Smodule2))[, 1:2])
    
}
