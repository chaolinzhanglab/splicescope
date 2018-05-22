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
maturation.predict <- function(test.data, outfile, plot = T, verbose = verbose, label=1) {
    if (verbose) {
        cat("Processing data......\n")
    }
    test.Smodule <- prefilter(test.data)
    test.Smodule2 <- na.replace(test.Smodule)
    test.Smodule3 <- extreme.replace(test.Smodule2)
    if (verbose) {
        cat("Predicting maturation......\n")
    }
    test.betareg <- beta.predict(test.Smodule3)
    test.rbp.betareg <- rbp.predict(test.Smodule3)
    if (verbose) {
        cat("Output results......\n")
    }
    if (!is.null(outfile)) {
        write.table(cbind(test.betareg, test.rbp.betareg), file = "result_prediction.txt", quote = F, 
            sep = "\t", col.names = T, row.names = T)
    }
    if (plot) {
        test.pca <- as.data.frame(predict(devcortex.pca, t(test.Smodule2))[, 1:2])
        test.plot <- data.frame(matrix("", ncol = ncol(data.plot), nrow = ncol(test.Smodule3)), stringsAsFactors = F)
        colnames(test.plot) <- colnames(data.plot)
        test.plot$Group <- "UserDefined"
        test.plot$Name <- colnames(test.Smodule3)
        test.plot$True.Age <- NA
        test.plot$True.Label <- NA
        test.plot$PC1 <- test.pca$PC1
        test.plot$PC2 <- test.pca$PC2
        test.plot$Maturation <- test.betareg$Maturation
        # user.plot <- rbind(data.plot, test.plot)
        pcaplot(data.plot, test.plot, plot, label)
    }
    if (!is.null(outfile)) {
        write.table(rbind(test.pca, devcortex.pca$x[, 1:2]), file = "result_pca.txt", quote = F, sep = "\t", 
            col.names = T, row.names = T)
    }
    if (verbose) {
        cat("Generating HTML file......\n")
    }
    generate.html(test.betareg, test.rbp.betareg)
    if (verbose) {
        cat("Generating zip file......\n")
    }
    if (label==1) {
        system(paste("zip -qm", outfile, "dataPCA.png dataPCA.pdf dataPCAwithlabel.pdf index.html result_prediction.txt result_pca.txt", 
            collapse = ""))
    } else {
        system(paste("zip -qm", outfile, "dataPCA.png dataPCA.pdf index.html result_prediction.txt result_pca.txt", 
            collapse = ""))
    }
    if (verbose) {
        cat("Done......\n")
    }
}
