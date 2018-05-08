#' Plot PCA scatter plot using ggplot2
#'
#' @param user.plot data with user data and existing data combined
#' @param plot default is TRUE to output the figure
#'
#' @return
#' @export
#'
#' @examples
<<<<<<< HEAD
pcaplot <- function(data.plot, test.plot, plot = T, label) {
=======
pcaplot <- function(data.plot, test.plot, plot = T, label =1) {
>>>>>>> 324c23af6b3f2ceb709a03d1510e8b822439bd17
    library(ggplot2)
    data.plot$Name <- NA
    user.plot <- rbind(data.plot, test.plot)
    color.code <- c("#045a8d", "#43a2ca", "#1b9e77", "#a6d854", "#fc8d59", "#d53e4f")
<<<<<<< HEAD
    shape.code <- c(16, 1, 2, 0, 16)
    size.code <- c(4, 4, 4, 4, 6)
=======
    shape.code <- c(0, 1, 2, 0, 16)
    size.code <- c(4, 4, 4, 4, 7)
>>>>>>> 324c23af6b3f2ceb709a03d1510e8b822439bd17
    user.plot$Group <- as.factor(user.plot$Group)
    user.plot$Maturation <- as.factor(user.plot$Maturation)
    
    plot.res <- ggplot(data = user.plot, aes(x = PC1, y = PC2)) + theme(panel.grid.major = element_line(), 
        panel.grid.minor = element_line(), panel.background = element_blank(), axis.line.y = element_line(colour = "black"), 
        axis.line.x = element_line(colour = "black")) + 
    geom_point(aes(shape = Group, colour = Maturation, size = Group)) + scale_shape_manual(values = shape.code) + 
        scale_size_manual(values = size.code) + scale_colour_manual(values = color.code) + xlab("PC1") + 
        ylab("PC2") + theme(legend.key = element_blank())
<<<<<<< HEAD
    if (label) {
        reswithlabel <- plot.res + geom_text(aes(label = Name), size = 3, show.legend = F, angle = 30)
        ggsave("dataPCAwithlabel.pdf", plot = reswithlabel, width = 10, height = 6)
    }
    # theme(legend.position = 'none')
    #ggsave("dataPCA.png", width = 10, height = 6)
    ggsave("dataPCA.pdf", plot = plot.res, width = 10, height = 6)
=======
   if (label==1) {
        reswithlabel <- plot.res + geom_text(aes(label = Name), size = 3, show.legend = F, angle = 30)
        ggsave("dataPCAwithlabel.pdf", plot = reswithlabel, width = 10, height = 6)
	ggsave("dataPCA.pdf", plot = plot.res, width = 10, height = 6)
	ggsave("dataPCA.png", plot=reswithlabel, width=10, height=6)
    }else{
 	ggsave("dataPCA.png", width = 10, height = 6)
   	 ggsave("dataPCA.pdf", plot = plot.res, width = 10, height = 6)
	}


>>>>>>> 324c23af6b3f2ceb709a03d1510e8b822439bd17
}
