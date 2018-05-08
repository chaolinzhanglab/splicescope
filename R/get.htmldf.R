#' generate html for data frame
#'
#' @param dataframe object
#'
#' @return 
#' @export
#'
#' @examples

<<<<<<< HEAD
get.htmldf <- function(x, Border = 1, innerBorder = 0, classfirstline = "firstline", classfirstcolumn = "firstcolumn", 
    classcellinside = "cellinside", append = TRUE, align = "center", captionalign = "bottom", classtable = "dataframe", 
    digits = 2, row.names = TRUE, ...) {
    
    
    txt <- paste("\n<p align=", align, ">")
    # txtcaption <- ifelse(is.null(caption), '', paste('\n<caption align=', captionalign, ' class=',
    # classcaption, '>', caption, '</caption>\n', sep=''))
    if (!is.null(Border)) 
        txt <- paste(txt, "\n<table cellspacing=0 border:1px solid black; table-layout: fixed; width:150px; style=\"font-family:arial;font-size=10;text-align=\"center\"", 
            "> \n<tr><td>", "\n\t<table border=", Border, " class=", classtable, ">", sep = "") else txt <- paste(txt, "\n<table border=", innerBorder, " class=", classtable, " cellspacing=0>", 
        txtcaption, sep = "")
    txt <- paste(txt, "\t<tbody>", sep = "\n")
    
    VecDebut <- c(if (row.names) paste("\n\t\t<th>", sep = "", collapse = ""), rep(paste("\n\t\t<th>", 
        sep = "", collapse = ""), ncol(x) - 1))
    VecMilieu <- c(if (row.names) "Sample", as.character(dimnames(x)[[2]]))
    VecFin <- c(if (row.names) rep(paste("", "</th>", collapse = ""), ncol(x) - 1), "</th>")
    txt <- paste(txt, "\n\t<tr class=", classfirstline, ">", paste(VecDebut, VecMilieu, VecFin, sep = "", 
        collapse = ""), "\n\t</tr>")
    
    x.formatted <- format(x, digits = 2)
    x.formatted <- as.matrix(x.formatted)
    x.formatted[is.na(x.formatted)] <- " "
    x.formatted[is.nan(x.formatted)] <- " "
    
    for (i in 1:dim(x)[1]) {
        if (i == 1) {
            VecDebut <- c(if (row.names) paste("\n<td class=", classfirstcolumn, ">", sep = ""), paste("\n<td class=", 
                classcellinside, ">", sep = ""), rep(paste("\n<td class=", classcellinside, ">", sep = ""), 
                dim(x)[2] - 1))
            VecMilieu <- c(if (row.names) dimnames(x)[[1]][i], x.formatted[i, ])
            VecFin <- c(if (row.names) "\n</td>", rep("\n</td>", dim(x)[2] - 1), "\n</td></tr>\n")
        } else {
            VecDebut <- c(if (row.names) paste("\n<td class=", classfirstcolumn, ">", sep = ""), paste(rep(paste("\n<td class=", 
                classcellinside, ">", sep = ""), dim(x)[2])))
            VecMilieu <- c(if (row.names) dimnames(x)[[1]][i], x.formatted[i, ])
            VecFin <- c(if (row.names) "\n</td>", rep("\n</td>", dim(x)[2] - 1), "\n</td></tr>\n")
        }
        txt <- paste(txt, "\n<tr align=", align, ">", paste(VecDebut, VecMilieu, VecFin, sep = "", 
            collapse = ""))
    }
    txt <- paste(txt, "\n\t</tbody>\n</table>\n", if (!is.null(Border)) 
        "</td></table>\n", "<br>")
    txt
=======
get.htmldf <- function(x,  rbp=NULL) {
	x.formatted <- format(x, digits = 2)
    x.formatted <- as.matrix(x.formatted)
    x.formatted[is.na(x.formatted)] <- " "
    x.formatted[is.nan(x.formatted)] <- " "
    x <- x.formatted
    if(is.null(rbp)){
		output2html("<table class=\"gridtable\">")
		output2html("<tbody> ")
		output2html("<tr class= firstline > ")
		output2html("<th>Sample </th>")
		output2html("<th>Maturation</th>")
		output2html("<th>Confidence </th> ")
		output2html("</tr> ")
		
		for (i in 1:dim(x)[1]) {
			output2html("<tr align= center > ")
			output2html(paste("<td class=firstcolumn>", rownames(x)[i], sep=""))
			output2html("</td>")
			output2html(paste("<td class=\"cellinside stage", x[i, 1],"\">", x[i, 1], sep=""))
			output2html("</td>")
			output2html(paste("<td class=cellinside>", x[i, 2], sep=""))
			output2html("</td></tr>")
		
		
		
		}
		output2html("</tbody>")
		output2html("</table>")	
	}else{
		output2html("<table class=\"gridtable\">")
		output2html("<tbody> ")
		output2html("<tr class= firstline > ")
		output2html("<th> </th>")
		output2html("<th colspan=2>Ptbp</th>")
		output2html("<th colspan=2>Nova</th>")
		output2html("<th colspan=2>Rbfox</th>")
		output2html("<th colspan=2>Mbnl</th>")
		output2html("</tr> ")
		output2html("<tr class= firstline > ")
		output2html("<th>Sample </th>")
		output2html("<th>Maturation </th>")
		output2html("<th>Confidence</th>")
		output2html("<th>Maturation </th>")
		output2html("<th>Confidence </th>")
		output2html("<th>Maturation </th>")
		output2html("<th>Confidence </th>")
		output2html("<th>Maturation</th>")
		output2html("<th>Confidence </th> ")
		output2html("</tr> ")
		for (i in 1:dim(x)[1]) {
			output2html("<tr align= center > ")
			output2html(paste("<td class=firstcolumn>", rownames(x)[i], sep=""))
			output2html("</td>")
			#for (j in 1:dim(x)[2]/2){
			output2html(paste("<td class=\"cellinside stage", x[i, 1],"\">", x[i, 1], sep=""))
			output2html("</td>")
			output2html(paste("<td class=cellinside>", x[i, 2], sep=""))
			output2html("</td>")
			#}
			output2html(paste("<td class=\"cellinside stage", x[i, 3],"\">", x[i, 3], sep=""))
                        output2html("</td>")
                        output2html(paste("<td class=cellinside>", x[i, 4], sep=""))
                        output2html("</td>")
			output2html(paste("<td class=\"cellinside stage", x[i, 5],"\">", x[i, 5], sep=""))
                        output2html("</td>")
                        output2html(paste("<td class=cellinside>", x[i, 6], sep=""))
                        output2html("</td>")
			output2html(paste("<td class=\"cellinside stage", x[i, 7],"\">", x[i, 7], sep=""))
                        output2html("</td>")
                        output2html(paste("<td class=cellinside>", x[i, 8], sep=""))
                        output2html("</td>")
	

			output2html("</tr>")
		}
		output2html("</tbody>")
		output2html("</table>")	
		
	}     
>>>>>>> 324c23af6b3f2ceb709a03d1510e8b822439bd17
}
