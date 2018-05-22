#' generate html for data frame
#'
#' @param dataframe object
#'
#' @return 
#' @export
#'
#' @examples

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
}
