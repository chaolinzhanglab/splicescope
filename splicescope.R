### suppress warning messages
options(warn=-1)
suppressMessages(library(getopt, quietly=T))
suppressMessages(library(Splicescope))

#argument mask: 0 - no argument, 1 - required argument, 2 - optional argument

optionSpec = matrix(c(
    'bed',            'b',    1, "character",
    'samplename',     'n',    1, "character",
    'splicingmatrix', 's',    1, "character",
    'output',         'o',    1, "character",
    'type',           't',    2, "character",
    'cache',          'c',    2, "character",
    'label',          'l',    2, "integer",
    'quantas',        'q',    1, "character",
    'annotation',     'a',    1, "character",
    'verbose',        'v',    2, "integer",
    'help'   ,        'h',    0, "logical"
    ), byrow=TRUE, ncol=4)
opt = getopt(optionSpec)

##default parameters

type <- "maturation"
label <- 1
verbose <- 0

sfile <- opt$splicingmatrix
bfile <- opt$bed
bfilename <- opt$samplename
outfile <- opt$output
perlloc <- opt$quantas
confloc <- opt$annotation
if (!is.null(opt$type)) {type <- opt$type}
if (!is.null(opt$cache)) {cache <- opt$cache}
if (!is.null(opt$label)) {label <- opt$label}
if (!is.null(opt$verbose)) {verbose <- opt$verbose}


if ( !is.null(opt$help) | (is.null(opt$splicingmatrix) & is.null(opt$bed)) | is.null(opt$output) |  (!is.null(opt$splicingmatrix) & !is.null(opt$bed)))
{
    cat (
        'Spliescope: prediction of cellular states based on splicing profile\n',
        'Usage1: Rscript ', get_Rscript_filename(),"[options] -s <in.splicingmatrix> -o <out.zip>\n",
	'Usage2: Rscript ', get_Rscript_filename(),"[options] -q countit/ -a mm10/ -b <in.junctionbed1>,<in.junctionbed2>... -n samplename.txt -o <out.zip>\n",        
        '[required]\n',
        ' -b, bedfile         Junction bed file\n',
        ' -n, samplenames     Txt file with all sample names(Must be specified if using -b)\n',
	' -s, splicingmatrix  Txt file with exon inclusion ratio matrix(Can not be specified if using -b)\n',
	' -o, output file     Output zip file(For example: output.zip)\n',
	' -q, Quantas dir     The Quantas directory(For example: /usr/local/src/quantas/countit/, must be specified if using -b)\n',
	' -a, Annotation dir  The annoation directory(For example: /usr/local/src/mm10/, must be specified if using -b)\n',
        '[options]\n',
        #' -t, analysis-type   Type of analysis to be performed based on splicing profile ([maturation])\n',
        ' -l, sample label    Output pdf format plot with sample label([1-On])\n',
	' -c, cache dir	      Path to write temporary file\n',
        ' -v, verbose         Verbose mode\n',
        ' -h, help            Print usage\n'
    );
    q(status=1);
}
## Set working directory as the user input directory 

setwd(dirname(outfile))

if(type=="maturation"){
	outfilename <- basename(outfile)
	outfilebase <- strsplit(outfilename, "\\.")[[1]][1]
	outfileext <- strsplit(outfilename, "\\.")[[1]][2]

	if(!is.null(sfile) & is.null(bfile)){
	## deal with splicing matrix file
		if(verbose) {cat('load data ...\n')}
		data <- read.table(sfile, stringsAsFactors = F, header=T, sep="\t")
		res <- maturation.predict(data, outfilebase, plot = T, verbose=verbose, label=label)
	## if output file named as .zip then do normal prediction without sybomlic link
		if(outfileext !="zip" & file.exists(paste(outfilebase, ".zip", sep=""))){
			system(paste("ln -s -f ", outfilebase, ".zip ", outfilename, sep=""))	
		}
		q(status=1)
	}else if(is.null(sfile) & !is.null(bfile)){
		if(is.null(bfilename)) stop("Sample name file must be specified if using -b\n")
		else if(is.null(perlloc)) stop("Quantas directory must be specified if using -b\n")
		else if(is.null(confloc)) stop("Annotation directory must be specified if using -b\n")		
		else{
		## deal with junction bed file
                        bfile <- strsplit(bfile, split=",")[[1]]  
			if(verbose) {
				cat('load data ...\n')
				for (i in 1:length(bfile)){
				print(perlloc)
				print(paste("perl ", perlloc ,"summarize_splicing_wrapper.pl -big -weight -conf ", confloc ,"mm10.conf -dbkey mm10 -cass  -v ",bfile[i], " ", strsplit(bfile[i], split="[.]")[[1]][1],sep=""))
					system(paste("perl ", perlloc ,"summarize_splicing_wrapper.pl -big -weight -conf ", confloc ,"mm10.conf -dbkey mm10 -cass  -v ",bfile[i], " ", strsplit(bfile[i], split="[.]")[[1]][1],sep=""))
				}
				matrixfile <- paste(dirname(outfile),"/", Sys.getpid(), "_user.cass.mat.txt", sep="")	
				system(paste("perl ", perlloc, "gen_splicing_matrix.pl -v -type cass --min-cov 10 --max-std 0.1 --id2gene2symbol ", confloc, "Mm.seq.all.devcortex.cass.chrom.can.id2gene2symbol ", bfilename, " ", matrixfile, sep=""))
			}
			if(!verbose) { 
                                #cat('load data ...\n')
                                for (i in 1:length(bfile)){
                                        system(paste("perl ",perlloc ,"summarize_splicing_wrapper.pl -big -weight -conf ",confloc ,"mm10.conf -dbkey mm10 -cass ",bfile[i]," ",strsplit(bfile[i], split="[.]")[[1]][1],sep=""), ignore.stdout=T)
                                }                             
				matrixfile <- paste(dirname(outfile),"/", Sys.getpid(), "_user.cass.mat.txt", sep="")  
				 system(paste("perl ", perlloc, "gen_splicing_matrix.pl -type cass --min-cov 10 --max-std 0.1 --id2gene2symbol ", confloc, "Mm.seq.all.devcortex.cass.chrom.can.id2gene2symbol ", bfilename, " ", matrixfile, sep=""),  ignore.stdout=T)
                        }
			 for (i in 1:length(bfile)){
				system(paste("rm -rf", strsplit(bfile[i], split="[.]")[[1]][1], sep=" "))
			}
			## Predict maturation using output from countit
			data <- read.table(matrixfile, stringsAsFactors = F, header=T, sep="\t", comment.char="")
			colnames(data)[1] <- "event_id"
                	system(paste("rm ", matrixfile, sep=""))
			res <- maturation.predict(data, outfilebase, plot = T, verbose=verbose, label=label)
        		## if output file named as .zip then do normal prediction without sybomlic link
                	if(outfileext !="zip" & file.exists(paste(outfilebase, ".zip", sep=""))){
                        #print("not zip, build link")   
                        	system(paste("ln -s -f ", outfilebase, ".zip ", outfilename, sep=""))
               		 }
	
		}
	}

}

 

