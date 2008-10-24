############################################################
#                      PBS Modelling                       #
# ---------------------------------------------------------#
#                                                          #
# Authors:                                                 #
#  Jon T. Schnute <SchnuteJ@pac.dfo-mpo.gc.ca>,            #
#  Alex Couture-Beil <alex@mofo.ca>, and                   #
#  Rowan Haigh <HaighR@pac.dfo-mpo.gc.ca>                  #
#  Anisa Egeli <EgeliA@pac.dfo-mpo.gc.ca>                  #
#                                                          #
############################################################

# ***********************************************************
# unpackList:
#  make global variables from the components of a list
# Input:
#  x - list object with named components
# Result: 
#  global variables with names and contents extracted from x
# Output: 
#  vector of variable names created
# -----------------------------------------------------------
unpackList <- function(x, scope="L") 
{
	namx <- names(x); nx <- length(namx);
	if (nx > 0) for (i in 1:nx) {
		if (namx[i] != "") {
			if (scope=="L")
				assign(namx[i], x[[i]], pos=parent.frame(1))
			else if (scope=="G")
				assign(namx[i], x[[i]], env = .GlobalEnv)
		}; 
	};
	namx[namx != ""];
};


# ***********************************************************
# compileDescription:
#  Convert a GUI description file into a complete GUI desc List
#  which can be passed directly to createWin
# Arguments:
#  descFile - filename of GUI description file
#  outFile  - filename to save list to. 
#             WARNING: this will overwrite the file, if it currently exists
# -----------------------------------------------------------
compileDescription <- function(descFile, outFile="")
{
	if (outFile!="")
		sink(outFile)

	x<-parseWinFile(descFile)

	cat(paste(
	"#This file was automaticaly generated from window description file \"",
	descFile, "\"\n#with the compileDescription function.\n\n", sep=""))
	cat("#This list can then be passed directly to createWin()\n\n")
	cat(paste("#To assign this list to a variable: GUIdesc<-eval(parse(\"", outFile, "\"))\n", sep=""))
	writeList(x)

	cat("\n")
	if (outFile!="")
		sink()
}


# ***********************************************************
# .addslashes:
#  escapes special characters from a string, which can then be used in the "P" format
#  if x has more than one element, then it will returned a nested characterVector
#  ie: c("it's", "O K") becomes => "'it\'s' 'O K'"
# Arguments:
#  x - string to escape
# -----------------------------------------------------------
.addslashes <- function(x)
{
	#escape backslashes
	x <- gsub("\\\\", "\\\\\\\\", x)

	#escase doublequotes
	x <- gsub("\"", "\\\\\"", x)

	#escase singlequotes
	x <- gsub("'", "\\\\'", x)

	#convert into substrings if applicable
	if (length(x)>1) {
		i<-append(grep("[ \t\\\\]+", x), grep("^$", x)) #indicies needing quotes
		x[i]<-paste("'", x[i], "'", sep="")
		x<-paste(x, collapse=" ")
	}
	else {
		#special case where it is a single word with no special chars
		if (!any(grep("[ \t\\\\]+", x)) && x!="")
			return(x)
	}

	return(paste("\"", x, "\"", sep=""))
}


# ***********************************************************
#saves list x to disk using "P" format
# -----------------------------------------------------------
.writeList.P <- function(x, fname="", comments)
{
	if (fname!="")
		sink(fname)

	if (!missing(comments)) {
		cat(paste(comments,collapse="\n")); cat("\n")
	}

	xNames <- names(x)

	#check for errors
	for(i in 1:length(x)) {
		if (!is.matrix(x[[i]]) && !is.vector(x[[i]]) && class(x[[i]])!="data.frame")
			stop("writelist can only support modes of vector, matrix, and dataframes.")
		if (xNames[i]=="")
			stop("list must have named elements")

		#prepare character strings with quotes if spaces exist
		if (is.character(x[[i]])) {
			for(j in 1:length(x[[i]])) {
				#only strings with spaces need quotes
				if (mode(x[[i]][j])=="character") {
					x[[i]][j] <- .addslashes(x[[i]][j])
				}
			}
		}
	}

	#start cat-ing keys and values
	for(i in 1:length(x)) {

		#print varName
		cat(paste("$", xNames[i], "\n", sep=""))

		if (is.matrix(x[[i]])) {
			#print colnames
			matColNames<-colnames(x[[i]])
			matRowNames<-rownames(x[[i]])
			if (is.null(matColNames))
				matColNames <- ""
			if (is.null(matRowNames))
				matRowNames <- ""
			matColNames <- .addslashes(matColNames)
			matRowNames <- .addslashes(matRowNames)
			cat(paste("$$matrix mode=\"", mode(x[[i]]), "\" rownames=", matRowNames, " colnames=", matColNames, " ncol=", ncol(x[[i]]), "\n", sep=""))

			for(j in 1:dim(x[[i]])[1]) {
				cat(x[[i]][j,]); cat("\n")
			}
		}
		else if (is.vector(x[[i]])) {
			#print names
			vecNames<-names(x[[i]])
			if (is.null(vecNames))
				vecNames <- ""
			vecNames <- .addslashes(vecNames)
			cat(paste("$$vector mode=\"", mode(x[[i]]), "\" names=", vecNames, "\n", sep=""))


			cat(x[[i]]); cat("\n")
		}
		else if (class(x[[i]])=="data.frame") {
			cat("$$data "); 
			#ncol
			cat("ncol="); cat(dim(x[[i]])[2]); cat(" ");
			#modes
			cat("modes=\"")
			for (j in 1:length(x[[i]])) {
				if (j>1)
					cat(" ")
				cat(mode(x[[i]][[j]]))
			}
			cat("\" ")

			#rownames
			cat("rownames="); cat(.addslashes(rownames(x[[i]]))); cat(" ")

			#colnames
			cat("colnames="); cat(.addslashes(colnames(x[[i]]))); cat(" ")

			#byrow
			cat("byrow=TRUE"); cat("\n")
			for(j in 1:dim(x[[i]])[1]) {
				for(k in 1:dim(x[[i]])[2]) {
					cat(x[[i]][j,k]); cat(" ")
				}
				cat("\n")
			}
		}
	}

	if (fname!="")
		sink()
}


# ***********************************************************
#Read list in "P" format
# -----------------------------------------------------------
.readList.P <- function(fname)
{

	srcfile <- orgfile <- scan(fname, what=character(), sep="\n", quiet=TRUE, blank.lines.skip=FALSE)
	#srcfile will be modified, orgfile is untouched and only used for user debug error messages


	data <- list()
	j <- 0
	halt <- FALSE
	extendLine <- FALSE #used for extending a single line into lines with \
	extendLineNumber <- 0 #where a new widget starts - used for error messages
	str <- ""

	if (!length(srcfile)) {
		stop("Input file is empty\n")
	}
#print("loop start"); print(date());
	#if comments were striped out earlier, we would lose the line count.
	for(i in 1:length(srcfile)) {
		if (!any(grep("^[[:space:]]*(#.*)?$", srcfile[i]))) {

			srcfile[i] <- .stripComments(srcfile[i])

			#append last string onto new string if applicable
			if (extendLine == TRUE)
				str <- paste(str, srcfile[i], sep=" ")
			else {
				str <- srcfile[i]
				extendLineNumber <- i
			}

			#determine if this string is extended by a \ at the end.
			tmp <- sub('\\\\$', '', str)
			if (tmp==str) #no sub took place
				extendLine = FALSE
			else
				extendLine = TRUE
			str <- tmp

			#parse the line once it is complete (no \)
			if (extendLine == FALSE) {
				j <- j + 1
				data[[j]]<-list(str=str, line.start=extendLineNumber, line.end=i)
			}
		}
	}

	#convert the "data" list into a real list
	varName <- NULL
	varOptions <- NULL
	varData <- list()
	retData <- list() #list to return
	for(i in 1:length(data)) {
		str <- data[[i]]$str

		#varOptions (optional)
		if (substr(str,1,2)=="$$") {
			if (!is.null(varOptions))
				stop("extra $$ line found")
			if (is.null(varName))
				stop("$$ line found before $ line")
			varOptions <-data[[i]]

			varOptions$str = substr(varOptions$str, 3, nchar(varOptions$str)) #remove $$
		}

		#varName
		else if (substr(str,1,1)=="$") {
			if (!is.null(varName)) {
				#save data into the retData list
				retData[[varName]] <- .readList.P.convertData(varOptions, varData, fname, orgfile)
				if (is.null(retData[[varName]]))
					halt<-TRUE
				varName <- varOptions <- NULL
				varData <- list()
			}
			varName <- .trimWhiteSpace(substr(str, 2, nchar(str)))
			if (!any(grep("^[a-zA-Z0-9_.]+$", varName))) {
				.catError(
					paste("Variable name \"", varName,"\" is not valid", sep=""), fname, 
					data[[i]]$line.start, data[[i]]$line.end, 
					orgfile, "readList error"
					)
				halt<-TRUE
			}
			line.start <- data[[i]]$line.start
		}
		else {
			varData[[length(varData)+1]] <-data[[i]]
		}
	}
	#save anything from after
	if (!is.null(varName)) {
		#print(".readList.P.convertData start"); print(date());
		retData[[varName]] <- .readList.P.convertData(varOptions, varData, fname, orgfile)
		#print(".readList.P.convertData end"); print(date());
		if (is.null(retData[[varName]]))
					halt<-TRUE
	}

	if (halt==TRUE) {
		stop("Errors were found in the file. Unable to continue\n")
	}
	return(retData)
}


# ***********************************************************
# helper function to convert data into proper mode
# -----------------------------------------------------------
.readList.P.convertData <- function(varOptions, varData, fname="", sourcefile=list())
{

	if (is.null(varOptions)) {
		#simple format with no options

		if (length(varData)>1) {
			#some sort of matrix to parse
			dimSize <- c(length(varData),0) #num of rows
			matData <- c() #vector to hold values byrow

			for(i in 1:length(varData)) {
				tmp <- .convertParamStrToVector(varData[[i]]$str, fname, varData[[i]]$line.start)
				if (dimSize[2]==0)
					dimSize[2] <- length(tmp)
				else if (length(tmp)!=dimSize[2]) {
					.catError(paste("Matrix row (line ",varData[[i]]$line.start,") lenght should match first row (line ",varData[[1]]$line.start,") length of ", dimSize[2], sep=""), fname, 
					varData[[i]]$line.start, varData[[i]]$line.end, 
					sourcefile, "readList error")
					return(NULL)
				}
				matData <- append(matData, tmp)
			}

			matData <- .autoConvertMode(matData)
			return(matrix(matData, dimSize[1], dimSize[2], byrow=TRUE))
		}
		else {
			#just a vector
			return(.autoConvertMode(.convertParamStrToVector(varData[[1]]$str, fname, varData[[1]]$line.start)))
		}
	}
	#otherwise varOptions was given (in string format)

	#convert it into a list first
	opts <-.getParamFromStr(varOptions$str, fname, varOptions$line.start, varOptions$line.end, sourcefile, .pFormatDefs)

	if (is.null(opts))
		stop("Errors were detected")

	#flatten all data into a vector (of characters)
	x <- c()
	for(i in 1:length(varData)) {
		#weird things happen if its x[i] <- as.vector(.convert...)
		x <- c(x, .convertParamStrToVector(varData[[i]]$str, fname, varData[[i]]$line.start))
	}

	if(opts$type=="vector") {
		x <- .convertMode(x, opts$mode)

		if (any(opts$names!="")) {
			names(x)<-opts$names
		}
		return(x)
	}
	else if(opts$type=="matrix") {
		x <- .convertMode(x, opts$mode)

		#calculate dims
		nrow <- length(x)/opts$ncol
		if (as.integer(nrow)!=nrow) {
			.catError(paste("Matrix data length [", length(x), "] is not a sub-multiple of ncol [", opts$ncol, "]", sep=""), fname, 
			varOptions$line.start, varData[[length(varData)]]$line.end, 
			sourcefile, "readList error")
			return(NULL)
		}

		#convert to matrix
		mat <- matrix(x, nrow, opts$ncol, byrow=opts$byrow)

		#add colnames
		if (any(opts$colnames!="")) {
			if (length(opts$colnames)!=opts$ncol) {
				.catError(paste("Matrix colnames length [", length(opts$colnames), "] is not equal to ncol [", opts$ncol, "]", sep=""), fname, 
				varOptions$line.start, varData[[length(varData)]]$line.end, 
				sourcefile, "readList error")
				return(NULL)
			}
			colnames(mat)<-opts$colnames
		}
		#add rownames
		if (any(opts$rownames!="")) {
			if (length(opts$rownames)!=nrow) {
				.catError(paste("Matrix rownames length [", length(opts$rownames), "] is not equal to nrow [", nrow, "]", sep=""), fname, 
				varOptions$line.start, varData[[length(varData)]]$line.end, 
				sourcefile, "readList error")
				return(NULL)
			}
			rownames(mat)<-opts$rownames
		}
		return(mat)
	}
	else if(opts$type=="array") {
		x <- .convertMode(x, opts$mode)

		opts$dim <- .convertMode(opts$dim, "numeric")
		if (any(is.na(opts$dim))) {
			.catError("dim values must be numeric", fname, 
			          varOptions$line.start, varData[[length(varData)]]$line.end, 
			          sourcefile, "readList error")
			return(NULL)
		}

		#check dims works
		if (length(x)!=prod(opts$dim)) {
			.catError(paste("dims [product ",prod(opts$dim),"] do not match the length of object [",length(x),"]", sep=""), fname, 
			          varOptions$line.start, varData[[length(varData)]]$line.end, 
			          sourcefile, "readList error")
			return(NULL)
		}
		if (opts$byright) {
			x <- .convertVecToArray(x,opts$dim,byright=TRUE)
		}
		else {
			#could use convertVecToArray, but this is faster
			dim(x) <- opts$dim
		}
		return(x)
	}
	else if(opts$type=="data") {
		#check ncol works

		if (length(x)%%opts$ncol>0) {
			.catError(paste("dataframe data length [", length(x), "] is not a sub-multiple of ncol [", opts$ncol, "]", sep=""), fname, 
			varOptions$line.start, varData[[length(varData)]]$line.end, 
			sourcefile, "readList error")
			return(NULL)
		}

		if (opts$ncol != length(opts$colnames)) {
			.catError(paste("Data colnames length [", length(opts$colnames), "] is not equal to ncol [", opts$ncol, "]", sep=""), fname, 
			varOptions$line.start, varData[[length(varData)]]$line.end, 
			sourcefile, "readList error")
			return(NULL)
		}

		if (opts$ncol != length(opts$modes)) {
			.catError(paste("Data modes length [", length(opts$modes), "] is not equal to ncol [", opts$ncol, "]", sep=""), fname, 
			varOptions$line.start, varData[[length(varData)]]$line.end, 
			sourcefile, "readList error")
			return(NULL)
		}

		#calculate nrow
		nrow <- length(x)/opts$ncol

		#break up data into a vector of a list, such that each element represents a column
		dataCols <- list()
		if (opts$byrow) {
			for(i in 1:length(x)) {
				j <- i%%opts$ncol
				if (j==0)
					j <- opts$ncol
				if (length(dataCols)<j)
					dataCols[[j]] <- x[i]
				else
					dataCols[[j]] <- c(dataCols[[j]], x[i])
			}
		}
		else {
			for(i in 1:length(x)) {
				j <- as.integer((i-1)/(length(x)/opts$ncol))+1

				if (length(dataCols)<j)
					dataCols[[j]] <- x[i]
				else
					dataCols[[j]] <- c(dataCols[[j]], x[i])
			}
		}
		#create data.frame and use colnames to refer to each colum
		#the data.frame will be stored as 'ret'
		txt <- "ret <- data.frame("
		for(i in 1:opts$ncol) { #foreach column
			#convert into propper mode
			dataCols[[i]] <- .convertMode(dataCols[[i]], opts$modes[i])

			if (i>1)
				txt <- paste(txt, ", ", sep="")
			name <- opts$colnames[i]
			txt <- paste(txt, name, "=dataCols[[", i, "]]", sep="")
		}
		txt <- paste(txt, ")", sep="")
		eval(parse(text=txt))

		#add rownames if any exist
		if (any(opts$rownames!="")) {
			if (length(opts$rownames)!=nrow) {
				.catError(paste("Data rownames length [", length(opts$rownames), "] is not equal to nrow [", nrow, "]", sep=""), fname, 
				varOptions$line.start, varData[[length(varData)]]$line.end, 
				sourcefile, "readList error")
				return(NULL)
			}
			rownames(ret)<-opts$rownames
		}

		return(ret)
	}

}


# ***********************************************************
# .mapArrayToVec:
#  determines which index to use for a vector, when given an 
#  N-dim index of an array.
# Arguments:
#  x       - array index (numeric vector)
#  d       - dimensions of the array
#  byright - if true, vary most right indices first, 
#            if false, vary by left (R default)
# -----------------------------------------------------------
.mapArrayToVec <- function(x,d, byright=TRUE)
{
	x <- x - 1 #start counting at 0 instead of 1

	m <- length(x)
	if (m!=length(d))
		stop("given points (x), does not match lenght of given dimensions (d)")

	if (byright) {
		ind <- x[m]
		for(i in (m-1):1) {
			ind <- ind+x[i]*prod(d[(i+1):m])
		}
		return(ind+1)
	}
	else {
		ind <- x[1]
		for(i in 2:length(d)) {
			ind <- ind+x[i]*prod(d[(i-1):1])
		}
		return(ind+1)
	}
	#return(x[1] + d[1]*x[2])
}


# ***********************************************************
# .getArrayPts:
#  Returns all possible indices of an array
# Arguments:
#  d is a vector of integers specifing the dimensions
# output:
#  a list of vectors of all possible indices
# -----------------------------------------------------------
.getArrayPts <- function(d)
{
	x<-list()
	for(i in 1:length(d))
		x[[i]]<-d[i]:1

	x<-expand.grid(x)
	y<-list()
	for(i in 1:length(x[[1]])) {
		z<-unlist(x[i,])
		attributes(z)<-NULL
		y[[i]]<-z
	}
	return(y)
}


# ***********************************************************
# .convertVecToArray:
#  converts a vector to an Array
# Arguments:
#  x       - a vector of data to use to create array
#  d       - dimensions of the array
#  byright - if TRUE, varry indicies by the most right number first
#                 ex) 1,1 - 1,2 - 1,3 - 2,1 - 2,2 - 2,3
#            if FALSE, varry by most left (R default)
#                 ex) 1,1 - 2,1 - 1,2 - 2,2 - 1,3 - 2,3  
# -----------------------------------------------------------
.convertVecToArray <- function(x,d, byright=TRUE)
{
	if (length(x)!=prod(d))
		stop("given vector x length does not match product of dimensions")

	#create array
	y<-vector(mode(x), prod(d))
	dim(y)<-d

	#iterate over every possible index
	pts<-.getArrayPts(d)
	for(i in 1:length(pts)) {
		arrIndex <- paste(pts[[i]],collapse=",")
		vecIndex <- .mapArrayToVec(pts[[i]], d, byright)

		#map it to the appropriate place in the given X vector
		code = paste("y[", arrIndex, "] <- ", x[vecIndex], sep="")
		eval(parse(text=code))
	}
	return(y)
}


# ***********************************************************
# createVector:
#  create a GUI with a vector widget and button
# Arguments:
#  vec:          a vector of widget variable names
#                if vec is named, then the names are used as widget variable 
#                names and the values are used as the default value
#
#  vectorLabels: if supplied, this vector of labels are printed above each entry box
#                There should be one label for every variable defined in vec
#                i.e. length(vectorLabels)==length(vec)
#
#  func:         function name as a string
#                If given, this function will be called whenever data is entered
#                i.e. Enter pressed, or submit button clicked. This user function
#                would then most likely use getWinVal()
#
#  windowname:  windowname to use for this GUI
#
# Output: If no user defined function is given (see func paramater), then global variables 
#         matching the variable name is set with the value of the widget
#         whenever text focus is in a widget and enter is pressed, or when submit is pushed.
#         Otherwise, func will be called and it is the user's responsibility to  make use of getWinVal
#
# -----------------------------------------------------------
createVector <- function (vec, vectorLabels=NULL, func="", windowname="vectorwindow") {
	if (is.null(names(vec))) {
		namesVal <- vec
		valuesVal <- ""
	}
	else {
		namesVal <- names(vec)
		valuesVal <- vec
	}
	if (!is.character(func))
		stop("func must be a character string")
	namesVal <- as.character(namesVal)
	if (is.null(vectorLabels)) 
		vecLabels <- names(vec)
	else {
		if (length(vectorLabels) != length(vec)) 
			stop("length of paramaters vec and vectorLabels should be the same length")
		vectorLabels <- as.character(vectorLabels)
		vecLabels <- vectorLabels
	}
	winList <- list(list(title = "Vector", windowname = windowname, vertical = TRUE, 
		onclose = "", .widgets = list(list(type = "vector", names = namesVal, 
		length = length(vec), labels = vecLabels, values = valuesVal, 
		font = "", vertical = FALSE, "function" = func, enter = TRUE, 
		action = "", mode = "numeric", width = 6, sticky = "", 
		padx = 0, pady = 0), list(type = "button", "function" = func, 
		text = "Go", padx = 0, pady = 0)), .menus = list()))
	createWin(winList)
}

# ***********************************************************
# promptOpenFile:
#  opens a prompt and asks a user to select a file.
# Arguments:
#  initialfile - filename to pre-select
#  filetype - list of vectors specifying allowed filetypes
# Returns:
#  selected filename
# Example:
#  promptOpenFile("intial_file.txt", filetype=list(c(".txt", "text files"), 
#                 c(".r", "R files"), c("*", "All Files")))
# -----------------------------------------------------------
promptOpenFile <- function(initialfile="", filetype=list(c("*", "All Files")), open=TRUE)
{
	filetypes <- ""
	for(i in 1:length(filetype)) {
		filetype[[i]]
		if (is.na(filetype[[i]][2]))
			filetype[[i]][2] <- filetype[[i]][1]
		if (filetype[[i]][1] != "*" && substr(filetype[[i]][1],1,1)!=".")
			filetype[[i]][1] <- paste(".", filetype[[i]][1], sep="")
		filetypes <- paste(filetypes, " {{", filetype[[i]][2], "} {", filetype[[i]][1], "}}", sep="")
	}

	filetypes <- .trimWhiteSpace(filetypes)
	if (open)
		return(tclvalue(tkgetOpenFile(initialfile=initialfile, filetypes=filetypes)))
	else
		return(tclvalue(tkgetSaveFile(initialfile=initialfile, filetypes=filetypes)))
}


# ***********************************************************
# promptSaveFile:
#  exactly the same as promptOpenFile except displays a 
#  save button instead of an open button
# -----------------------------------------------------------
promptSaveFile <- function(initialfile="", filetype=list(c("*", "All Files")), save=TRUE)
{
	return(promptOpenFile(initialfile, filetype, !save))
}


# ***********************************************************
# showArgs:
#  show arguments of a widget definition
# Arguments:
#  widget - only show information about supplied widget
# -----------------------------------------------------------
showArgs <- function(widget="")
{
	x <- .widgetDefs
	if (!missing(widget)) {
		#only display info about ONE widget
		tmp<-x[[widget]]
		x <- list()
		x[[widget]]<-tmp
	}
	z <- names(x)
	for(i in 1:length(x)) { 
		cat(z[i])
		cat("\n-------\n")


	cat(z[i])
	cat(" ")
		for(j in 2:length(x[[i]])) { 
			cat(x[[i]][[j]]$param)
			if (x[[i]][[j]]$required==TRUE) {

			}
			else if (!is.null(x[[i]][[j]]$default)) {
				cat("=")
				if (x[[i]][[j]]$class=="character" || x[[i]][[j]]$class=="characterVector")
					cat('"')
				cat(x[[i]][[j]]$default)
				if (x[[i]][[j]]$class=="character" || x[[i]][[j]]$class=="characterVector")
					cat('"')

			}
			else {
				cat("\n\n")
				stop(paste(z[i],"::",x[[i]][[j]]$param, "is not required, but has no default."))
			}
			cat(" ")
		}

		cat("\n\n")


		for(j in 1:length(x[[i]])) { 
			cat(x[[i]][[j]]$param)
		if (x[[i]][[j]]$required==TRUE) {
				cat("\t")
				cat("(required)")
			}
			cat("\n")
		}
		cat("\n\n")
	}
}


# ***********************************************************
# resetGraph:
#  Resets par() values to R default
# -----------------------------------------------------------
resetGraph <- function()
{
	warn <- options()$warn
	options(warn = -1)
	defaultVals <-
	structure(list(xlog = FALSE, ylog = FALSE, adj = 0.5, ann = TRUE, 
	ask = FALSE, bg = "transparent", bty = "o", cex = 1, cex.axis = 1, 
	cex.lab = 1, cex.main = 1.20000004768372, cex.sub = 1, col = "black", 
	col.axis = "black", col.lab = "black", col.main = "black", 
	col.sub = "black", crt = 0, family = "", 
	fg = "black", fig = c(0, 1, 0, 1), fin = c(7.166665625, 7.166665625
	), font = as.integer(1), font.axis = as.integer(1), font.lab = as.integer(1), 
	font.main = as.integer(2), font.sub = as.integer(1), 
	lab = as.integer(c(5, 5, 7)), las = as.integer(0), lend = "round", 
	lheight = 1, ljoin = "round", lmitre = 10, lty = "solid", 
	lwd = 1, mai = c(0.95625, 0.76875, 0.76875, 0.39375), mar = c(5.1, 
	4.1, 4.1, 2.1), mex = 1, mfcol = as.integer(c(1, 1)), mfg = as.integer(c(1, 
	1, 1, 1)), mfrow = as.integer(c(1, 1)), mgp = c(3, 1, 0), 
	mkh = 0.001, new = FALSE, oma = c(0, 0, 0, 0), omd = c(0, 
	1, 0, 1), omi = c(0, 0, 0, 0), pch = as.integer(1), pin = c(6.004165625, 
	5.441665625), plt = c(0.107267457451665, 0.945058131549147, 
	0.133430251952072, 0.892732542548335), ps = as.integer(12), 
	pty = "m", srt = 0, tck = NA, tcl = -0.5, usr = c(0, 
	1, 0, 1), xaxp = c(0, 1, 5), xaxs = "r", xaxt = "s", xpd = FALSE, 
	yaxp = c(0, 1, 5), yaxs = "r", yaxt = "s"), .Names = c("xlog", 
	"ylog", "adj", "ann", "ask", "bg", "bty", "cex", "cex.axis", 
	"cex.lab", "cex.main", "cex.sub", "col", "col.axis", "col.lab", 
	"col.main", "col.sub", "crt", "family", "fg", "fig", "fin", 
	"font", "font.axis", "font.lab", "font.main", "font.sub", 
	"lab", "las", "lend", "lheight", "ljoin", "lmitre", "lty", "lwd", 
	"mai", "mar", "mex", "mfcol", "mfg", "mfrow", "mgp", "mkh", "new", 
	"oma", "omd", "omi", "pch", "pin", "plt", "ps", "pty", 
	"srt", "tck", "tcl", "usr", "xaxp", "xaxs", "xaxt", "xpd", "yaxp", 
	"yaxs", "yaxt"))
	#if (R.Version()$os!="mingw32")
	#	defaultVals$gamma <- NULL
	frame()
	par(defaultVals)
	frame()
	options(warn = warn)
	invisible()
}

# ***********************************************************
# expandGraph:
#  Tweaks values to expand margins for multiple graphs
# Arguments:
#  mar - margin paramater
#  mgp - margin points
#  ... - additional par settings
# -----------------------------------------------------------
expandGraph <- function(mar=c(4,3,1.2,0.5), mgp=c(1.6,.5,0),...)
{
	par(mar=mar, mgp=mgp, ...)
	invisible()
}

# ***********************************************************
# drawBars:
#  Draw a linear barplot on the current graph
# Arguments:
#  x,y   - data coordintates
#  width - bar width, computed if missing
#  base  - y value of the base of each bar
#  ...   - additional parameters for 'lines'
# -----------------------------------------------------------
drawBars <- function (x, y, width, base = 0, ...) 
{
	nx <- length(x)
	n5 <- 5 * nx
	if ((nx != length(y) || nx == 0)) 
		stop("Inconsistent (x,y)-data.")
	if (missing(width)) {
		width <- ifelse(nx > 1, 0.8 * (x[2] - x[1]), 1)
	}
	if (length(width) == 1) 
		width <- rep(width, nx)
	if (length(base) == 1) 
		base <- rep(base, nx)
	if ((length(width) != nx) || (length(base) != nx)) 
		stop("Inconsistent width or base data.")
	x1 <- numeric(n5)
	y1 <- numeric(n5)
	dx <- width/2
	k <- seq(1:nx)
	x1[5 * k - 4] <- x - dx
	x1[5 * k - 3] <- x - dx
	x1[5 * k - 2] <- x + dx
	x1[5 * k - 1] <- x + dx
	y1[5 * k - 4] <- base
	y1[5 * k - 3] <- y
	y1[5 * k - 2] <- y
	y1[5 * k - 1] <- base
	x1[5 * k] <- NA
	y1[5 * k] <- NA
	xy <- list(x = x1, y = y1)
	lines(xy, ...)
}


# -------------------------------------------------------------
# Function: plotAsp
# --------------
#  Plots x and y vectors with plot() but while maitaining a propper aspect
#
#  Arguments:
#  ---------
#     x      - the x coordinates of points in the plot
#     y      - the y coordinates of points in the plot
#     asp    - the y/x aspect ratio
#     ...    - any arguments to be passed to plot()
# -------------------------------------------------------------
plotAsp <- function(x,y,asp=1,...)
{
	dots <- list(...)

	if (is.null(dots$xlim))
		dots$xlim = range(x)
	if (is.null(dots$ylim))
		dots$ylim = range(y)

	xAxisSize <- abs(dots$xlim[1] - dots$xlim[2])
	yAxisSize <- abs(dots$ylim[1] - dots$ylim[2])

	#leave some room for margins
	width <- par("pin")[1]
	height <- par("pin")[2]

	if (xAxisSize > asp*yAxisSize) {
		#x larger than y
		fact <- xAxisSize/(asp*yAxisSize)
		if (width/fact > height) {
			width <- height * fact
		}
		newMaiTop <- (par("fin")[2] - width/fact)
		newMaiSide <- (par("fin")[1] - width)
	}
	else {
		#y larger than x
		fact <- (asp*yAxisSize) / xAxisSize
		if (height/fact > width) {
			height <- width * fact
		}
		#par(pin=c(height/fact, height))
		newMaiTop <- (par("fin")[2] - height)
		newMaiSide <- (par("fin")[1] - height/fact)
	}

	old_mai <- par()$mai
	par(mai=c(par()$mai[1] + (newMaiTop -par()$mai[1]-par()$mai[3])/2,
	          par()$mai[2] + (newMaiSide-par()$mai[2]-par()$mai[4])/2,
	          par()$mai[3] + (newMaiTop -par()$mai[1]-par()$mai[3])/2,
	          par()$mai[4] + (newMaiSide-par()$mai[2]-par()$mai[4])/2))

	plot(x,y,asp=asp,...) 
	par(mai=old_mai)
}

# -------------------------------------------------------------
# Function: plotCsum
# --------------
#  Plots cumulative frequecy of data
#
#  Arguments:
#  ---------
#     x      - vector of values
#     add    - if TRUE, add cumul. frequency curve to current plot
#     ylim   - limits for y-axis
#     xlab   - label for x-axis
#     ylab   - label for y-axis
# -------------------------------------------------------------
plotCsum <- function (x, add = FALSE, ylim = c(0, 1), xlab = "Measure", ylab = "Cumulative Proportion", ...)
{
	x <- sort(x); n <- length(x); y <- (1:n)/n
	z <- y >= ylim[1] & y <= ylim[2]
	mdx <- median(x, na.rm = TRUE)
	mnx <- mean(x, na.rm = TRUE); mny <- approx(x,y,xout=mnx)$y
	if (!add) {
		resetGraph();
		plot(x[z], y[z], type = "n", xlab = "", ylab = "", las=1, mgp=c(0,.6,0), ...)
	}
	lines(x[z], y[z], col = "blue")
	abline(h = c(0.5,mny), lty = 3, col=1:2)
	abline(v = c(mdx,mnx), lty = 2, col=1:2)
	addLabel(0.95,0.1,paste("Median = (",paste(signif(c(mdx,.5),3),collapse=", "),")"),cex=1,adj=1)
	addLabel(0.95,0.05,paste("Mean = (",paste(signif(c(mnx,mny),3),collapse=", "),")"),cex=1,adj=1,col=2)
	mtext(xlab, side = 1, line = 2.75, cex = 1.5);  mtext(ylab, side = 2, line = 2.5, cex = 1.5)
	invisible(data.frame(x=x,y=y))
}

# ***************** Bubble Plots *************************************
# Function to construct a bubble plot for a matrix z
#
# z:     input matrix
# xval:  x-values for the columns of z
#        if xval=TRUE, first row contains x-values for the columns
# yval:  y-values for the rows of z
#        if yval=TRUE, first column contains y-values for the rows
# dnam:  if TRUE, use dimnames as xval and yval
#        (overwrites previously specified values)
# rpro:  if rpro=TRUE, convert rows to proportions
# cpro:  if cpro=TRUE, convert columns to proportions
# rres:  if rres=TRUE, use row residuals (subtract row means)
# cres:  if cres=TRUE, use column residuals (subtract column means)
# powr:  power tranform; radii proportional to z^powr
#        powr=0.5 gives bubble areas proportional to z
# clrs:  colours used for positive and negative values
# size:  size (inches) of the largest & smallest bubble
# lwd:   line width for drawing circles
# hide0: if TRUE, hide zero-value bubbles
# debug: invoke browser if debug=TRUE
# ...:   further parameters for the plot command (e.g., xlab)
# -----------------------------------------------------------
plotBubbles <- function(z, xval=FALSE, yval=FALSE, dnam=FALSE, rpro=FALSE, 
	cpro=FALSE, rres=FALSE, cres=FALSE, powr=1, size=0.2, lwd=2, 
	clrs=c("black","red","blue"), hide0=FALSE, debug=FALSE, ...) 
{
	dz <- dim(z);  ny <- dz[1];  nx <- dz[2]
	xval1 <- 1:nx;  yval1 <- 1:ny
	nx1 <- nx;  ny1 <- ny

	# If first row contains x-values for columns
	if (mode(xval) == "logical") {
		if (xval[1]) {
			xval1 <- z[1,]; ny1 <- ny - 1; } }
	# If first column contains y-values for rows 
	if (mode(yval) == "logical") {
		if (yval[1]) {
			yval1 <- z[,1]; nx1 <- nx - 1; } }
	xind <- (nx - nx1 + 1):nx
	x2 <- xval1[xind]
	yind <- (ny - ny1 + 1):ny
	y2 <- yval1[yind]
	if ((mode(xval) == "numeric") & (length(xval) == nx1)) 
		x2 <- xval
	if ((mode(yval) == "numeric") & (length(yval) == ny1)) 
		y2 <- yval
	zz <- array(z[yind, xind],dim=c(length(yind),length(xind)),dimnames=dimnames(z)) # new 8/30/07

	# dimnames are to be used to over-ride xval and yval
	if (dnam & !is.null(dimnames(zz))) { # new 8/30/07
		if (!is.null(dimnames(zz)[[2]])) { # new 8/30/07
			xnam <- as.numeric(dimnames(zz)[[2]]);
			if (!any(is.na(xnam)) && (length(xnam)==1 || all(diff(xnam)>0 | all(diff(xnam)<0)))) # new 8/30/07
				x2 <- xnam;  # strictly increasing / decreasing
		}
		if (!is.null(dimnames(zz)[[1]])) { # new 8/30/07
			ynam <- as.numeric(dimnames(zz)[[1]])
			if (!any(is.na(ynam)) && (length(ynam)==1 || all(diff(ynam)>0 | all(diff(ynam)<0)))) # new 8/30/07
				y2 <- ynam;  # strictly increasing / decreasing
		}
	};
	xx <- rep(x2, each = length(y2))
	yy <- rep(y2, length(x2))
	minz <- min(zz,na.rm=TRUE);  maxz <- max(zz,na.rm=TRUE);
	if (rpro | cpro) {
		if (minz < 0) {
			zz <- zz - minz
			minz <- 0
			maxz <- max(zz,na.rm=TRUE) } }
	if (rpro) {
		zs <- apply(zz, 1, sum, na.rm=TRUE)
		zz <- sweep(zz, 1, zs, "/") }
	if (cpro) {
		zs <- apply(zz, 2, sum, na.rm=TRUE)
		zz <- sweep(zz, 2, zs, "/") }
	if (rres) {
		zm <- apply(zz, 1, mean, na.rm=TRUE)
		zz <- sweep(zz, 1, zm, "-") }
	if (cres) {
		zm <- apply(zz, 2, mean, na.rm=TRUE)
		zz <- sweep(zz, 2, zm, "-") }
	zNA <- is.na(zz) | is.nan(zz) | is.infinite(zz); zz[zNA] <- 0;
	z0 <- sign(zz) * abs(zz)^abs(powr)
	z1 <- z3 <- z0;  z1[z0 <= 0] <- NA; z3[z0<0 | z0>0] <- NA;
	z2 <- -z0; z2[z0 >= 0] <- NA;
	za <- max(z0,na.rm=TRUE);  zb <- min(z0,na.rm=TRUE)
	zM <- max(abs(z0))
	sz1 <- max(za * size/zM, 0.001)
	sz2 <- max(-zb * size/zM, 0.001)
	if (debug) browser()
	symbols(xx, yy, circles = as.vector(abs(z0)), inches = size, fg=0, ...)
	if (debug) browser()
	if (!hide0 && !all(is.na(z3))) {
		symbols(xx, yy, circles = as.vector(z3), inches = 0.001, 
			fg = clrs[3], lwd = lwd, add = TRUE, ...) }
	if (!all(is.na(z2))) {
		symbols(xx, yy, circles = as.vector(z2), inches = sz2, 
			fg = clrs[2], lwd = lwd, add = TRUE, ...) }
	if (!all(is.na(z1))) {
		symbols(xx, yy, circles = as.vector(z1), inches = sz1, 
			fg = clrs[1], lwd = lwd, add = TRUE, ...) }
	invisible(z0)
}

# ***********************************************************
# genMatrix:
#  Generate a test matrix for use in plotBubbles
# Arguments:
#  m     - number of rows
#  n     - number of columns
#  mu    - mean value of distribution
#  sigma - std deviation of distribution
# -----------------------------------------------------------
genMatrix <- function (m,n,mu=0,sigma=1)
{
   matrix(rnorm(m*n,mean=mu,sd=sigma), m, n)
}

# ***********************************************************
# addArrows:
#  Calls 'arrows' function using relative (0:1) coordinates
# Arguments:
#  x1 - draw from
#  y1 - draw from
#  x2 - draw to
#  y2 - draw to
#  ... - arguments used by key, such as "lines", "text", or "rectangle"
# -----------------------------------------------------------
addArrows <- function (x1, y1, x2, y2, ...) 
{
	uxy <- par()$usr
	ux1 <- uxy[1]; ux2 <- uxy[2]
	uy1 <- uxy[3]; uy2 <- uxy[4]
	px1 <- ux1 + x1 * (ux2 - ux1)
	px2 <- ux1 + x2 * (ux2 - ux1)
	py1 <- uy1 + y1 * (uy2 - uy1)
	py2 <- uy1 + y2 * (uy2 - uy1)
	if(par()$xlog) { px1 <- 10^px1; px2 <- 10^px2 }
	if(par()$ylog) { py1 <- 10^py1; py2 <- 10^py2 }
	arrows(px1, py1, px2, py2, ...)
	invisible(NULL)
}

# ***********************************************************
# addLegend:
#  Panel key function (Adapted from code by Rob Kronlund)
# Arguments:
#  x,y - label coordinates in the range (0,1); can step outside
#  ... - arguments used by key, such as "lines", "text", or "rectangle"
# Result: label 'key' at (x,y) in current plot
# -----------------------------------------------------------
addLegend <- function (x, y, ...) 
{
	uxy <- par()$usr
	x1 <- uxy[1]; x2 <- uxy[2]
	y1 <- uxy[3]; y2 <- uxy[4]
	x0 <- x1 + x * (x2 - x1)
	y0 <- y1 + y * (y2 - y1)
	if(par()$xlog) x0 <- 10^x0
	if(par()$ylog) y0 <- 10^y0
	legend(x0, y0, ...)
	invisible(NULL)
}

# ***********************************************************
# addLabel:
#  Panel label function (Adapted from code by Rob Kronlund)
# Input:
#   x,y - label coordinates in the range (0,1); can step outside
#   txt - desired label at (x,y)
#   ... - arguments used by text, such as "adj", "cex", or "col"
# Result: label 'txt' at (x,y) in current plot
# -----------------------------------------------------------
addLabel <- function (x, y, txt, ...) {
	uxy <- par()$usr
	x1 <- uxy[1]; x2 <- uxy[2]
	y1 <- uxy[3]; y2 <- uxy[4]
	x0 <- x1 + x * (x2 - x1)
	y0 <- y1 + y * (y2 - y1)
	if(par()$xlog) x0 <- 10^x0
	if(par()$ylog) y0 <- 10^y0
	text(x0, y0, txt, ...)
	invisible()
}

# ***********************************************************
# pickCol:
#  display interactive colour picking palette
# Arguments:
#  returnValue - if T, user only selects one colour which is returned
#                if F, intermediate GUI is used to display HEX number
# -----------------------------------------------------------
pickCol <- function(returnValue=TRUE) {
	#simply return the first selected value
	if (returnValue)
		return(tclvalue(.Tcl(paste("tk_chooseColor", .Tcl.args(title="Choose a colour")))))

	#otherwise have an intermediate window to display colour codes in

	tt <- tktoplevel()
	tkwm.title(tt,"pickCol()")
	colour <- "#8cda36"
	entryVar<-tclVar(colour)
	entry <- tkentry(tt,textvariable=entryVar, width=8,bg=colour,fg="#000000")
	.changeColour <- function()
	{
		#launch colour picker
		colour <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=colour,title="Choose a colour"))))
		tmp <- col2rgb(colour)
		#pick white or black foreground colour
		#255*3/2=382.5
		if (sum(tmp)>382 || tmp[2]>180)
			colourFG <- "#000000"
		else
			colourFG <- "#FFFFFF"

		if (nchar(colour)>0) {
			tkconfigure(entry,bg=colour,fg=colourFG)
			tclvalue(entryVar) <- colour
		}
	}
	button <- tkbutton(tt,text="Pick Colour",command=.changeColour)
	tkgrid(entry,button)
}

# ***********************************************************
# testLty:
#  Display line types available
# Arguments:
#  newframe - if T, clear graphics frame, if F, overlay
# -----------------------------------------------------------
testLty <- function (newframe = TRUE) 
{
	if (newframe) 
		frame()
	par0 <- par(no.readonly = TRUE)
	par(usr = c(c(1, 20), c(0, 1)))
	for (i in 1:20) lines(c(i, i), c(0, 1), lty = i)
	mtext(as.character(1:20), side = 1, line = 1, at = (1:20))
	mtext("LINE TYPES (lty)", side = 3, line = 2)
	par(par0)
	invisible(NULL)
}

# ***********************************************************
# testLwd:
#  Display line widths
# Arguments:
#  lwd      - line widths to test
#  col      - colours to use
#  newframe - if T, use a new graphics frame, if F, overlay
# -----------------------------------------------------------
testLwd <- function (lwd=1:20, col=c("black","blue"), newframe=TRUE)
{
	if (newframe) { resetGraph(); frame(); }
	par0 <- par(no.readonly = TRUE); xlim <- range(lwd);
	nl <- length(lwd); nc <- length(col); col <- rep(col,ceiling(nl/nc));
	par(usr = c(c(xlim[1]-1, xlim[2]+1), c(0, 1)))
	for (i in lwd) lines(c(i, i), c(0, 1), lty = 1, lwd = i, col=col[i-xlim[1]+1])
	mtext(as.character(lwd), side = 1, line = 1, at = lwd)
	mtext(paste("LINE WIDTHS (",xlim[1],"-",xlim[2],")"), side=3, line=2)
	par(par0)
	invisible(NULL)
}


# ***********************************************************
# testPch:
#  Display plotting symbols
# Arguments:
#  pch      - symbols to test
#  ncol     - number of columns to use
#  grid     - display in a grid
#  newframe - if T, use a new graphics frame, if F, overlay
#  bs       - use backslash values if T
# -----------------------------------------------------------
testPch <- function (pch=1:100, ncol=10, grid=TRUE, newframe=TRUE, bs=FALSE)
{
	if (!is.element(ncol,c(2,5,10))) stop("Set ncol to 2 or 5 or 10")
	if (!all(diff(pch)==1)) stop("pch vector must be a continuous increasing integer series")
	if (!bs && (all(pch>255) | any(pch<0))) stop("pch must be in the range 0 - 255")
	if (bs && (all(pch<41) | all(pch>377))) stop("pch must be in the range 41 - 377")
	if (newframe) {
		resetGraph();
		frame(); 
	}
	par0 <- par(no.readonly = TRUE); npch=length(pch);
	xlim <- c(.5,ncol+.5);
	rlim <- floor((pch[c(1,npch)]-1)/ncol); yval <- rlim[1]:rlim[2]
	ylim <- rev(rlim); ylim <- ylim + c(.5,-.5)
	pchy <- pch[is.element(pch,seq(0,1000,ncol))];
	if(length(pchy)<length(yval)) {
		pchy <- c(pchy,floor((pchy[length(pchy)]+ncol)/ncol)*ncol);
	}
	ylab <- pchy - ncol + 1
	par(usr=c(xlim,ylim))
	if (grid) {
		abline(v=seq(.5,ncol+.5,1),h=seq(rlim[1]-.5,rlim[2]+.5,1),col="gray");
	}
	for (i in pch) {
		y <- floor((i - 1)/ncol);
		x <- i - ncol * y;
		if (bs) {
			if (i<41 | i>377 | is.element(i,seq(9,379,10)) | is.element(i,c(90:99,190:199,290:299))) next
			cset <- eval(parse(text=paste("\"\\", i, "\"", sep = "")))
			text(x,y, cset, cex=1.5) 
		}
		else {
			if (i>255 | is.element(i,26:31)) next
			points(x, y, pch = i, cex=1.5)
		}
	}
	mtext(as.character(1:ncol), side=1, line=.5, at=(1:ncol), cex=1.3, col="blue")
	mtext(as.character(1:ncol), side=3, line=.4, at=(1:ncol), cex=1.3, col="blue")
	mtext(ylab, side=2, line=1, at=yval, cex=1.3, col="red",las=1)
	mtext(paste(ifelse(bs,"BACKSLASH","PCH"),"CHARACTERS (",pch[1],"-",pch[npch],")"), side=3, line=2.2, cex=1.2)
	par(par0); invisible(yval);
}

# ----------------------------------------------------------
# pad0 - Takes numbers, converts them to integers then text,
#        and pads them with leading zeroes.
# Arguments:
#    x - Vector of numbers
#    n - Length of padded integer
#    f - Factor of 10 to expand x by
# Note: For meaningful results, n should be at least as
#       large as the order of factored x (x * 10^f).
# ------------------------------------------------------
pad0 <- function (x, n, f = 0) {
	xin <- x; xord <- max(ceiling(log10(abs(x * 10^f))), na.rm = TRUE);
	if (any(max(abs(x * 10^f)) == 10^(-10:10))) xord <- xord + 1;
	if (n < xord) n <- xord; # No padding occurs if n<=xord
	x <- round(x, f) * 10^f; xneg <- x < 0;
	x <- abs(x);  x <- format(x, scientific=FALSE);
	x <- gsub(" ", "", x); nx <- length(x);
	base0 <- rep(paste(rep(0, n), collapse = ""), nx);
	nchr <- nchar(x); ndiff <- n - nchr;
	add0 <- substring(base0, 1, ndiff);
	xnew <- paste(add0, x, sep = "");
	xnew[xneg] <- paste("-", xnew[xneg], sep = "");
	attr(xnew, "input") <- xin; return(xnew); };


#show0	Shows decimal places including zeroes (string)
# -------------------------------------------------------
# Function: show0
# ---------------
#    Return character representation of number with
#    specified decimal places.
#
# Arguments:
# ---------
#   x  - Number as scalar or vector
#   n  - Number of decimal places to show, include zeroes
#   add2int - If TRUE, add zeroes on the end of integers
# -------------------------------------------------------
show0 <- function (x, n, add2int = FALSE) 
{
	x <- as.character(x)
	oldx <- x
	pnt <- regexpr("\\.", x)
	z <- grep(-1, pnt)
	x[z] <- paste(x[z], ".", sep = "")
	pnt[z] <- nchar(x)[z]
	int <- substring(x, 1, pnt)
	end <- substring(x, pnt + 1)
	nx <- length(end)
	base0 <- rep(paste(rep(0, n), collapse = ""), nx)
	nchr <- nchar(end)
	ndiff <- n - nchr
	add0 <- substring(base0, 1, ndiff)
	newx <- paste(int, end, add0, sep = "")
	if (!add2int) 
		newx[z] <- oldx[z]
	return(newx)
}


# --------------------------------------------
# View first/last/random n element/rows of an object.
view <- function (obj, n=5, last=FALSE, random=FALSE, ...) 
{
	getn=function(n,N,last=FALSE,random=FALSE,...) {
		n=min(n,N)
		if (random) return(sample(1:N,n,...)) 
		n1=ifelse(last,N-n+1,1); n2=ifelse(last,N,n)
		return(n1:n2) }
	showVec=function(obj,n,last=FALSE,random=FALSE,...){
		N=length(obj); if (N==0) return("empty vector")
		v.vec=obj[getn(n,N,last,random,...)]
		return(v.vec) }
	showTab=function(obj,n,last=FALSE,random=FALSE,...){
		N=nrow(obj); if (N==0) return("empty table")
		v.tab=obj[getn(n,N,last,random,...),]
		return(v.tab) }
	showLis=function(obj,n,last=FALSE,random=FALSE,...){
		nL=length(obj); if (nL==0) return("empty list")
		v.lis=list()
		if (is.null(names(obj))) ii=1:nL else ii=names(obj)
		for (i in 1:nL) {
			iobj=obj[[i]]
			if (is.data.frame(iobj) || is.matrix(iobj))
				v.lis=c(v.lis,list(showTab(iobj,n,last,random,...)))
			else if (is.list(iobj))
				v.lis=c(v.lis,list(showLis(iobj,n,last,random,...)))
			else if (is.vector(iobj) || is.integer(obj) || is.numeric(obj) || is.character(obj))
				v.lis=c(v.lis,list(showVec(iobj,n,last,random,...)))
			else  v.lis=c(v.lis,list(showAll(iobj))) 
		}
		names(v.lis)=ii; return(v.lis) }
	showAll=function(obj){
		return(obj) }

	if (n==0) return("nada")
	n=abs(n) # coerce to positive
	if (is.data.frame(obj) || is.matrix(obj)) 
		viewed=showTab(obj,n,last,random,...)
	else if (is.list(obj)) 
		viewed=showLis(obj,n,last,random,...)
	else if (is.vector(obj) || is.integer(obj) || is.numeric(obj) || is.character(obj)) 
		viewed=showVec(obj,n,last,random,...)
	else viewed=showAll(obj)
	print(viewed); invisible(viewed)
}


#calcGM	Calculates the geometric mean of a vector of numbers
# --------------------------------------------------
# Function: gm
# ------------
#   Return the geometric mean of a vector of numbers
#
# Arguments:
# ---------
#   x      - Vector of numbers
#   offset - Added value to validate zeroes
#   exzero - If TRUE, exclude zeroes
# --------------------------------------------------
calcGM <- function (x, offset = 0, exzero = TRUE) 
{
	x <- x[!is.na(x)]
	if (exzero) 
		x <- x[x > 0 & !is.na(x)]
	n <- length(x)
	if (n == 0) 
		return(0)
	x <- x + offset
	g <- exp(mean(log(x)))
	return(g)
}


# ***********************************************************
# pause:
#  Pause, typically between graphics displays
# Arguments:
#  s  - string to display to user
# -----------------------------------------------------------
pause <- function (s = "Press <Enter> to continue") 
{
	cat(s)
	readline()
	invisible()
}


# ***********************************************************
# calcFib:
#  calculate a vector containing fibonacci numbers
# Arguments:
#  len    - return the last "len" calculated numbers 
#  n      - calculate the nth number
#  method - use .C, .Call, R code, or closed form
# -----------------------------------------------------------
calcFib <- function(n, len=1, method="C")
{
	if (n<0)
		return(NA)
	if (len>(n+1))
		len <- (n+1)

	switch(casefold(method),
	       c=.fibC(n,len),
	       call=.fibCall(n,len),
	       r=.fibR(n,len),
	       closed=.fibClosedForm(n,len)
	       )
}

.fibCall <- function(n, len=1)
{
	retArr <- numeric(len)
	out <- .Call("fibonacci2", as.integer(n), as.integer(len), PACKAGE="PBSmodelling")
	return(out)
}

.fibC <- function(n, len=1)
{
	retArr <- numeric(len)
	out <- .C("fibonacci", as.integer(n), as.integer(len), as.numeric(retArr), PACKAGE="PBSmodelling")
	x <- out[[3]]
	return(x)
}

.fibR <- function(n, len=1)
{
	retArr <- numeric(len)
	xa <- 0; xb <- 1;

	for(i in 0:n) {
		#init conds: fib(0)=0, fib(1)=1
		if (i <= 1) {
			xn <- i
		}
		#fib(n)=fib(n-1)+fib(n-2)
		else {
			xn <- xa+xb
			xa <- xb
			xb <- xn
		}

		## save results if iteration i is within the 
		## range from n-len to n
		j <- i - n + len;
		if (j>0)
			retArr[j] <- xn;
	}

	return(retArr)
}

.fibClosedForm <- function(n, len=1)
{
	n <- (n-(len-1)):n
	phi <- (1+sqrt(5))/2
	return(round((phi^n - (1-phi)^n)/sqrt(5)))
}
#### FIB functions finished


# ------------------------------------------------------
# Searches all patterns in pat from vec, and returns the
# matched elements in vec.
# Arguments:
#    pat - character vector of patterns to match in vec.
#    vec - character vector where matches are sought.
# ------------------------------------------------------
findPat <- function (pat, vec) 
{
	n <- length(vec)
	z <- NULL
	for (xstr in pat) {
		a <- regexpr(xstr, vec)
		b <- (1:n)[a > 0]
		z <- union(z, b)
	}
	found <- vec[z]
	return(found)
}

# -----------------------------------
# Plotting functions to support BRugs
# -----------------------------------
plotTrace <- function(file,clrs=c("blue","red","green","magenta","navy"),...) {
	if (is.vector(file)) file <- data.frame(x=1:length(file),y=file);
	nc <- ncol(file)
	x  <- file[,1]; xlim <- range(x); ylim <- range(file[,2:nc])
	plot(0,0,xlim=xlim,ylim=ylim,type="n",tck=.03,xlab="",ylab="",las=1,...)
	for (i in 2:nc) {
		y <- file[,i]
		lines(x,y,col=clrs[i-1],...) }; };

plotDens <- function(file,clrs=c("blue","red","green","magenta","navy"),...) {
	if (is.vector(file)) file <- matrix(file,ncol=1);
	nc <- ncol(file)
	dd <- density(unlist(file[,1:nc]),adjust=1.25); xlim <- range(dd$x,na.rm=TRUE); ylim <- range(dd$y,na.rm=TRUE)
	for (i in 1:nc) {
		d <- density(file[,i], adjust=1.25);
		xlim[1] <- min(xlim[1],min(d$x)); xlim[2] <- max(xlim[2],max(d$x));
		ylim[1] <- min(ylim[1],min(d$y)); ylim[2] <- max(ylim[2],max(d$y)); };
	plot(0,0,xlim=xlim,ylim=ylim,type="n",tck=.03,xlab="",ylab="",las=1,...)
	lines(dd$x,dd$y,col="grey",lwd=2)
	for (i in 1:nc) {
		y <- file[,i]; d <- density(y, adjust=1.25)
		lines(d$x,d$y,col=clrs[i],...) }; };

plotACF <- function(file,lags=20,clrs=c("blue","red","green","magenta","navy"),...) {
	if (is.vector(file)) file <- matrix(file,ncol=1);
	nc   <- ncol(file); nch <- nc; nr <- nrow(file); lags <- min(nr-1,lags);
	clim <- qnorm(c(.025,.975))/sqrt(nr);
	acfout <- acf(file,lag.max=lags,plot=FALSE); acfacf <- acfout$acf
	ymin <- min(diag(apply(acfacf,2:3,min)),-.2); ymax <- max(diag(apply(acfacf,2:3,max)));
	xlim <- c(0,lags+.5); ylim <- c(ymin,ymax); ylim[1] <- min(ylim[1],clim[1]); ylim[2] <- max(ylim[2],clim[2]);
	plot(0,0,xlim=xlim,ylim=ylim,type="n",tck=.03,xlab="",ylab="",las=1,...)
	if (lags<=30) axis(1,at=1:30,tcl=.25,label=FALSE);
	abline(h=clim,col="#400080",lty=2);
	for (i in 1:nc) {
		x <- (0:lags)+(i-1)*(.7/nch); y <- acfacf[,i,i];
		lines(x,y,type="h",col=clrs[i],...); };
   abline(h=0,col="grey40",lty=3); box(); };

# ***********************************************************
# testWidgets:
#  Display a "master" GUI that displays other sample GUIs
# -----------------------------------------------------------
testWidgets <- function () {

	.testWidHelper <<- function() {
		getWinVal(scope="L");
		if (getWinAct()[1]=="__USE_EDIT__") {
			if (wtxt=="\n" || wtxt=="No widgets displayed\n")
				return()
			winDesc <- strsplit(wtxt, "\n")[[1]]
			createWin(winDesc, astext=TRUE)
			return()
		}

		if (wN==0) {
			wtxt <- "No widgets displayed";
			closeWin(name="widWin");
		}
		else {
			pckg <- "PBSmodelling"; dnam <- "testWidgets";
			act  <- getWinAct()[1];
			wtmp <- paste(dnam,"/",act,".txt",sep="");
			wnam <- system.file(wtmp,package=pckg)
			wtxt <- paste(readLines(wnam),collapse="\n");
			createWin(wnam);
		}
		setWinVal(list(wtxt=wtxt), winName="testW");
	}
	pckg <- "PBSmodelling"; dnam <- "testWidgets";
	wtmp <- paste(dnam,"/","testWidgetWin.txt",sep="");
	wnam <- system.file(wtmp,package=pckg)
	createWin(wnam);
}

# ***********************************************************
# runDemos:
#  Display a GUI to display something equivalent to R's demo()
# -----------------------------------------------------------
.viewPkgDemo <- function()
{
	act <- getWinAct()[1]
	if (act=="pkg")
		return(runDemos(getWinVal("pkg")$pkg))
	if (act=="demo") {
		demo <- getWinVal("demo")$demo
		source(demo, echo=TRUE, max.deparse.length=100)
		return(invisible(NULL))
	}
	if (act=="source") {
		demo <- getWinVal("demo")$demo
		openFile(demo)
		return(invisible(NULL))
	}
}

# *******************************************
# runDemos:
#  Function to execute on closing runDemos().
# -------------------------------------------
.dClose <- function() {
	act <- getWinAct()[1];
	closeWin();
	setwd(.dwd)
	if (is.null(act) || act=="demo") {
		remove(list = setdiff(ls(pos=1, all=TRUE), .dls), pos = 1);
		remove(list = c(".dwd", ".dls"), pos = 1); }; # final good-bye
	return(); };

# ***********************************************************
# runDemos:
#  Display a GUI to display something equivalent to R's demo()
# -----------------------------------------------------------
runDemos <- function (package) 
{
	if (!exists(".dwd",where=1)) .dwd <<- getwd()
	if (!exists(".dls",where=1)) .dls <<- c(".dls",ls(pos = 1, all = TRUE));
	closeWin();

	x <- demo(package = .packages(all.available = TRUE))
	if (missing(package)) {
		#display a list of packages to choose from
		pkgDemo <- unique(x$results[,"Package"])
		radios <- list(list(list(type="label", text="Select a package to view available demos.", sticky="w", padx=12)))
		i <- 2
		for(pkg in pkgDemo) {
			len <- length(x$results[,"Package"][x$results[,"Package"]==pkg])
			if (len==1)
				items <- "(1 demo)"
			else
				items <- paste("(",len," demos)", sep="")
			radios[[i]] <- list(list(type="radio",
			                    name="pkg",
			                    value=pkg,
			                    text=paste(pkg, items),
			                    mode="character",
			                    sticky="w",
			                    padx=12))
			i <- i+1
		}
		xxy <<- win <- list(title = "R Demos", windowname = "pbs.demo", onclose=".dClose", 
			.widgets = list(list(type="grid", .widgets=c( #mixing the c() and lists() become really akward - watch out!
			#the c() requires an extra level of list() which are later stripped out
			#the reason is because of the way the radios list is merged into this list
			list(list(
			list(type="label", text=paste("R Demos", paste(rep(" ", times=100), collapse="")), font="bold underline", fg="red3", padx=10, sticky="w")
			)),
			radios,
			list(list(
			list(type="button", "function"=".viewPkgDemo", action="pkg", text="View Demos", sticky="w", padx=12)
			))
			))))
		createWin(list(win))
		return(invisible(NULL))
	}

	#display demos from a certain package
	x <- x$results[x$results[,"Package"]==package,]
	radios <- list(list(list(type="label", text="Select a Demo to view.", sticky="w", padx=12)))
	i <- 2
	
	if (is.null(dim(x))) {
		tmp<-names(x)
		dim(x)<-c(1,4)
		colnames(x)<-tmp
	}
	for(j in 1:length(x[,1])) {
		demoDir <- file.path(x[j,"LibPath"], package, "demo")
		path <- tools::list_files_with_type(demoDir, "demo")
		path <- path[x[j,"Item"]==tools::file_path_sans_ext(basename(path))]
		if (length(path)==0)
			stop("error - could not find the path for demo - this is most likely a bug!")
		
		
		radios[[i]] <- list(list(type="radio",
		                    name="demo",
		                    value=path,
		                    text=x[j,"Item"],
		                    mode="character",
		                    font="underline",
		                    sticky="w",
		                    padx=12))
		i <- i+1
		radios[[i]] <- list(list(type="label",
		                    text=x[j,"Title"],
		                    sticky="w",
		                    wraplength=500,
		                    padx=20
		                    ))
		i <- i+1
	}
	xx <<- win <- list(title = paste("R Demos:", package), windowname = "pbs.demo", onclose=".dClose",
		.widgets = list(list(type="grid", .widgets=c(
			list(list(
				list(type="label", text=paste(package, paste(rep(" ", times=100), collapse="")), font="bold underline", fg="red3", sticky="w")
			)),
			radios,
			list(list(list(type="null", pady=4))),
				list(list(
					list(type="grid", sticky="w", pady=3, .widgets=
						list(
							list(
								list(type="button", "function"=".viewPkgDemo", action="demo", text="Run Demo", sticky="w", padx=12),
								list(type="button", "function"=".viewPkgDemo", action="source", text="View Source", sticky="w", padx=12),
								list(type="button", "function"="runDemos", action="", text="All Packages", sticky="w", padx=12)
							)
						)
					)
				))
			)
		)))
	createWin(list(win))
	return(invisible(NULL))
}

#-------------------------------------------------
# Show help files for package contents as HTML in browser window
# Author: Rowan Haigh
#-------------------------------------------------
showHelp <- function(pat="methods") {
	warn <- options()$warn
	options(warn = -1)
	Apacks = .packages(all.available = TRUE) # all packages
	Spacks = findPat(pat,Apacks)             # show packages that match the pattern
	npacks = length(Spacks)
	if (npacks==0) { print("No such package"); return() }
	getURL = function(x) {
		path=system.file(package=x)
		url=paste(path,"/html/00Index.html",sep="")
		return(url) }
	URLs=sapply(Spacks,getURL)
	openFile(URLs)
	options(warn = warn)
	invisible(list(Apacks=Apacks,Spacks=Spacks,URLs=URLs))
}

#-------------------------------------------------
# showVignettes:
#  Display a GUI to display something equivalent to R's vignette()
# Arguments: package = string specifying a package name.
# Author: Anisa Egeli
#-------------------------------------------------
showVignettes <- function (package)
{
	if (!exists(".dwd",where=1)) .dwd <<- getwd()
	if (!exists(".dls",where=1)) .dls <<- c(".dls",ls(pos = 1, all = TRUE));
	closeWin();

	x <- vignette()
	if (missing(package)) {
		#display a list of packages to choose from
		pkgVignette <- unique(x$results[,"Package"])
		radios <- list(list(list(type="label", text="Select a package to view available vignettes.", sticky="w", padx=12)))
		i <- 2
		for(pkg in pkgVignette) {
			len <- length(x$results[,"Package"][x$results[,"Package"]==pkg])
			if (len==1)
				items <- "(1 vignette)"
			else
				items <- paste("(",len," vignettes)", sep="")
			radios[[i]] <- list(list(type="radio",
			                    name="pkg",
			                    value=pkg,
			                    text=paste(pkg, items),
			                    mode="character",
			                    sticky="w",
			                    padx=12))
			i <- i+1
		}
		xxy <<- win <- list(title = "R Vignettes", windowname = "pbs.vignette", onclose=".dClose",
			.widgets = list(list(type="grid", .widgets=c(

			list(list(
			list(type="label", text=paste("R Vignettes", paste(rep(" ", times=100), collapse="")), font="bold underline", fg="red3", padx=10, sticky="w")
			)),
			radios,
			list(list(
			list(type="button", "function"=".viewPkgVignette", action="pkg", text="View Vignettes", sticky="w", padx=12)
			))
			))))
		createWin(list(win))
		return(invisible(NULL))
	}

	#display vignettes from a certain package
	x <- x$results[x$results[,"Package"]==package,]
	radios <- list(list(list(type="label", text="Select a Vignette to view.", sticky="w", padx=12)))
	i <- 2

	if (is.null(dim(x))) {
		tmp<-names(x)
		dim(x)<-c(1,4)
		colnames(x)<-tmp
	}
	for(j in 1:length(x[,1])) {
		vignetteDir <- file.path(x[j,"LibPath"], package, "doc")
		path <- tools::list_files_with_type(vignetteDir, "vignette")
		path <- path[x[j,"Item"]==tools::file_path_sans_ext(basename(path))]

		if (length(path)==0)
			stop("error - could not find the path for vignette - this is most likely a bug!")

		radios[[i]] <- list(list(type="radio",
		                    name="vignette",
		                    value=path,
		                    text=x[j,"Item"],
		                    mode="character",
		                    font="underline",
		                    sticky="w",
		                    padx=12))
		i <- i+1
		radios[[i]] <- list(list(type="label",
		                    text=x[j,"Title"],
		                    sticky="w",
		                    wraplength=500,
		                    padx=20
		                    ))
		i <- i+1
	}
	xx <<- win <- list(title = paste("R Vignettes:", package), windowname = "pbs.vignette", onclose=".dClose",
		.widgets = list(list(type="grid", .widgets=c(
			list(list(
				list(type="label", text=paste(package, paste(rep(" ", times=100), collapse="")), font="bold underline", fg="red3", sticky="w")
			)),
			radios,
			list(list(list(type="null", pady=4))),
				list(list(
					list(type="grid", sticky="w", pady=3, .widgets=
						list(
							list(
								list(type="button", "function"=".viewPkgVignette", action="vignette", text="View Vignette", sticky="w", padx=12),
								list(type="button", "function"=".viewPkgVignette", action="source", text="View Source", sticky="w", padx=12),
								list(type="button", "function"="showVignettes", action="", text="All Packages", sticky="w", padx=12)
							)
						)
					)
				))
			)
		)))
	createWin(list(win))
	return(invisible(NULL))
}

#-------------------------------------------------
# .viewPkgVignettes:
#  Display a GUI to display something equivalent to R's vignette()
#-------------------------------------------------
.viewPkgVignette <- function()
{
	act <- getWinAct()[1]
	if (act=="pkg")
		return(showVignettes(getWinVal("pkg")$pkg))
	vignette <- getWinVal("vignette")$vignette
	if (act=="vignette")
		 vignette <- paste(tools::file_path_sans_ext(vignette), ".pdf", sep="")
	openFile(vignette)
	return(invisible(NULL))
}

#-------------------------------------------------
# writeList:
#  writes a list to a file in "D" or "P" format
# Arguments:
#  x        - list to save
#  fname    - file to write list to
#  format   - write list in "D" or "P" format
#  comments - include string as comment at the top of file
# Change (Anisa Egeli): Function previously crashed R if an empty 
#   list was given for the first argument. This function is the same 
#   except that now there is a check, and the function will stop if
#   the list is empty. (The error message is changed appropriately)
#-------------------------------------------------
writeList <- function(x, fname="", format="D", comments="")
{
	NoComments<-missing(comments)
	comments <- sub("^", "#", comments)

	if (format=="D") {
		dput(x, fname)
		if (file.exists(fname) && !NoComments) {
			output <- scan(fname, what=character(0), sep="\n")
			output <- paste(output, collapse="\n")

			sink(fname)
				#add comments
				if (all(comments!="#")) {
					cat(paste(comments, collapse="\n")); cat("\n");
				}
				#spit out original output from dput
				cat(output)
				cat("\n")
			sink()
		}
		return(fname)
	}
	if (format=="P") {
		if (!is.list(x) || !length(x))
			stop("x must be a non-empty list.")
		.writeList.P(x, fname, comments)
		return(fname)
	}
	stop(paste("format \"",format,"\" not recognized."))
}

#-------------------------------------------------
# readList:
#  returns a list in either "D" or "P" format read from disk
# Arguments:
#  fname - file to read
# Change (Anisa Egeli): There is a check to see if the file exists. 
#   This allows try(readList(...), silent=TRUE) to catch the error.
#-------------------------------------------------
readList <- function(fname)
{
	if(!file.exists(fname))
		stop(paste("File", fname, "does not exist."))

	#detect file type
	f <- scan(fname, what=character(), sep="\n", quiet=TRUE)
	for(i in 1:length(f)) {
		if (!any(grep("^[ \t]*#", f[i]))) {

			if (any(grep("^[ \t]*structure", f[i])))
				fileformat <- "D"
			else if (any(grep("^[ \t]*list", f[i])))
				fileformat <- "R"
			else if (any(grep("^[ \t]*\\$", f[i])))
				fileformat <- "P"
			else
				stop("unknown fileformat detected.")
			break;
		}
	}
	if (fileformat == "R" || fileformat == "D") {
		return(eval(parse(fname)))
	}
	if (fileformat == "P") {
		return(.readList.P(fname))
	}
}


#2008-04-29-------------------------------------JS
# Show results of the calculation in string x
#-------------------------------------------------
showRes <- function(x, cr=TRUE, pau=TRUE) {
  cat(">",x," "); if(cr==TRUE) cat("\n");
  if(pau) pause("...") else cat("...\n");
  xres <- eval(parse(text=x));
  print(xres); 
  cat("************** Results shown above **************\n\n");
  if(pau) pause();
  invisible(xres); };

#2008-08-28-------------------------------------AE
# Remove all data in the global environment
# Arguments:
#  hidden  - if T remove all variables including dot variables
#  verbose - list all removed variables
#  PBSsave - if TRUE, do not remove .PBSmod
# ------------------------------------------------
clearAll <- function(hidden=TRUE, verbose=TRUE, PBSsave=TRUE) {
	objs <- ls(all.names = TRUE, pos = ".GlobalEnv")
	if (verbose && length(objs))
		print(objs)
	rmlist <- ls(all.names = hidden, pos = ".GlobalEnv")
	if(PBSsave)
		rmlist=rmlist[rmlist!=".PBSmod"]
	rm(list = rmlist, pos = ".GlobalEnv")
	if (verbose) {
		cat("Removed:\n")
		if (length(rmlist))
			print(rmlist)
		else
			cat("\n")

		cat("Remaining:\n")
		if (length(ls(all.names = TRUE, pos = ".GlobalEnv")))
			print(ls(all.names = TRUE, pos = ".GlobalEnv"))
		else
			cat("\n")
	}
	invisible()
}

#2008-09-03----------------------------------SM/RH
#  Pairs plot featuring fried eggs and beer.
#  Original code by Steve Martell (UBC).
#-------------------------------------------------
plotFriedEggs <- function(A, eggs=TRUE, rings=TRUE,
		levs=c(0.01,0.1,0.5,0.75,0.95), pepper=200, replace=FALSE,
		jitt=c(1,1), bw=25, histclr=NULL) {
	require(KernSmooth)
	expandGraph(las=1,mgp=c(0,.75,0))

	panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
		usr <- par("usr"); on.exit(par(usr))
		par(usr = c(0, 1, 0, 1))
		r <- (cor(x, y))
		txt <- format(c(r, 0.123456789), digits=digits)[1]
		txt <- paste(prefix, txt, sep="")
		if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
		#text(0.5, 0.5, txt, cex = cex * r)
		beer(round(r,2)) }

	panel.hist <- function(x) {
		usr <- par("usr"); on.exit(par(usr))
		par(usr = c(usr[1:2], 0, 1.25))
		h <- hist(x,breaks=20,plot=FALSE)
		breaks <- h$breaks; nB <- length(breaks)
		y <- h$counts; y <- y/max(y)
		if (!is.null(histclr)) {clrs=histclr; bord=1}
		else if (eggs) {clrs=c("moccasin","burlywood"); bord="saddlebrown" }
		else if (rings) {clrs=c("lightsteelblue1","steelblue"); bord="darkblue" }
		else {clrs=c("grey85","grey40"); bord="black" }
		rect(breaks[-nB],0,breaks[-1],y,col=clrs,border=bord);  box() }

	fried.eggs <- function(x, y) {
		bwx=(max(x)-min(x))/bw; bwy=(max(y)-min(y))/bw
		est <- bkde2D(cbind(x,y),bandwidth=c(bwx,bwy),gridsize=c(51, 51))
		est$fhat=est$fhat/max(est$fhat)
		levs=sort(levs); maxct=max(levs)
		nlev=length(levs); is.white=rev(is.element(nlev:1,seq(nlev,1,-2))); is.yolk=!is.white
		thelines=contourLines(est$x1,est$x2,est$fhat,levels=levs)

		crap=colorRamp(c("sandybrown","gold","white"),space="Lab")
		yolks=apply(crap(seq(.3,.8,len=nlev)),1,function(x){rgb(x[1],x[2],x[3],maxColorValue=255)})
		crap=colorRamp(c("snow","whitesmoke"),space="Lab")
		whites=apply(crap(seq(0,1,len=nlev)),1,function(x){rgb(x[1],x[2],x[3],maxColorValue=255)})

		for (i in 1:(nlev-1)) {
			ii=nlev-i+1
			if (is.yolk[i]) clr=yolks else clr=whites
			polygon(thelines[[i]]$x,thelines[[i]]$y,col=clr[ii],border="grey",lwd=1) }
		if(pepper>0) pepper.mill(x,y,scale=0:7/10)
		polygon(thelines[[nlev]]$x,thelines[[nlev]]$y,col="white",border="grey",lwd=1)
		box() }

	smoke.rings <- function(x, y) {
		bwx=(max(x)-min(x))/bw; bwy=(max(y)-min(y))/bw
		est <- bkde2D(cbind(x,y),bandwidth=c(bwx,bwy),gridsize=c(51, 51))
		est$fhat=est$fhat/max(est$fhat)
		levs=sort(levs); maxct=max(levs)
		nlev=length(levs)
		points(x,y,pch=20,col="grey",cex=0.2)
		crap=colorRamp(c("white","cornflowerblue","black"),space="Lab") # smoke ring ramp function bounded by white and black
		clrs=apply(crap(seq(.2,.7,len=nlev)),1,function(x){rgb(x[1],x[2],x[3],maxColorValue=255)})
		if (pepper>0) pepper.mill(x,y,scale=0)
		contour(est$x1,est$x2,est$fhat,drawlabels=FALSE,add=TRUE,levels=levs,lwd=2,col=clrs)
		box() }

	pepper.mill = function (x,y,scale=0:9/10) { # grind some pepper
		N=length(x)
		xi=sample(1:N,pepper,replace=ifelse(pepper>N,TRUE,replace))
		points(jitter(x[xi],factor=jitt[1]),jitter(y[xi],factor=ifelse(is.na(jitt[2]),jitt[1],jitt[2])),
			pch=20,cex=0.5,col=grey(scale)) }

	beer <- function(v, col.glass=c("#C6D5D8","#6D939E")) {
		usr=par("usr")
		ymin=usr[3]; ymax=usr[4]; yrng=ymax-ymin
		ymin1=usr[3]+.1*yrng
		ymax1=usr[4]-.2*yrng
		yrng1=ymax1-ymin1
		ymax2=ymin1+abs(v)*yrng1
		xmid=(usr[2]+usr[1])/2
		ymid=(ymax1+ymin1)/2
		xrng=(usr[2]-usr[1])
		Lbot=xmid-.15*xrng; Rbot=xmid+.15*xrng; Ltop=xmid-.25*xrng; Rtop=xmid+.25*xrng             # sides of glass
		Lbeer=xmid-(.15+abs(v)*.1)*xrng;  Rbeer=xmid+(.15+abs(v)*.1)*xrng                          # top of beer

		curved=function(x,yval,scale=0.2) {
			xmin=min(x,na.rm=TRUE); xmax=max(x,na.rm=TRUE)
			s=(2/pi)*asin(sqrt((x-xmin)/(xmax-xmin))) # scale between 0 and 1
			smid=mean(s,na.rm=TRUE)
			ycur=scale/cos(s-smid); ycur=yval+ycur-max(ycur,na.rm=TRUE); return(ycur) }

		xbot=seq(Lbot,Rbot,len=100); ybot=curved(xbot,ymin1) # bottom of glass
		xtop=seq(Ltop,Rtop,len=100); ytop=curved(xtop,ymax1) # top of glass
		xglass=c(xbot,rev(xtop)); yglass=c(ybot,rev(ytop))   # curved glass poly

		#xbeer=c(Lbot,Lbeer,Rbeer,Rbot,Lbot); ybeer=c(ymin1,ymax2,ymax2,ymin1,ymin1) # rectangular beer poly
		xsip=seq(Lbeer,Rbeer,len=100); 
		if (v==0) ysip=ybot else ysip=curved(xsip,ymax2)  # top of beer
		xbeer=c(xbot,rev(xsip)); ybeer=c(ybot,rev(ysip))        # curved beer poly
		bubblex=runif(round(500*abs(v),0),xmid-(.15+abs(v*.95)*.1)*xrng,xmid+(.15+abs(v*.95)*.1)*xrng)
		if (v==0) yfroth=ybot else yfroth=curved(bubblex,ymax2)
		#bubbley=runif(round(500*abs(v),0),ymax2-.02*yrng1,ymax2+.02*yrng1)
		bubbley=runif(round(500*abs(v),0),yfroth-.02*yrng1,yfroth+.02*yrng1)

		polygon(xglass,yglass,,col="aliceblue",border=FALSE)              # glass surface
		lines(xtop,ymax1+abs(ytop-ymax1),lwd=2,col=col.glass[1])          # top back rim
		if (v!=0) {
			polygon(xbeer,ybeer,col=ifelse(v<0,"orange","gold"),border="burlywood")                # beer
			points(bubblex,bubbley,pch=21,col=ifelse(v<0,"orange","gold"),bg="white",cex=seq(0.1,1,length=10)) }
		lines(xbot,ybot,lwd=3,col=col.glass[1])                           # bottom edge
		lines(xbot,ybot-.005,lwd=1,col=col.glass[2])                      # bottom edge shadow
		lines(c(Lbot,Ltop),c(ymin1,ymax1),lwd=4,col=col.glass[1])         # left edge
		lines(c(Lbot-.01,Ltop-.01),c(ymin1,ymax1),lwd=1,col=col.glass[2]) # left edge shadow
		lines(c(Rbot,Rtop),c(ymin1,ymax1),lwd=4,col=col.glass[1])         # right edge
		lines(c(Rbot+.01,Rtop+.01),c(ymin1,ymax1),lwd=1,col=col.glass[2]) # right edge shadow
		lines(xtop,ytop,lwd=3,col=col.glass[1])                           # top front rim
		text(xmid,ymid,labels=c(paste(ifelse(v<0,"-","+"),abs(v))),cex=abs(v*100)^.1)
	}
	if (eggs) lower=fried.eggs else if (rings) lower=smoke.rings else lower=pepper.mill
	pairs(A,lower.panel=lower,diag.panel=panel.hist,upper.panel=panel.cor,gap=0,label.pos=0.92)
}

#2008-09-08-------------------------------------RH
# Display test colours as circular patches
# Arguments:
#  cnam - colour names to search for
# ------------------------------------------------
testCol <- function(cnam=colors()[sample(length(colors()),15)]) {

	#get similar colours
	getCol <- function(x) {
		palette <- colors()
		n <- length(palette)
		z <- NULL
		for (i in x) {
			a <- regexpr(i,palette)
			b <- (1:n)[a>0]
			z <- union(z,b)
		}
		lovely <- palette[z]
		return(lovely)
	}
 
	clrs <- getCol(grep("^[^#0-9]", cnam,value=TRUE))
	clrs <- c(clrs, 
	          grep("^#[0-9a-f]{6}$", cnam, value=TRUE, ignore.case=TRUE), 
	          grep("^[0-9]+$", cnam, value=TRUE, ignore.case=TRUE)
	          )
	# fiddle for mfrow
	N <- length(clrs); din <- par()$din; x <- din[1]; y <- din[2]
	cell <- sqrt(prod(din)/N)
	cols <- ceiling(x/cell); rows <- ceiling(y/cell)
	if (N <= rows*cols-cols) rows <- rows-1

	par0 <- par(no.readonly = TRUE)
	xlim <- c(1, cols) + c(-.25,.25); ylim <- c(-rows,-1) + c(-.25,.25)

	resetGraph()
	par(mfrow=c(1,1),mai=c(.05,.05,.05,.05))
	plot(0,0,xlim=xlim,ylim=ylim,type="n",axes=FALSE,xlab="",ylab="")
	k <- 0
	for (i in 1:rows) {
		for (j in 1:cols) {
			k <- k+1
			points(j,-i, col=clrs[k], pch=16,cex=5)
			text(j,-i-.04*diff(ylim),clrs[k],cex=.6) } }
	par(par0)
	invisible(clrs)
}

#2008-10-24-------------------------------------RH; Rev. JTS
#  Display a master GUI to display examples
#-------------------------------------------------
runExamples <- function () {
	allWin=c("runE","window","widWin","testW","choisir","Swiss") # all potential windows open
	.runExHelper <<- function() {
		getWinVal(scope = "L")
		act <- getWinAct()[1]
		if (!exists("act") || !exists("eN")) return()
		if (act == "quit") { 
			.runExHelperQuit()
		} else if (act=="clear") {
			wtxt <- "No examples chosen"
			closeWin(name=setdiff(allWin,"runE"))
		} else if (act == "__USE_EDIT__") {
			if (wtxt == "\n" || wtxt == "No examples chosen\n") return()
			winDesc <- strsplit(wtxt, "\n")[[1]]
			createWin(winDesc, astext = TRUE)
			return()
		} else if (act=="SwissTalk") {
			closeWin(name=setdiff(allWin,"runE"))
			tnam=paste(act,".txt",sep="") # talk description file
			wtxt <- paste(readLines(tnam), collapse = "\n")
			presentTalk(tnam)
		} else {
			if (act!="TestFuns")
				closeWin(name=setdiff(allWin,c("runE","window")))
			source(paste(act, ".r", sep = ""))
			wnam <- paste(act, "Win.txt", sep = "") # window description file
			wtxt <- paste(readLines(wnam), collapse = "\n")
		}
		setWinVal(list(wtxt=wtxt), winName="runE")
	}
	.runExHelperQuit <<- function() {
		closeWin(name=allWin)
		setwd(.cwd)
		remove(list = setdiff(ls(pos = 1), .cls), pos = 1)
		return()
	}
	.cls <<- ls(pos = 1, all = TRUE)
	.cwd <<- getwd()
	pckg <- "PBSmodelling"
	dnam <- "examples"
	rdir <- system.file(package = pckg)
	wdir <- paste(rdir, "/", dnam, sep = "")
	fnam <- paste(wdir, list.files(wdir), sep = "/")
	rtmp <- tempdir()
	file.copy(fnam, rtmp, overwrite = TRUE)
	setwd(rtmp)
	createWin("runExamplesWin.txt")
	msg <- paste("Examples are running in ", rtmp, sep = "")
	setWinVal(list(wtxt = msg), winName = "runE")
}

#2008-09-08-------------------------------------JS
#  Prints the class, mode, type, and attributes
#  of the given object x.
#-------------------------------------------------
isWhat <- function(x) {
  cat("class: "); print(class(x));
  cat("mode:  "); print(mode(x));
  cat("type:  "); print(typeof(x));
  att=attributes(x)
  cat(paste("attributes:",ifelse(is.null(att)," NULL\n","\n"),sep=""))
  if (!is.null(att)) print(att)
  invisible() }

#2008-09-22------------------------------------ACB
# Opens a file for viewing based on System file
# extension association or .PBSmod$.options$openfile
#-------------------------------------------------
openFile <- function(fname="") {
	.openFile=function(fname) {
		if (!exists(".PBSmod"))  .initPBSoptions()
		if (fname=="")  fname=getWinAct()[1]
		if (any(grep("^~", fname)))
			fname <- path.expand(fname)
		else if (!any(grep("^([a-z]:(\\\\|/)|\\\\\\\\|/)", fname, ignore.case = TRUE)))
			fname <- paste(getwd(), "/", fname, sep="")
		if (!file.exists(fname))
			stop(paste("File \"", fname, "\" does not exist", sep=""))

		ext <- sub("^.*\\.", "", fname)
		if (.isReallyNull(.PBSmod$.options$openfile, ext)) {
			if (!exists("shell.exec", mode="function")) 
				stop(paste("There is no program associated with the extension '", ext, "'\n",
				           "Please set an association with the setPBSext command\n"))
			shell.exec(fname); return(fname)
		} else {
			cmd <- getPBSext(ext)
			cmd <- gsub("%f", fname, cmd)
			if (.Platform$OS.type=="windows")
				shell(cmd,wait=FALSE)
			else
				system(cmd,wait=FALSE)
			return(cmd)
		}
	}
	ops=sapply(fname,.openFile)
	invisible(ops)
}

#2008-10-02------------------------------ACB/AE/RH
# Change user options. Arguments:
#   option - name of option to change
#   value  - new value of option
# AE: Now a value '.PBSmod$.options$.optionsChanged' is set to TRUE when an option is changed,
#   so that the user doesn't always have to be prompted to save the options file.
#   By default, '.PBSmod$.options$.optionsChanged' is not set or NULL.
#   Also, if an option is set to "" or NULL then it is removed.
#   '.initPBSoptions()' is now called first (options starting with a dot "." do not set '.optionsChanged').
# RH: if the value is a sublist of an option, it can be changed individually using 'sublist=TRUE'.
#Start setPBSoptions------------------------------
setPBSoptions <- function(option, value, sublist=FALSE) {
	.initPBSoptions()
	if(!is.null(value) && length(value)==1 && value=="") value=NULL
 	if(substr(option, 1, 1)!="." && !identical(.PBSmod$.options[[option]], value))
		.PBSmod$.options$.optionsChanged<<-TRUE
	if(is.null(value) && !sublist)
		.PBSmod$.options <<- .removeFromList(.PBSmod$.options, option)
	else{
		if(is.list(value) && sublist){
			for (i in 1:length(value)){
				ii=names(value[i]); if (ii=="") next
				ival=value[[i]]
				txt=paste(".PBSmod$.options$",option,ifelse(ii=="","","$"),ii," <<- ival",sep="")
				eval(parse(text=txt))
			}
		}
		else .PBSmod$.options[[option]] <<- value
	}
}
#End setPBSoptions--------------------------------

#2008-10-06----------------------------------AE/RH
# Remove items from a list.
#Start .removeFromList----------------------------
.removeFromList = function (l, items) 
{
	if (!length(l) || !length(items))  return(l)
	keep = l[!is.element(names(l),items)]
	return(keep) }
#End .removeFom List------------------------------

#200x-xx-xx-------------------------------------AE
# Called from zzz.R's .First.lib() intialization function
#Start .initPBSoptions----------------------------
.initPBSoptions <- function() {
	if (!exists(".PBSmod"))
		.PBSmod <<- list()
	if (is.null(.PBSmod$.options))
		.PBSmod$.options <<- list()
	if (is.null(.PBSmod$.options$openfile))
		.PBSmod$.options$openfile <<- list()
}
#End .initPBSoptions------------------------------

#2000x-xx-xx-----------------------------------ACB
# Retrieve a user option.  Argument:
#   option - name of option to retrieve
#Start getPBSoptions------------------------------
getPBSoptions <- function(option) {
	if (missing(option))
		return(.PBSmod$.options)
	return(.PBSmod$.options[[option]])
}
#End getPBSoptions--------------------------------

#2000x-xx-xx-----------------------------------ACB
# Retrieve previously saved command.  Argument:
#  ext - file extension
#Start getPBSext----------------------------------
getPBSext <- function(ext) {
	if (!exists(".PBSmod"))
		stop(".PBSmod was not found")
	if (missing(ext))
		return(.PBSmod$.options$openfile)
	if (.isReallyNull(.PBSmod$.options$openfile, ext))
		return(NULL)
	return(.PBSmod$.options$openfile[[ext]])
}
#End getPBSext------------------------------------

#20008-07-31--------------------------------ACB/AE
# Associate a new command with file types;
#  use "%f" in cmd to designate where the filename will be placed.
# AE: Added the setting of 'optionsChanged'
# Arguments:
#  ext - file extension
#  cmd - cmd to open these types of files
#Start setPBSext----------------------------------
setPBSext <- function(ext, cmd) {
	if (!exists(".PBSmod")) 
		stop(".PBSmod was not found")
	if (!any(grep("%f", cmd)))
		stop(paste("No %f was found in supplied command \"", cmd, 
		           "\".\n%f must be used to indicate where the filename will ",
		           "be inserted by openfile().\n",
		           "Did you mean \"", cmd, " %f\"?", sep=""))
		           
	if(is.null(.PBSmod$.options$openfile[[ext]]) ||
			.PBSmod$.options$openfile[[ext]]!=cmd)
		.PBSmod$.options.optionsChanged<<-TRUE
		
	.PBSmod$.options$openfile[[ext]] <<- cmd
}
#End setPBSext------------------------------------

#2008-07-21-------------------------------------AE
# Disassociate any number of file extensions from commands
#  previously save with setPBSext.
# Argument:
#  ext - optional character vector of file extensions to
#        clear; if unspecified, all associations are removed
#Start clearPBSext--------------------------------
clearPBSext=function(ext){
  .initPBSoptions()
  if(missing(ext))
    .PBSmod$.options$openfile<<-list()
  else{
    oldLen=length(.PBSmod$.options$openfile)
    .PBSmod$.options$openfile<<-.removeFromList(.PBSmod$.options$openfile, ext)
    if(oldLen!=length(.PBSmod$.options$openfile))
      .PBSmod$.options$.optionsChanged<<-TRUE
  }
}
#End clearPBSext----------------------------------


# ***********************************************************
# writePBSoptions:
#  Save PBS options to a text file
# Input:
#  fname - name of options file (or path to this file)
# -----------------------------------------------------------
writePBSoptions=function(fname="PBSoptions.txt"){
  .initPBSoptions()
  
  if(fname!="PBSoptions.txt")
    .PBSmod$.options$.optionsFile<<-fname
    
	.PBSmod$.options$.optionsChanged<<-NULL
	
	saveOpt=.PBSmod$.options[-grep("^[.]", names(.PBSmod$.options))]
  writeList(saveOpt, fname)
}

# ***********************************************************
# readPBSoptions:
#  Load PBS options from a text file. The loaded options will
#  overwrite existing ones in memory; however, an existing
#  option in memory will not be cleared if this option does
#  not exist in the options file.
# Input:
#  fname - name of options file (or path to this file)
# Output:
#   returns FALSE if file did not exist or if read failed
#   otherwise returns TRUE
# -----------------------------------------------------------
readPBSoptions=function(fname="PBSoptions.txt"){
  .initPBSoptions()
     
  optList=try(readList(fname), silent=TRUE)
  if(class(optList)=="try-error")
    return(FALSE)
    
  .PBSmod$.options<<-.mergeLists(.PBSmod$.options, optList)
  if(fname!="PBSoptions.txt")
    .PBSmod$.options$.optionsFile<<-fname
  .PBSmod$.options$.optionsChanged<<-NULL
}
