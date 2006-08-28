############################################################
#                      PBS Modelling                       #
# ---------------------------------------------------------#
# This file aims to include functions specific to          #
# createWin and other GUI functions                        #
#                                                          #
# Authors:                                                 # 
#  Jon T. Schnute <SchnuteJ@pac.dfo-mpo.gc.ca>,            #
#  Alex Couture-Beil <alex@mofo.ca>, and                   #
#  Rowan Haigh <HaighR@pac.dfo-mpo.gc.ca>                  #
#                                                          #
############################################################




# ***********************************************************
# .trimWhiteSpace:
#  remove leading and trailing whitespace
# Arguments:
#  x - string to trim
# Example:
#  "   foo bar " becomes "foo bar"
# -----------------------------------------------------------
.trimWhiteSpace <- function(x)
{
	return(sub("[[:space:]]+$", "", sub("^[[:space:]]+", "", x)))
}


# ***********************************************************
# .stripComments:
#  removes any trailing comments from a line, 
#  but ignores #'s in quoted strings
# Arguments:
#  x - a string with or without comments
# Output:
#  string without comments
# Example: 
#   x='type="label" text="I am #1" #comment'
#   returns 'type="label" text="I am #1"'
# -----------------------------------------------------------
.stripComments <- function(x)
{
	if (length(x)>1) {
		retVal <- c()
		for(i in 1:length(x)) {
			retVal[i] <- .stripComments(x[i])
		}
		return(retVal)
	}
	escape<-0
	quoted<-0	#0=none, 1=", 2='

	
	#used for removing comments in strings that wrap lines
	#ex: foo="a string \    #comment time "more comments"
	wrapelineBackslash<-0
	word<-""
	quotefound <- 0 #used to capture ""
	x<-.trimWhiteSpace(x)
	
	
	for(i in 1:nchar(x)) {
		c<-substr(x,i,i)
		
		#escaped char is expected
		if (escape!=0) {
			escape <- 0
			if (c==" "||c=="\t")
				wrapelineBackslash <- 1
			if (c=="#")
				return(substr(x, 1, i-1))
		}
		
		#next char will be escapped
		else if (c=="\\" && quoted!=0) {
			escape <- 1
			wrapelineBackslash <- 0
		}
		
		# doublequote (") found
		else if (c=="\"") {
			if (quoted==0) {
				quoted<-1
			}
			else if (quoted==1) {
				quoted<-0
			}
			else if (quoted==2) {
				#continue
			}
			quotefound <- 1
			wrapelineBackslash <- 0
		}
		
		# singlequote (') found
		else if (c=="'") {
			if (quoted==0) {
				quoted<-2
			}
			else if (quoted==1) {
				
			}
			else if (quoted==2) {
				quoted<-0
			}
			quotefound <- 1
			wrapelineBackslash <- 0
		}
		
		# '#' found - ignore everything else (unless we are in a quote)
		else if (c=="#") {
			if (quoted==0 || wrapelineBackslash==1) {
				#game over, comment starting char '#' found
				#(outside of a quote, or after \ (indicating wrapline))
				#return everything up to this character
				return(substr(x, 1, i-1))
			}
			else {
				#continue
			}
		}
			
		#regular character found
		else {
			if (c!=" " && c!="\t")
				wrapelineBackslash <- 0
				
			#continue
		}
	}
	if (quoted > 0) {
		#this will be caught later
		#.catError("unterminated quote found", fname, line)
		#return()
	}
	return(x)
}


# ***********************************************************
# .inCollection:
#   returns true if needle occurs in haystack
# Input: 
#   haystack - a vector to search
#   needle   - a single element to search for
# -----------------------------------------------------------
.inCollection <- function(haystack, needle)
{
	if (is.null(haystack)) {
		return(FALSE)
	}
	if (is.vector(haystack)) {
		return(any(haystack==needle))
	}
	stop("only vectors are supported")
	return(FALSE)
}


# ***********************************************************
# .isReallyNull:
#   returns true if key is not a real key of the list
#   false if key is found in names(list)
# Arguments:
#   list - a named list
#   key  - named element of list to look for
# -----------------------------------------------------------
.isReallyNull <- function(list, key)
{
	return (!any(names(list)==key))
}


# ***********************************************************
# .searchCollection:
#   searches a haystack for a needle, or a similar longer needle.
# Arguments:
#   haystack - list to search
#   needle = scaler to search for
# Output: 
#   position of needle in list (non-negative)
#   -1 if none are found
#   -2 if two similar needles are found.
#      ex) -2 for "nee" is similar to "need", and "needle"
# -----------------------------------------------------------
.searchCollection <- function(haystack, needle)
{
	similar <- -1
	for(i in 1:length(haystack)) {
		if (haystack[[i]]$param == needle) {
			return(i)
		}
		else if (any(grep(paste("^", needle, sep=""), haystack[[i]]$param))) {
			#this is used to find any similar matches
			#if more than two are similar, then it is impossible
			#to determine which needle we are after
			if (similar == -1)
				similar <- i #this is the first similar needle
			else
				similar <- -2 #two similar needles were found
		}
	}
	return(similar)
}


# ***********************************************************
# .hash:
#   creates a hash table and stores the value of each hash when first set
#   setting a hash that has been defined, will just return the old value.
#   keys starting with a . will be ignored by getAll
# -----------------------------------------------------------
.hash <- function() {
	varStore<-list()
	list(
		add = function(key, ...)
		{
			if (!is.character(key)) {
				stop("hash error - key must be a string")
				return()
			}
			if (key=="") {
				stop("hash error - key must be atleast 1character long")
				return()
			}
			
			if (!.isReallyNull(varStore, key))
				return(varStore[[key]])
			
			#add new entry
			varStore[[key]] <<- list(...)
			
			return(varStore[[key]])
		},
		#WARNING: due to the fact R doesnt force unique keys, if one is already set
		#a new one will be added, rather than overwritten
		set = function(key, ...)
		{
			if (!is.character(key)) {
				stop("hash error - key must be a string")
				return()
			}
			if (key=="") {
				stop("hash error - key must be atleast 1character long")
				return()
			}
			
			if (!is.list(varStore[[key]]))
				varStore[[key]] <<- list()
			
			#set additional keys
			tmp <- list(...)
			tmpNames <- names(tmp)
			if (length(tmp)>0) {
				for (i in 1:length(tmp)) {
					if (is.null(tmpNames[i]))
						varStore[[key]][[i]] <<- tmp[[i]]
					else if (tmpNames[i]=="")
						varStore[[key]][[i]] <<- tmp[[i]]
					else
						varStore[[key]][[tmpNames[i]]] <<- tmp[[i]]
				}
			}
			
			return(varStore[[key]])
		},
		get = function(key)
		{
			return(varStore[[key]])
		},
		getAll = function()
		{
			retData <- list()
			len<-length(varStore)
			x<-names(varStore)
			if (len > 0) {
				for(i in 1:len) {
					if (!any(grep("^\\.", x[i]))) {
						#dont return any vars with a key that starts with .
						retData[[x[i]]] <- varStore[[i]]
					}
				}
			}
			return(retData)
		},
		getNames = function()
		{
			retData <- c()
			len<-length(varStore)
			x<-names(varStore)
			if (len > 0) {
				for(i in 1:len) {
					if (!any(grep("^\\.", x[i]))) {
						#dont return any vars with a key that starts with .
						retData <- c(retData,x[i]) #add x[i] to vector to return
					}
				}
			}
			return(retData)
		}
	)
}


# ***********************************************************
# .extractVar:
#   extracts values from the tclvar ptrs
# Arguments:
#   data - named list containing list(type="tcl", tclvar="tcl_var_ptr", mode="numeric")
# -----------------------------------------------------------
.extractVar <- function(tclHash)
{
	data <- tclHash$getAll()
	
	
	values <- list()
	keys <- names(data)
	if (length(data)<1)
		return(NULL)

	#extract values from tcl into an R list whose index corresponds to the data list
	for(i in 1:length(data)) {
		if (!is.null(data[[i]]$tclvar))
			values[[i]] <- tclvalue(data[[i]]$tclvar)
		else if (!is.null(data[[i]]$tclwidget)) {
			#special case for text widgets
			values[[i]] <- tclvalue(tkget(data[[i]]$tclwidget,"0.0","end"))
			data[[i]]$widget$mode <- "character"
		}
		else {
			stop(paste("unknown type:", data[[i]]))
		}


		#convert data to propper type
		if (is.null(data[[i]]$widget$mode))
			mode <- "numeric"
		else
			mode <- data[[i]]$widget$mode
		
		#special case for checkboxes which give 0 or 1
		if (mode=="logical") {
			if (values[[i]]=="1" || values[[i]]=="TRUE" || values[[i]]=="T")
				values[[i]] <- TRUE
			else
				values[[i]] <- FALSE
		}
		else if (mode=="numeric" || mode=="interget") {
			#does the string look like numeric or integer?
			if (any(grep("^(\\-|\\+)?[0-9]*(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?$", values[[i]])) && values[[i]]!="-")
				values[[i]] <- as(values[[i]], mode)
			else
				values[[i]] <- NA
		}
		else if (mode=="complex") {
			#does the string look like a complex number
			if (any(grep("^(\\-|\\+)?[0-9]*(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?(((\\-|\\+)?[0-9]+|[0-9]*)(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?i)?$", values[[i]])) && values[[i]]!="-")
				values[[i]] <- as(values[[i]], mode)
			else
				values[[i]] <- NA		
		}
		#otherwise it is a character, and doesnt need converting
		
	}

	retData <- list()
	for(i in 1:length(values)) {
		#look for any vectors (arrays, matrices)
		if (any(grep("^[^\\[]+\\[([0-9,]+)\\]$", keys[i]))) {
			ind<-gsub("^[^\\[]+\\[([0-9,]+)\\]$", "\\1", keys[i])
			ind<-as.numeric(unlist(strsplit(ind, ",")))
			name <- gsub("\\[[0-9,]+\\]", "", keys[i])
			if (length(ind)>1) {
				#we collect information about matricies in a bunch of lists first
				if (!exists("matrixTmp"))
					matrixTmp <- list()
				
				#create a list for the new matrix
				
				if (is.null(matrixTmp[[name]])) {
					matrixTmp[[name]] <- list()
				}
				
				#call matrixhelper to build a list, and then save the new changes
				matrixTmp[[name]] <- .matrixHelp(matrixTmp[[name]], ind, values[[i]])
			}
			else {
				#single index
				retData[[name]][ind] <- values[[i]]
			}
		}
		#any var ending with indicies and a d EX: var[3,5]d is an element of a data.frame
		else if (any(grep("^[^\\[]+\\[([0-9,]+)\\]d$", keys[i]))) {
			ind<-gsub("^[^\\[]+\\[([0-9,]+)\\]d$", "\\1", keys[i])
			ind<-as.numeric(unlist(strsplit(ind, ",")))
			name <- gsub("\\[[0-9,]+\\]d", "", keys[i])
			if (length(ind)>1) {
				#we collect information about matricies in a bunch of lists first
				if (!exists("dataframeTmp"))
					dataframeTmp <- list()
				
				#create a list for the new matrix
				
				if (is.null(dataframeTmp[[name]])) {
					dataframeTmp[[name]] <- list()
				}
				
				#call matrixhelper to build a list, and then save the new changes
				dataframeTmp[[name]] <- .matrixHelp(dataframeTmp[[name]], ind, values[[i]])
			}
			else {
				#single index
				retData[[name]][ind] <- values[[i]]
			}		
		}
		else {
			#no index
			retData[[keys[i]]] <- values[[i]]
		}
	}
	
	#convert all collected matrix lists, and convert them into real n-dim arrays.
	if (exists("matrixTmp")) {
		keys <- names(matrixTmp)
		for(i in 1:length(matrixTmp)) {
			colnames <- tclHash$get('.PBS.names')[[1]]$get(keys[i])$widget$colnames
			colnames <- unlist(strsplit(colnames, "( |\t)+"))
			
			rownames <- tclHash$get('.PBS.names')[[1]]$get(keys[i])$widget$rownames
			rownames <- unlist(strsplit(rownames, "( |\t)+"))
			
			retData[[keys[i]]] <- .convertMatrixListToMatrix(matrixTmp[[i]])
			
			dimnames(retData[[keys[i]]])<- .PBSdimnameHelper(rownames, colnames, dim(retData[[keys[i]]]))
		}
	}
	
	#convert dataframe lists into dataframes
	if (exists("dataframeTmp")) {
		keys <- names(dataframeTmp)
		for(i in 1:length(dataframeTmp)) {
			colnames <- tclHash$get('.PBS.names')[[1]]$get(keys[i])$widget$colnames
			rownames <- tclHash$get('.PBS.names')[[1]]$get(keys[i])$widget$rownames

			retData[[keys[i]]] <- .convertMatrixListToDataFrame(dataframeTmp[[i]], colnames, rownames)
			dimnames(retData[[keys[i]]]) <- .PBSdimnameHelper(rownames, colnames, dim(retData[[keys[i]]]))
		}
	}
	####
	#
	#  data.frame(unlist(x[[1]]), unlist(x[[2]]), unlist(x[[3]]))
	#  problem is this givves us a transpossed data.frame
	#  x[[1]] is really a row and not column.
	#
	###
	
	
	
	return(retData)
}


# ***********************************************************
# .PBSdimnameHelper:
#   adds dimnames to stuff (matrix, data.frame)
# Arguments:
#   rownames - vector of size 1, or dim[1] nameing the rows.
#              if only one name is given, a number (1..dim[1]) will be appended to the name
#   colnames - vector of size 1 or dim[2] naming columns
#              if only one name is given, then (1..dim[2]) is appended
#   dim      - vector of size 2, dim[1] is nRows, dim[2] is nCols

# -----------------------------------------------------------
.PBSdimnameHelper <- function(rownames, colnames, dim)
{
	if (length(rownames)>1)
		rName <- rownames
	else if (length(rownames)==0)
		rName <- NULL
	else if (rownames=="")
		rName <- NULL
	else {
		nRows <- dim[1]
		rName <- paste(rownames, 1:nRows, sep="")
	}
			
	if (length(colnames)>1)
		cName <- colnames
	else if (length(colnames)==0)
		cName <- NULL
	else if (colnames=="")
		cName <- NULL
	else {
		nCols <- dim[2]
		cName <- paste(colnames, 1:nCols, sep="")
	}
	
	return(list(rName, cName))
}


# ***********************************************************
# .convertMatrixListToMatrix:
#   converts a list into an N-dim array
# Arguments:
#   mList = z[[1]][[1]]...[[1]]=x
#           z[[1]][[1]]...[[2]]=x
#           ...
#           z[[1]][[1]]...[[1]]=x
#           ...
#           z[[i]][[j]]...[[k]]=x
#
# output an N-dim array
# -----------------------------------------------------------
.convertMatrixListToMatrix <- function(mList)
{
	size <- .getMatrixListSize(mList)
	arr <- array(dim=size)
	arr <- .setMatrixElement(mList, arr)
	return(arr)
}


# ***********************************************************
# .convertMatrixListToDataFrame:
#   similar to toArray but to data.frame
# Arguments:
#   mList - see .convertMatrixListToMatrix:
# -----------------------------------------------------------
.convertMatrixListToDataFrame <- function(mList, colName="Y", rowNames="X")
{
	size <- .getMatrixListSize(mList)
	arr <- array(dim=size)
	arr <- .setMatrixElement(mList, arr)
	
	x<-list()
	for(i in 1:size[2]) {
		x[[i]]<-list()
	}

	for(i in 1:length(mList)) {
		for(j in 1:length(mList[[i]])) {
			x[[j]][[i]] <- mList[[i]][[j]]
		}
	}
	
	if (length(rowNames)==0) {
		rowNames=NULL
	}
	else {
		if (length(rowNames)==1) {
			if (rowNames=="") {
				rowNames=NULL
			}
			rowNames <- paste(rowNames, 1:size[1], sep="")
		}
		else if (length(rowNames)!=size[1])
			stop(paste("rowNames should be NULL, or a vector of size 1 or", size[1], ".\nGot rowNames=", rowNames, sep=""))
	}
	
	#data.frame(unlist(x[[1]]), unlist(x[[2]]), unlist(x[[3]]))
	txt <- "ret <- data.frame(row.names=rowNames, "
	for(i in 1:size[2]) { #foreach column
		if (i>1)
			txt <- paste(txt, ",")
		name <- paste("X", i, sep="")
		txt <- paste(txt, name, "=unlist(x[[", i, "]])", sep="")
	}
	txt <- paste(txt, ")")
	
	eval(parse(text=txt))
	
	return(ret)
}


# ***********************************************************
# .setMatrixElement:
#   helper function used by .convertMatrixListToMatrix
#   to assign values from the matrix list into the array
# -----------------------------------------------------------
.setMatrixElement <- function(m, a, ind=NULL)
{
	if (is.null(m))
		return(a)
	if (!is.list(m)) {
		eval(parse(text=paste("a[", paste(ind, collapse=','), "] <- m", sep="")))
		return(a)
	}

	for(i in 1:length(m)) {
		a<-.setMatrixElement(m[[i]], a, c(ind,i))
	}
	return(a)
}


# ***********************************************************
# .getMatrixListSize:
#   helper function used by .convertMatrixListToMatrix
#   to determine the minumum required size of the array
#   needed to create to convert the list into array
# -----------------------------------------------------------
.getMatrixListSize <- function(m, d=NULL, big=0)
{
	if (!is.list(m)) {
		return(pmax(d, big))
	}

	for(i in 1:length(m)) {
		big <- .getMatrixListSize(m[[i]], c(d,i), big)
	}
	return(big)
}


# ***********************************************************
# func:
#   used to help .extractVar deal with N-dim maticies
#   firstly it is converted into a "matrix list"
#   once the matrix list is completed (and size known)
#   it should be converted into a true array
# -----------------------------------------------------------
.matrixHelp <- function(matrixList, ind, value)
{

	if (length(ind)>1) {
		if (length(matrixList)<ind[1])
			matrixList[[ind[1]]]<-list()
		else if(!is.list(matrixList[[ind[1]]]))
			matrixList[[ind[1]]]<-list()

		matrixList[[ind[1]]] <- .matrixHelp(matrixList[[ind[1]]], ind[-1], value)
		return(matrixList)
	}
	else if(length(ind)==1) {
		matrixList[[ind[1]]]<-value
		return(matrixList)
	}
	else {
		stop(".matrixHelp() was called with no indices.")
	}
}


# ***********************************************************
# createWin:
#   creates a GUI window from a given file, or GUI description list
# -----------------------------------------------------------
createWin <- function(fname, astext=FALSE)
{

	if (astext) {
		guiDesc <- parseWinFile(fname, astext=TRUE)
	}
	else if (is.character(fname)) {
		guiDesc <- parseWinFile(fname)
	}
	else if (is.list(fname)) {
		guiDesc <- .validateWindowDescList(fname)
	}
	else {
		cat("ERROR, supplied argument is wrong type\n")
		return()
	}
	
	if (is.null(guiDesc)) {
		return()
	}
	
	#create global hash for
	if (!exists('.PBS.tclHash', envir= .GlobalEnv)) {
		PBSmodelling.tclHash <- list()
	}
	else {
		PBSmodelling.tclHash <- get('.PBS.tclHash', envir= .GlobalEnv)
	}



	#iterate over all possible windows
	for(i in 1:length(guiDesc)) {

		if (is.null(guiDesc[[i]]$windowname))
			stop("No window name given.")
	
	
	
		#create a hash for the window if it doesnt exist
		if(.isReallyNull(PBSmodelling.tclHash, guiDesc[[i]]$windowname)) {
			PBSmodelling.tclHash[[guiDesc[[i]]$windowname]] <- .hash()

		}
		else {
			#destroy old window (if active)
			tt <- PBSmodelling.tclHash[[guiDesc[[i]]$windowname]]$get(".PBS.tkwindow")[[1]]
			#if (!is.null(tt)) #TODO WHY IS THIS NULL? it shouldnt be - related to changes made to $set func
			tkdestroy(tt)
			tt <- PBSmodelling.tclHash[[guiDesc[[i]]$windowname]] <- .hash()
		}
		
		

		#setup some default intial values in the hash list
		PBSmodelling.tclHash[[guiDesc[[i]]$windowname]]$add(".PBS.defaults", list())
		PBSmodelling.tclHash[[guiDesc[[i]]$windowname]]$add(".PBS.maxaction", 100)
		PBSmodelling.tclHash[[guiDesc[[i]]$windowname]]$add(".PBS.windowname", guiDesc[[i]]$windowname)
		
		#store hash within hash for any functions called by widgets
		#to be returned in PBS.win$funs
		PBSmodelling.tclHash[[guiDesc[[i]]$windowname]]$add(".PBS.functions", .hash())
		
		#used to store original names
		#which is used to look up details on matrix and vector widgets
		PBSmodelling.tclHash[[guiDesc[[i]]$windowname]]$add(".PBS.names", .hash())
		
		#create TK window
		tt <- tktoplevel() #make window
		
		#store the TK handle (so we can destroy it)
		PBSmodelling.tclHash[[guiDesc[[i]]$windowname]]$set(".PBS.tkwindow", tt)
		
		
		#set window title
		tkwm.title(tt,guiDesc[[i]]$title)
		
		
		
		#setup for menus
		if (length(guiDesc[[i]]$menus) > 0) {
			topMenu <- tkmenu(tt)
			tkconfigure(tt,menu=topMenu)
			
			.menuFunctionCall <- function(command)
			{

				#call the real function
				if (is.null(command))
					cat(paste("Warning: no function given.\n", sep=""))
					
				if (exists(command,mode="function")) {
					do.call(command, list())
				}
				else {
					cat(paste("Warning: cannot find function '", command, "'.\n", sep=""))
				}
			}
			
			.createMetaWidget.menu <- function(topMenu, widget)
			{
				label <- widget$label
				if (widget$nitems < 1)
					stop("menu nitems must have atleast one menuitem.")
					
				subMenu <- tkmenu(topMenu, tearoff=FALSE)
				
				for(i in 1:widget$nitems) {
					if (widget$data[[i]]$type=="menu") {
						.createMetaWidget.menu(subMenu, widget$data[[i]])
						#stop("submenus need work.")
					}
					else if (widget$data[[i]]$type=="menuitem")
					{
						#something weird happened that required me to use eval
						#otherwise all menus only got the settings of the last created menu
						if (widget$data[[i]]$font == "")
							eval(parse(text=paste('
							tkadd(subMenu,"command",
						      label=widget$data[[i]]$label,
						      command=function(...) .menuFunctionCall("',widget$data[[i]][["function"]],'"))
						    ', sep="")))
						else
							eval(parse(text=paste('
							tkadd(subMenu,"command",
						      label=widget$data[[i]]$label,
						      font=.createTkFont("', widget$data[[i]]$font, '"),
						      command=function(...) .menuFunctionCall("',widget$data[[i]][["function"]],'"))
						    ', sep="")))
					}
					else {
						stop(paste("widget type", widget$data[[i]]$type, "found when expecting a menu or menuitem widget"))
					}
						
				}
				
				if (widget$font == "")
					tkadd(topMenu,"cascade",label=label,menu=subMenu)
				else
					eval(parse(text=paste('
					tkadd(topMenu,"cascade",label=label,menu=subMenu, font=.createTkFont("', widget$font, '"))
					')))
				
			}
			
			#create menus
			for(menu_i in 1:length(guiDesc[[i]]$menus)) {
				.createMetaWidget.menu(topMenu, guiDesc[[i]]$menus[[menu_i]])
			}
		}


		if (length(guiDesc[[i]]$widgets)>0) {
			#pack all widgets into a grid with ncol=1 nrow=<number of widgets>
			gridWidget = list(
		               type="grid", 
		               font="",
		               borderwidth=0,
		               relief="flat",
		               padx=0,
		               pady=0,
		               data=list(),
			           nrow <- length(guiDesc[[i]]$widgets),
			           ncol <- 1,
			           byrow=guiDesc[[i]]$gridVertical
		             )
    
    		for(j in 1:length(guiDesc[[i]]$widgets)) {
				gridWidget$data[[j]] <- list()
				gridWidget$data[[j]][[1]] <- guiDesc[[i]]$widgets[[j]]
			}

			wid <- .createWidget(tt, gridWidget, PBSmodelling.tclHash[[guiDesc[[i]]$windowname]])
			tkgrid(wid)
		}
	}
	
	PBS.win <- .getPBS.win(PBSmodelling.tclHash[[guiDesc[[i]]$windowname]])
	assign("PBS.win", PBS.win, env = .GlobalEnv)
	assign(".PBS.tclHash", PBSmodelling.tclHash, env = .GlobalEnv)
	return(invisible(NULL))
}


# ***********************************************************
# closeWin:
#   closes a window
# Arguments:
#   name - window name to close
# -----------------------------------------------------------
closeWin <- function(name=names(.PBS.tclHash))
{
	for(i in 1:length(name)) {
		if(!.isReallyNull(.PBS.tclHash, name[i])) {
			tt <- .PBS.tclHash[[name[i]]]$get(".PBS.tkwindow")[[1]]
			tkdestroy(tt)
		}
	}
}


# ***********************************************************
# .validateWindowDescList:
#   determines if the list represents a valid PBS Modelling description List
#   if any required fields are missing, it will halt via stop()
#   if any fields are ommitied which have default values defined in .getParamOrder()
#   then those fields and values will be set
# Arguments:
#   x - list to validate
# -----------------------------------------------------------
.validateWindowDescList <- function(x)
{
	if (!is.list(x))
		stop("no list was given")
	if (length(x)==0)
		stop("No windows were given")
	
	paramOrder <- .getParamOrder()
	
	for (i in 1:length(x)) {
		#validate each window
		
		#check for a window title
		if (is.null(x[[i]]$title))
			x[[i]]$title <- paramOrder$window$title
		
		#check for widgets
		if (!is.list(x[[i]]$widgets))
			stop("The widget list is missing")
		
		x[[i]]$widgets <- .validateWindowDescWidgets(x[[i]]$widgets)
	}
	
	return(x)
}


# ***********************************************************
# .validateWindowDescWidgets:
#   used by .validateWindowDescList to validate each widget
# Arguments:
#   x - widget list to validate
# -----------------------------------------------------------
.validateWindowDescWidgets <- function(x)
{
	paramOrder <- .getParamOrder()
	for(i in 1:length(x)) {
		type <- x[[i]]$type
		if (is.null(paramOrder[[type]]))
			stop(paste("unknown widget type found:", type))
		
		#look for all options, if any are missing assign the default value
		#unless they are absolutely required
		args <- paramOrder[[type]]
		for(j in 1:length(args)) {
			if (is.null(x[[i]][[args[[j]]$param]])) {
				#a paramater is missing from the list.
				
				#is the paramater required?
				if (args[[j]]$required)
					stop(paste("missing argument", args[[j]]$param, "from widget", type))
				
				#is there a default value?
				if (!is.null(args[[j]]$default))
					x[[i]][[args[[j]]$param]] <- args[[j]]$default
			}
			else {
				#the argument was found. let's check that its the right type
				#and matches the grep
				
				#check grep if applicable
				if (!is.null(args[[j]]$grep)) {
					#check grep from .getParamOrder() with supplied value from list
					if (!any(grep(args[[j]]$grep, x[[i]][[args[[j]]$param]])))
						stop(paste("given value \"", x[[i]][[args[[j]]$param]], 
						"\" does not match grep:", args[[j]]$grep, sep=""))
				}
			
			}
				
		}
	}
	return(x)
}


# ***********************************************************
# parseWinFile:
#   parse window description file into a list
# Arguments:
#   fname - filename or vector of strings
#   astext - if F, treat fname as a filename
#            if T, treat it as the contents of a file
# -----------------------------------------------------------
parseWinFile <- function(fname, astext=FALSE)
{
	if (astext) {
		srcfile <- orgfile <- fname
		fname <- "read as text"
	}
	else {
		#read in file, and strip out any comments. (comments start with #)
		if (fname=="")
			stop("No filename given")
		srcfile <- orgfile <- scan(fname, what=character(), sep="\n", quiet=TRUE, blank.lines.skip=FALSE)
	}
	data <- list()
	j <- 0
	halt <- FALSE
	extendLine <- FALSE #used for extending a single line into lines with \
	extendLineNumber <- 0 #where a new widget starts - used for error messages
	str <- ""

	if (!length(srcfile)) {
		stop("Input file is empty\n")
	}

	
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
			str <- .trimWhiteSpace(str)
			tmp <- sub('\\\\$', '', str)
			if (tmp==str) #no sub took place
				extendLine = FALSE
			else
				extendLine = TRUE
			str <- tmp

			#parse the line once it is complete (no \)
			if (extendLine == FALSE) {
				tmp <- .getParamFromStr(str, fname, extendLineNumber, i, orgfile)
				if (is.null(tmp)) {
					halt <- TRUE
				}
				else if(halt==FALSE) {
					j <- j + 1
					data[[j]]<-tmp
				}
			}
		}
	}
	if (halt==TRUE) {
		stop("Errors were found in the GUI description file. Halting\n")
	}

	#by this point all widgets from the text file have been converted into
	#an appropriate list of widgets, we will need to setup the nested grids
	
	#we must make sure the first element is a window, if not we will insert one
	#as the head of the list
	if (data[[1]]$type != "window") {
		data <- c(1, data)
		data[[1]] <- list(type="window", title="", vertical=TRUE, name="window")
	}
		
	#data[[1]] is now guarenteed to be a window type

	#start parsing the read data - this mostly setups grid data
	parsedData<-list()
	j <- 0; #widget index
	i <- 0; #window index
	k <- 0; #menu index
	while(length(data)) {
		#pull out any options
		if (data[[1]]$type=="window") {
			i <- i + 1 #increment new window index
			j <- 0 #reset widget index
			k <- 0 #reset menu index
			parsedData[[i]] <- list()
			parsedData[[i]]$title <- data[[1]]$title
			parsedData[[i]]$windowname <- data[[1]]$name
			parsedData[[i]]$gridVertical <- data[[1]]$vertical
			parsedData[[i]]$widgets <- list() #holds all widgets
			parsedData[[i]]$menus <- list() #holds all menu widgets

			data <- data[-1]
		}
		else {
			#look for menu widgets
			if (data[[1]]$type=="menu") {
				k <- k + 1 #increment menu index
				
				#save menu widget
				parsedData[[i]]$menus[[k]] <- data[[1]]
				
				#pull out n menuitem
				tmp <- .parsemenu(data[-1], data[[1]]$nitems)
				parsedData[[i]]$menus[[k]]$data <- tmp$menuData
				
				#parse remaining widgets
				data <- tmp$unparsedData
			}
			
			#look for regular widgets
			else {
				j <- j + 1 #incrememnt widget index
				
				#save widget
				parsedData[[i]]$widgets[[j]] <- data[[1]]
				
				#associate child widgets if grid
				if (data[[1]]$type=="grid") {
					
					tmp <- .parsegrid(data[-1], data[[1]]$nrow, data[[1]]$ncol)
					parsedData[[i]]$widgets[[j]]$data <- tmp$gridData
		
					#parsedData returns all left over widgets
					data <- tmp$unparsedData
				}
				else {
					data <- data[-1] #remove widget from to parse list
				}
			}
		}
			
	}

	return(parsedData)
}


# ***********************************************************
# func:
#   TODO
# Arguments:
#  
# -----------------------------------------------------------
.parsemenu <- function(data, nItems)
{
	menuitems <- list()
	itemCount <- 0
	while(length(data)) {
		#increment count
		itemCount <- itemCount + 1

		
		if (data[[1]]$type!="menuitem" && data[[1]]$type!="menu")
			stop("non menu, or menuitem widget found, when expecting one. Check your menu nitems count.")
		

		#add/associate widget with menu
		menuitems[[itemCount]] <- data[[1]]
				
		#add a nested grid type
		if (data[[1]]$type=="menu") {
			tmp <- .parsemenu(data[-1], data[[1]]$nitems)
			menuitems[[itemCount]]$data <- tmp$menuData
			data <- tmp$unparsedData
		}
		else {
			data <- data[-1] #remove widget
		}

		#return menu sub items
		if (itemCount == nItems) {
			tmp <- list()
			tmp$menuData <- menuitems
			tmp$unparsedData <- data #left over data
			return(tmp)
		}
	}
	
	stop("menu did not have enough child menuitems. Check your menu nitems count.")
}


# ***********************************************************
# .parsegrid:
#   returns two items in a list:
#   return$gridData which is a list of lists representing columns
#   and return$unparsedData - which is left over from the grid and 
#   still needs parsing
# Arguments:
#   data - list of widget lists
#   nRow - num of grid rows
#   nCol - num of grid columns
# -----------------------------------------------------------
.parsegrid <- function(data, nRow, nCol)
{
	parsedData=list()
	rows=list()
	cols=list()
	row <- 0;
	col <- 0;
	while(length(data)) {


		#add item into column
		col <- col + 1
		cols[[col]] <- data[[1]]
		

				
		#add a nested grid type
		if (data[[1]]$type=="grid") {
			tmp <- .parsegrid(data[-1], data[[1]]$nrow, data[[1]]$ncol)
			cols[[col]]$data <- tmp$gridData
			data <- tmp$unparsedData
		}
		else {
			data <- data[-1]
		}
		
		#check for a filled row
		if (col == nCol) {
			
			row <- row + 1
			col <- 0
			rows[[row]] <- cols
			
			#any more rows left?
			if (row == nRow) {
				
				#return two parts of the data
				tmp <- list()
				tmp$gridData <- rows
				tmp$unparsedData <- data #left over data
				return(tmp)
				
			}
		}
		
	
		
	}
	
	stop("Grid did not have enough child objects.")

}


# ***********************************************************
# .stripSlashes:
#   removes slashes from a string
# Arguments:
#   TODO
# -----------------------------------------------------------
.stripSlashes <- function(x, fname="", line.start=0, line.end=0, sourcefile=list())
{
	word<-""
	escape<-0
	for(i in 1:nchar(x)) {
		ch<-substr(x,i,i)
		
		#escaped char is expected
		if (escape!=0) {
			if (ch=="n")
				ch <- "\n"
			else if (ch=="t")
				ch <- "\t"
			else if (ch=="r")
				ch <- "\r"
			
			word <- paste(word, ch, sep="")
			escape <- 0
		}
		
		#next char will be escapped
		else if (ch=="\\") {
			escape <- 1
		}
		#shouldnt find any singlequotes - if we did it should be a vector of strings
		else if (ch=="'" || ch=="\"") {
			.catError("unexpected singlequote found.", fname, line.start, line.end, sourcefile)
			return(NULL)
		}
		#any other character
		else {
			word <- paste(word, ch, sep="")
		}
	}
	return(word)
}


# ***********************************************************
# func:
#   given a string x, x is split into a vector of words, which were seperated by spaces
#   however, if single quotes are used, space is perserved
#   x="a b 'c d'" converts into "a" "b" "c d"
# Arguments:
#  
# -----------------------------------------------------------
.stripSlashesVec <- function(x, fname="", line.start=0, line.end=0, sourcefile=list())
{
	word<-""
	words=c()
	escape<-0
	quoted<-0
	quoteFound<-0
	j <- 0
	for(i in 1:nchar(x)) {
		ch<-substr(x,i,i)
		
		#escaped char is expected
		if (escape!=0) {
			if (ch=="n")
				ch <- "\n"
			else if (ch=="t")
				ch <- "\t"
			else if (ch=="r")
				ch <- "\r"
			
			word <- paste(word, ch, sep="")
			escape <- 0
		}
		
		#next char will be escapped
		else if (ch=="\\") {
			escape <- 1
		}
		#shouldnt find any doublequotes anywhere
		else if (ch=="\"") {
			.catError("unexpected doublequote found.", fname, line.start, line.end, sourcefile)
			return(NULL)
		}
		else if (ch=="'") {
			if (quoted==0) {
				quoted<-1
				quoteFound<-1
			}
			else {
				quoted <- 0
			}
		}
		#space found
		else if (ch==" " || ch=="\t") {
			if (quoted==0) {
				if (word!="" || quoteFound==1) {
					##save key and value
					j <- j + 1
					words[j] <- word
					
					#reset variables for next loop
					word <- ""
					quoteFound <- 0
				}
			}
			else {
				word <- paste(word, ch, sep="")
			}
		}
		#any other character
		else {
			word <- paste(word, ch, sep="")
		}
	}
	#look for last word
	if (quoted==0) {
		if (word!="" || quoteFound==1) {
			##save key and value
			j <- j + 1
			words[j] <- word
			
			#reset variables for next loop
			word <- ""
			quoteFound <- 0
		}
	}
	else {
		.catError("unterminated quote found.", fname, line.start, line.end, sourcefile)
		return(NULL)
	}
	if (is.null(words))
		words[1]<-""
	return(words)
}


# ***********************************************************
# .convertPararmStrToVector:
#   function to convert a string, x, into a vector of elements seperated by
#   whitespace.
#   whitespace can be interupted as values if it is enclosed by quotes.
#   special characters (newline, tab, \, ', ") must be escaped with \
#
# Arguments:
#   x     - string
#   fname - filename string for warning messages
#   line  - line number for warning messages
#
# Output:  
#   vector of values
# -----------------------------------------------------------
.convertPararmStrToVector <- function(x, fname="", line=0)
{
	escape<-0
	quoted<-0	#0=none, 1=", 2='
	word<-""
	j <- 0 #counter for words array
	equal <- 0 #counter for equal char
	quotefound <- 0 #used to capture ""
	words <- NULL
	x<-.trimWhiteSpace(x)
	for(i in 1:nchar(x)) {
		c<-substr(x,i,i)
		
		#escaped char is expected
		if (escape!=0) {
			if (c=="n")
				c <- "\n"
			else if (c=="t")
				c <- "\t"
			else if (c=="r")
				c <- "\r"
			
			word <- paste(word, c, sep="")
			escape <- 0
		}
		
		#next char will be escapped
		else if (c=="\\" && quoted!=0) {
			escape <- 1
		}
		
		# doublequote (") found
		else if (c=="\"") {
			if (quoted==0) {
				quoted<-1
			}
			else if (quoted==1) {
				quoted<-0
			}
			quotefound <- 1
		}
		
		# singlequote (') found
		#else if (c=="'") {
		#	if (quoted==0) {
		#		quoted<-2
		#	}
		#	else if (quoted==1) {
		#		word <- paste(word, c, sep="")
		#	}
	#		else if (quoted==2) {
	#			quoted<-0
	#		}
	#		quotefound <- 1
	#	}
		
		# '#' found - ignore everything else (unless we are in a quote)
		else if (c=="#") {
			if (quoted==0) {
				if (word!="" || quotefound==1) {
					##save key and value
					
					words <- c(words, word)
					
				}
				
				#ignore everything else on this line now
				return(words)
			}
			else {
				word <- paste(word, c, sep="")
			}
		}
		
		#space found
		else if (c==" " || c=="\t") {
			if (quoted==0) {
				if (word!="" || quotefound==1) {
					##save key and value
					j <- j + 1
					words[[j]] <- word
					
					#reset variables for next loop
					word <- ""
					key <- ""
					equal <- 0
					quotefound <- 0
				}
			}
			else {
				word <- paste(word, c, sep="")
			}
		}
		
		#regular character found
		else {
			word <- paste(word, c, sep="")
		}
	}
	if (quoted > 0) {
		.catError("unterminated quote found", fname, line)
		return()
	}

	#save last value
	words <- c(words, word)

	return(words)
}


# ***********************************************************
# func:
#   converts a given string of values seperated by spaces into a list
#   while preserving space and escaped quotes within quotes 
#   (kindof - the value must still be stripped depending if its a single string, or vector of strings)
# Arguments:
#  
# -----------------------------------------------------------
.convertPararmStrToList <- function(x, fname="", line.start=0, line.end=0, sourcefile=list())
{
	escape<-0
	quoted<-0	#0=none, 1=", 2='
	word<-""
	j <- 0 #counter for words array
	equal <- 0 #counter for equal char
	quotefound <- 0 #used to capture ""
	words <- list()
	x<-.trimWhiteSpace(x)
	for(i in 1:nchar(x)) {
		c<-substr(x,i,i)
		
		#escaped char is expected
		if (escape!=0) {
			word <- paste(word, c, sep="")
			escape <- 0
		}
		
		#next char will be escapped
		else if (c=="\\" && quoted!=0) {
			word <- paste(word, c, sep="")
			escape <- 1
		}
		
		# doublequote (") found
		else if (c=="\"") {
			if (quoted==0) {
				quoted<-1
			}
			else if (quoted==1) {
				quoted<-0
			}
			quotefound <- 1
		}

		# singlequote (') found
		else if (c=="'") {
			if (quoted==0) {
				.catError("unexpected singlequote found outside of doublequoted string", fname, line.start, line.end, sourcefile)
				return()
			}
			else if (quoted==1) {
				word <- paste(word, c, sep="")
			}
		}
		
		# '#' found - ignore everything else (unless we are in a quote)
		else if (c=="#") {
			if (quoted==0) {
				if (word!="" || quotefound==1) {
					##save key and value
					j <- j + 1
					words[[j]] <- list()
					if (equal==1) {
						words[[j]]$key=key
						key<-""
					}
					words[[j]]$value=word
					
				}
				
				#ignore everything else on this line now
				return(words)
			}
			else {
				word <- paste(word, c, sep="")
			}
		}
		
		#space found
		else if (c==" " || c=="\t") {
			if (quoted==0) {
				if (word!="" || quotefound==1) {
					##save key and value
					j <- j + 1
					words[[j]] <- list()
					if (equal==1) {
						words[[j]]$key=key
						key<-""
					}
					words[[j]]$value=word
					
					#reset variables for next loop
					word <- ""
					key <- ""
					equal <- 0
					quotefound <- 0
				}
			}
			else {
				word <- paste(word, c, sep="")
			}
		}
		
		else if (c=="=") {
			if (quoted==0 && equal==0) {
				if (word=="") {
					#found        =value
					.catError("unexpected '=' found", fname, line.start, line.end, sourcefile)
					return()
				}
				equal<-1
				key <- word
				word <- ""
			}
			else {
				word <- paste(word, c, sep="")
			}
		}
		
		#regular character found
		else {
			word <- paste(word, c, sep="")
		}
	}
	if (quoted > 0) {
		.catError("unterminated quote found", fname, line.start, line.end, sourcefile)
		return()
	}

	#save last key, value pair
	j <- j + 1
	words[[j]] <- list()
	if (equal==1) {
		words[[j]]$key=key
		key<-""
	}
	words[[j]]$value=word

	return(words)
}


# ***********************************************************
# .catError:
#   used to display parsing errors
# Arguments:
#   err        - error string to display
#   fname      - file name where error was found
#   line.start - starting line of widget with error
#   line.end   - end line of widget with error
#   sourcefile - source code of the file in question
#   errorType  - type of error to display
# -----------------------------------------------------------
.catError <- function(err, fname, line.start, line.end, sourcefile=list(), errorType="GUI parse error")
{
	err <- paste(errorType, " (", fname, ":", line.start, ") : ", err, "\n", sep="")
	cat(err)
	if (length(sourcefile)>0) {
		for(i in line.start:line.end) {
			cat(paste(i, ": ", sourcefile[[i]], "\n", sep=""))
		}
		cat("\n")
	}
}


# ***********************************************************
# .stopWidget:
#   Fatal error during window creation (not parse)
# Arguments:
#   err       - error string to display
#   wid.debug - list of widget code (created in parsing process
#   tclHash   - contains pointer to window to close
# -----------------------------------------------------------
.stopWidget <- function(err, wid.debug, tclHash)
{
	err <- paste("\nGUI parse error (", wid.debug$fname, ":", wid.debug$line.start, ") : ", err, "\n\n", sep="")

	if (length(wid.debug$sourceCode)>0) {
		j <- 0;
		for(i in wid.debug$line.start:wid.debug$line.end) {
			j <- j + 1
			err <- paste(err, i, ": ", wid.debug$sourceCode[j], "\n", sep="")
		}
	}

	tt <- tclHash$get(".PBS.tkwindow")[[1]]
	tkdestroy(tt)
	
	stop(err, call.=FALSE)
}


# ***********************************************************
# .getParamOrder:
#   contains the list of allowed and required widget arguments
# -----------------------------------------------------------
.getParamOrder <- function()
{
	paramOrder <- list()
	paramOrder$window   <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='name', required=FALSE, class="character", default="window"),
	                        list(param='title', required=FALSE, class="character", default=""),
	                        list(param='vertical', required=FALSE, class="logical", default=TRUE)
	                        )


	paramOrder$grid     <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='nrow', required=FALSE, class="integer", default=1),
	                        list(param='ncol', required=FALSE, class="integer", default=1),
	                        list(param='toptitle', required=FALSE, class="character", default=""),
	                        list(param='sidetitle', required=FALSE, class="character", default=""),
	                        list(param='topfont', required=FALSE, class="character", default=""),
	                        list(param='sidefont', required=FALSE, class="character", default=""),
	                        list(param='byrow', required=FALSE, class="logical", default=TRUE),
	                        list(param='borderwidth', required=FALSE, class="integer", default=1),
	                        list(param='relief', required=FALSE, class="character", default="flat", grep="^(raised|sunken|flat|ridge|groove|solid)$"),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )

	paramOrder$menu     <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='nitems', required=FALSE, class="integer", default=1),
	                        list(param='label', required=TRUE, class="character"),
	                        list(param='font', required=FALSE, class="character", default="") #only is valid on sub-menus and not at top level
	                        )

	paramOrder$menuitem     <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='label', required=TRUE, class="character"),
	                        list(param='font', required=FALSE, class="character", default=""),
	                        list(param='function', required=TRUE, class="character"),
	                        list(param='action', required=FALSE, class="character", default="menuitem")
	                        )


	paramOrder$label    <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='text', required=FALSE, class="character", default=""),
	                        list(param='font', required=FALSE, class="character", default=""),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )


	paramOrder$null    <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )


	paramOrder$entry    <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?$"),
	                        list(param='value', required=FALSE, class="character", default=""),
	                        list(param='width', required=FALSE, class="integer", default=20),
	                        list(param='label', required=FALSE, class="character", default=""),
	                        list(param='font', required=FALSE, class="character", default=""),
	                        list(param='function', required=FALSE, class="character", default=""),
	                        list(param='enter', required=FALSE, class="logical", default=TRUE), #require an enter to call function
	                        list(param='action', required=FALSE, class="character", default="entry"),
	                        list(param='mode', required=FALSE, class="character", default="numeric", grep="^(numeric|integer|complex|logical|character)$"),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )

	paramOrder$button   <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='text', required=FALSE, class="character", default="Calculate"),
	                        list(param='font', required=FALSE, class="character", default=""),
	                        list(param='width', required=FALSE, class="integer", default=0),
	                        list(param='function', required=FALSE, class="character", default=""),
	                        list(param='action', required=FALSE, class="character", default="button"),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )


	paramOrder$check    <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+$"),
	                        list(param='checked', required=FALSE, class="logical", default=FALSE),
	                        list(param='text', required=FALSE, class="character", default=""),
	                        list(param='font', required=FALSE, class="character", default=""),
	                        list(param='function', required=FALSE, class="character", default=""),
	                        list(param='action', required=FALSE, class="character", default="check"),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )
	                        
	                        
	paramOrder$radio    <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?$"),
	                        list(param='value', required=TRUE, class="character"),
	                        list(param='text', required=FALSE, class="character", default=""),
	                        list(param='font', required=FALSE, class="character", default=""),
	                        list(param='function', required=FALSE, class="character", default=""),
	                        list(param='action', required=FALSE, class="character", default="radio"),
	                        list(param='mode', required=FALSE, class="character", default="numeric", grep="^(numeric|integer|complex|logical|character)$"),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )


	paramOrder$slide    <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+$"),
	                        list(param='from', required=FALSE, class="integer", class="integer", default=0),
	                        list(param='to', required=FALSE, class="integer", default=100),
	                        list(param='value', required=FALSE, class="integer", default=NA),
	                        list(param='showvalue', required=FALSE, class="logical", default=FALSE),
	                        list(param='orientation', required=FALSE, class="character", default="horizontal"), #TODO: grep="^(horizontal|vertical)" - test this
	                        list(param='function', required=FALSE, class="character", default=""),
	                        list(param='action', required=FALSE, class="character", default="slide"),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )


	paramOrder$slideplus    <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+$"),
	                        list(param='from', required=FALSE, class="numeric", default=0),
	                        list(param='to', required=FALSE, class="numeric", default=1),
	                        list(param='by', required=FALSE, class="numeric", default=0.01),
	                        list(param='value', required=FALSE, class="numeric", default=NA),
	                        list(param='function', required=FALSE, class="character", default=""),
	                        list(param='enter', required=FALSE, class="logical", default=FALSE), #require an enter to change min/max values
	                        list(param='action', required=FALSE, class="character", default="slideplus"),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )
	                        
	                        
	paramOrder$vector   <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='names', required=TRUE, class="character", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?([ \t]+([a-zA-Z0-9])+(\\[[0-9,]+\\])?)*$"),
	                        list(param='length', required=FALSE, class="integer", default=0),
	                        list(param='labels', required=FALSE, class="characterVector", default=""),
	                        list(param='values', required=FALSE, class="characterVector", default=""),
	                        list(param='font', required=FALSE, class="character", default=""),
	                        list(param='vertical', required=FALSE, class="logical", default=FALSE),
	                        list(param='function', required=FALSE, class="character", default=""),
	                        list(param='enter', required=FALSE, class="logical", default=TRUE),
	                        list(param='action', required=FALSE, class="character", default="vector"),
	                        list(param='mode', required=FALSE, class="character", default="numeric", grep="^(numeric|integer|complex|logical|character)$"),
	                        list(param='width', required=FALSE, class="integer", default=6, grep="^[0-9]+$"),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )


	paramOrder$matrix   <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='nrow', required=TRUE, class="integer"),
	                        list(param='ncol', required=TRUE, class="integer"),
							list(param='names', required=TRUE, class="characterVector", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?([ \t]+([a-zA-Z0-9])+(\\[[0-9,]+\\])?)*$"), #variable names (or name)
	                        list(param='rowlabels', required=FALSE, class="characterVector", default=""),
	                        list(param='collabels', required=FALSE, class="characterVector", default=""),
	                        list(param='rownames', required=FALSE, class="characterVector", default=""),
	                        list(param='colnames', required=FALSE, class="characterVector", default=""),
	                        list(param='font', required=FALSE, class="character", default=""),
	                        list(param='values', required=FALSE, class="characterVector", default=""), #variable names (or name)
	                        list(param='byrow', required=FALSE, class="logical", default=TRUE),
	                        list(param='function', required=FALSE, class="character", default=""),
	                        list(param='enter', required=FALSE, class="logical", default=TRUE),
	                        list(param='action', required=FALSE, class="character", default="matrix"),
	                        list(param='mode', required=FALSE, class="character", default="numeric", grep="^(numeric|integer|complex|logical|character)$"),
	                        list(param='width', required=FALSE, class="integer", default=6, grep="^[0-9]+$"),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )

	paramOrder$data   <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='nrow', required=TRUE, class="integer"),
	                        list(param='ncol', required=TRUE, class="integer"),
	                        list(param='names', required=TRUE, class="character", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?([ \t]+([a-zA-Z0-9])+(\\[[0-9,]+\\])?)*$"), #variable names (or name)
	                        list(param='modes', required=FALSE, class="character", default="numeric", grep="^(numeric|integer|complex|logical|character)([ \t]+(numeric|integer|complex|logical|character))*$"),
	                        list(param='rowlabels', required=FALSE, class="characterVector", default=""),
	                        list(param='collabels', required=FALSE, class="characterVector", default=""),
	                        list(param='rownames', required=FALSE, class="characterVector", default="X"),
	                        list(param='colnames', required=FALSE, class="characterVector", default="Y"),
	                        list(param='font', required=FALSE, class="character", default=""),
	                        list(param='values', required=FALSE, class="characterVector", default=""), #variable names (or name)
	                        list(param='byrow', required=FALSE, class="logical", default=TRUE),
	                        list(param='function', required=FALSE, class="character", default=""),
	                        list(param='enter', required=FALSE, class="logical", default=TRUE),
	                        list(param='action', required=FALSE, class="character", default="data"),
	                        list(param='width', required=FALSE, class="integer", default=6, grep="^[0-9]+$"),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )

	paramOrder$history  <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='name', required=FALSE, class="character", default="default", grep="^([a-zA-Z0-9]+)$"),
	                        list(param='archive', required=FALSE, class="logical", default=TRUE),
	                        list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S)?(e|w|E|W)?$"),	#choices: N,NE,E,SE,S,SW,W,NW
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )

	paramOrder$text  <-  list(
	                        list(param='type', required=TRUE, class="character"),
	                        list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+$"),
	                        list(param='height', required=FALSE, class="integer", grep="^([0-9])+$", default=8),
	                        list(param='width', required=FALSE, class="integer", grep="^([0-9])+$", default=30),
	                        list(param='edit', required=FALSE, class="logical", default=FALSE),
	                        list(param='bg', required=FALSE, class="character", grep="^(white|gray|red|#[0-9A-F]{3}|#[0-9A-F]{6})$", default="white"),
	                        list(param='mode', required=FALSE, class="character", default="character", grep="^(numeric|integer|complex|logical|character)$"),
	                        list(param='font', required=FALSE, class="character", default=""),
	                        list(param='value', required=FALSE, class="character", default=""),
	                        list(param='borderwidth', required=FALSE, class="integer", default=1),
	                        list(param='relief', required=FALSE, class="character", default="sunken", grep="^(raised|sunken|flat|ridge|groove|solid)$"),
	                        list(param='edit', required=FALSE, class="logical", default=TRUE), #t=user can change text
	                        list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	                        list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	                        )

 

	return(paramOrder)
}


# ***********************************************************
# .getParamFromStr:
#   returns a list with all parameters extracted from a list
# Arguments:
#   lots - TODO
# -----------------------------------------------------------
.getParamFromStr <- function(inputStr, fname="", line.start=0, line.end=0, sourcefile=list(), paramOrder=.getParamOrder())
{
	#now passed in function - to enable overriding
	# a "constant" defines how the parameters should look.
	#paramOrder <- .getParamOrder()
	
	
	namedArguments <- 0 # determines if we can accept unnamed arguments
	paramData <- list() #extracted params from a line to return
	typeDefined <- 0 #this must be set before returning

	if (inputStr=="") {
		.catError(paste("input line is empty", sep=""), fname, line.start)
		return(NULL)
	}
	
	#split inputed string into seperate arguments
	s<-.convertPararmStrToList(inputStr, fname, line.start, line.end, sourcefile)

	
	for(j in 1:length(s)) {
	
		value<-s[[j]]$value
		
		#argument is named, unnamed arguments are no longer valid	
		if (!is.null(s[[j]]$key)) {
			namedArguments <- 1 
			
			key<-casefold(.trimWhiteSpace(s[[j]]$key))
			
			paramData[[key]] <- value
			if (typeDefined==0) {
				if (key=="type") {
					typeDefined <- 1;
					
					#case of type is ignored
					paramData[[key]] <- value <- casefold(value, upper=FALSE) 
					
					#fetch argument Ordering
					#eval(parse(text=paste("argOrder <- paramOrder$", value)))
					argOrder <- paramOrder[[value]]
				}
			}
		}
		
		#argument is not named (no key was given)
		else if(namedArguments==0) {
			if (j==1) {
				#first argument must be "type"
				widgetType <- paramData$type <- casefold(value)
				
				#fetch argument Ordering
				#eval(parse(text=paste("argOrder <- paramOrder$", widgetType)))
				argOrder <- paramOrder[[widgetType]]
				if (is.null(argOrder)) {
					#widget type is not valid
					.catError(paste("unknown widget type '", paramData$type,"'", sep=""), fname, line.start, line.end, sourcefile)
					return(NULL)
				}
				typeDefined <- 1;
			}
			else if(j > length(argOrder)) {
				.catError(paste("more arguments given than supported by widget '",paramData$type,"'", sep=""), fname, line.start, line.end, sourcefile)
				return(NULL)
			}
			else {
				#determine the name of the second argument
				argName <- argOrder[[j]]$param
				#eval(parse(text=paste("paramData$", argName, " <- value")))
				paramData[[argName]] <- value
			}
		}
		
		#error - unnamed arg given after named arg
		else {
			.catError(paste("unnamed argument with value \"",value,"\" given after a named argument.", sep=""), fname, line.start, line.end, sourcefile)
			return(NULL)
		}
	}

	#test if a type has been defined
	if (typeDefined==0) {
		.catError(paste("no widget type given", sep=""), fname, line.start, line.end, sourcefile)
		return(NULL)
	}
	
	#check that widget type is valid
	if(.isReallyNull(paramOrder, paramData$type)) {
		.catError(paste("unknown widget type '", paramData$type,"'", sep=""), fname, line.start, line.end, sourcefile)
		return(NULL)
	}


	#test if all given arguments are valid arguments of the given type
	errorFound <- 0
	givenNames <- names(paramData)
	for(i in 1:length(givenNames)) {
		pos <- .searchCollection(argOrder, givenNames[i])
		if (pos==-1) {
			#argument name not valid
			.catError(paste('argument \'', givenNames[i], '\' is not supported by widget \'', paramData$type, '\'', sep=""), fname, line.start, line.end, sourcefile)
			errorFound <- 1
		}
		else if (pos == -2) {
			.catError(paste('argument \'', givenNames[i], '\' of widget \'', paramData$type, '\' matches multiple formal arguments.', sep=""), fname, line.start, line.end, sourcefile)
			errorFound <- 1
		}
		else {
			#sometimes, only a bit of the paramater name is given
			#if so, we would like to know the full name
			fullParamName <- argOrder[[pos]]$param
			if (fullParamName != givenNames[i]) {
				names(paramData)[i]=fullParamName
			}
			
			#check supplied argument data matches grep pattern (if defined)
			if (!is.null(argOrder[[pos]]$grep)) {
				if (!any(grep(argOrder[[pos]]$grep, paramData[[i]]))) {
					#supplied data is not formatted correctly
					
					.catError(paste('argument \'', givenNames[i], '\': value \'', paramData[[i]], '\' is not accepted. It should match', argOrder[[pos]]$grep , sep=""), 	fname, line.start, line.end, sourcefile)
					errorFound <- 1
				}
			}
			
			#some strings - if character need to be stripped of slashes, or sub-divided
			if (errorFound == 0 && !is.null(argOrder[[pos]]$class)) {
				if (argOrder[[pos]]$class=="character") {
					#convert value to either a single string (.stripSlashes)
					tmp <- .stripSlashes(paramData[[i]], fname, line.start, line.end, sourcefile)
					if (is.null(tmp)) {
						#most likely an unescaped quote was found - .catError called from .stripSlashes, not here
						errorFound <- 1
					}
					else
						paramData[[i]] <- tmp
				}
				else if (argOrder[[pos]]$class=="characterVector") {
					#convert value to a vector of strings
					argOrder[[pos]]$class="character"
					tmp <- .stripSlashesVec(paramData[[i]], fname, line.start, line.end, sourcefile)
					if (is.null(tmp)) {
						errorFound <- 1
					}
					else
						paramData[[i]] <- tmp
				}
			}
			

			#convert data from string to another data class
			if (errorFound == 0 && !is.null(argOrder[[pos]]$class)) {
				if ((argOrder[[pos]]$class=="numeric" || argOrder[[pos]]$class=="integer") && any(grep("[a-z]", paramData[[i]], ignore.case=TRUE))) {
					paramData[[i]] <- 0
				}
				else if (argOrder[[pos]]$class=="logical" && !any(grep("^(F|T|FALSE|TRUE|false|true)$", paramData[[i]]))) {
					paramData[[i]] <- FALSE
				}
				else {
					paramData[[i]] <- as(paramData[[i]], argOrder[[pos]]$class)
				}
			}
		}
	}
	if (errorFound != 0)
		return(NULL)


	#check that all required arguments have been supplied
	errorFound <- 0
	for(i in 1:length(argOrder)) {
		if (argOrder[[i]]$required) {
			if (.isReallyNull(paramData, argOrder[[i]]$param)) {
				.catError(paste('required argument \'', argOrder[[i]]$param, '\' is missing from widget \'', paramData$type, '\'', sep=""), fname, line.start, line.end, sourcefile)
				errorFound <- 1
			}
		}
		else if (!is.null(argOrder[[i]]$default)) {
			#fill in any default values if applicible
			#eval(parse(text=paste("tmp <- is.null(paramData$", argOrder[[i]]$param, ")", sep="")))

			if (is.null(paramData[[argOrder[[i]]$param]])) {
				#set it to default value
				#eval(parse(text=paste("paramData$", argOrder[[i]]$param, " <- argOrder[[i]]$default", sep="")))
				paramData[[argOrder[[i]]$param]] <- argOrder[[i]]$default
			}
		}
	}
	if (errorFound != 0)
		return(NULL)
	
	#add debug information to be used if there are any errors in building the GUI
	sourceCode <- c()
	if (line.start<=line.end && !missing(sourcefile)) {
		for(i in line.start:line.end) {
			sourceCode <- c(sourceCode, sourcefile[[i]])
		}
	}
	paramData$.debug <- list(sourceCode=sourceCode, fname=fname, line.start=line.start, line.end=line.end)

	return(paramData)
}


# ***********************************************************
# .buildgrid:
#   used to create a grid on a window
# Arguments:
#   tk      - parent tk frame/window to attach widget to
#   grid    - widget list describing the grid
#   tclHash - contains pointers
# -----------------------------------------------------------
.buildgrid <- function(tk, grid, tclHash)
{
	
	rows <- grid$data
	toptitle <- grid$toptitle
	sidetitle <- grid$sidetitle
	
	if (is.null(toptitle))
		toptitle <- ""
	
	if (is.null(sidetitle))
		sidetitle <- ""
	
	if (is.null(grid$ncol)) {
		grid$ncol=length(rows[[1]])
	}
	
	if (is.null(grid$nrow)) {
		grid$nrow=length(rows)
	}


	#offset the title (useful for centering titles over a certain part)
	#like over the 3 columns of a matrix, but not row labels
	if (is.null(grid$toptitle.offset))
		grid$toptitle.offset<-0
	if (is.null(grid$sidetitle.offset))
		grid$sidetitle.offset<-0
	
	#set byrow
	if (is.null(grid$byrow)) {
		grid$byrow=TRUE
	}
	
	#set font options
	if (is.null(grid$topfont))
		topfont <- ""
	else
		topfont <- grid$topfont
		
	if (is.null(grid$sidefont))
		sidefont <- ""
	else
		sidefont <- grid$sidefont
		
	
	
	#display title (if set)
	if (toptitle!="") {
		colspan=as.integer(grid$ncol)-grid$toptitle.offset
		
		if (topfont!="") {
			font <- .createTkFont(topfont)
			tkgrid(tklabel(tk,text=toptitle,font=font), columnspan=colspan, row=0, column=1+grid$toptitle.offset)
		}
		else {
			tkgrid(tklabel(tk,text=toptitle), columnspan=colspan, row=0, column=1+grid$toptitle.offset)
		}
	}

	#display column title (if set)
	if (sidetitle!="") {
		rowspan=as.integer(grid$nrow)-grid$sidetitle.offset

		if (sidefont!="") {
			font<-.createTkFont(sidefont)
			tkgrid(tklabel(tk,text=sidetitle,font=font), rowspan=rowspan, row=1+grid$sidetitle.offset, column=0)
		}
		else {
			tkgrid(tklabel(tk,text=sidetitle), rowspan=rowspan, row=1+grid$sidetitle.offset, column=0)
		}
		showsidetitle<-TRUE
	}

	widget <- list()
	for(i in 1:length(rows)) {
		for(j in 1:length(rows[[i]])) {

			#create Widget			
			widget[[j]] <- .createWidget(tk, rows[[i]][[j]], tclHash)
			
			#set row and column position
			if (grid$byrow==TRUE) {
				row=i
				column=j
			}
			else {
				row=j
				column=i
			}
			#if (showtitle==TRUE)
			#	row <- row + 1
			#if (showsidetitle==TRUE)
			#	column <- column + 1

			#y padding
			if (is.null(rows[[i]][[j]]$pady))
				pady <- 0
			else
				pady <- rows[[i]][[j]]$pady
			
			#x padding
			if (is.null(rows[[i]][[j]]$padx))
				padx <- 0
			else
				padx <- rows[[i]][[j]]$padx
			
			#create initial call to attach widget to grid
			evalCmd <- paste("tkgrid(widget[[j]],row=", row, ",column=",column,",padx=",padx,",pady=",pady, sep="")
			
			#append sticky flag argument if set
			if (is.character(rows[[i]][[j]]$sticky)) {
				evalCmd <- paste(evalCmd, ",sticky=rows[[i]][[j]]$sticky", sep="")
			}
			
			
			#close function call, and eval it.
			evalCmd <- paste(evalCmd, ")", sep="")
			eval(parse(text=evalCmd))
		}
	}
	return(tk)
}


# ***********************************************************
# .createTkFont:
#   creates a usable TK font from a given string
# Arguments:
#   fontStr - string describing a font and colour
# -----------------------------------------------------------
.createTkFont <- function(fontStr)
{
	fontstr <- .convertPararmStrToVector(casefold(fontStr))
	
	#default options
	fontparam<-list()
	
	for(i in 1:length(fontstr)) {
		if (fontstr[i]=="bold")
			fontparam$weight="bold"
		else if (fontstr[i]=="italic")
			fontparam$slant="italic"
		else if (fontstr[i]=="underline")
			fontparam$underline=TRUE
		else if (fontstr[i]=="overstrike")
			fontparam$overstrike=TRUE
		else if (fontstr[i]=="times")
			fontparam$family="Times"
		else if (fontstr[i]=="courier")
			fontparam$family="Courier"
		else if (fontstr[i]=="helvetica")
			fontparam$family="Helvetica"
		else if (any(grep("^[0-9]+$", fontstr[i])))
			fontparam$size=fontstr[i]
		else if (fontstr[i] != "")
			cat(paste("warning: ignoring font option \"", fontstr[i], "\"\n", sep=""))
		
	}
	
	return(do.call(tkfont.create, fontparam))
}


# ***********************************************************
# .createWidget:
#   generic function to create most widgets, which
#   calls appropriate .createWidget.xxxWidgetTypexxx() func
# Arguments:
#   tk      - frame to attach widget to
#   widget  - widget list
#   tclHash - contains pointers
# -----------------------------------------------------------
.createWidget <- function(tk, widget, tclHash)
{
	#save functions
	if (!is.null(widget[["function"]])) {
		if (widget[["function"]]!="") {
			funcHash <- unlist(tclHash$get(".PBS.functions"))
			funcHash$add(widget[["function"]], TRUE)
		}
	}
	
	#save widget information by name parameter (and not widget$name)
	#widget name can sometimes be "foo[1,2,3]" or some such combo.
	#where as type="vector" name="foo" is never seen in the regular tclhash
	if (!is.null(widget$name)) {
		if (length(widget$name)==1) {
			nameHash <- unlist(tclHash$get(".PBS.names"))
			nameHash$add(widget$name, widget=widget)
		}
	}
	
	#look for a function called .createWidget.WIDGETTYPE
	#all of these functions have the same parameters: (tk, widget, tclHash)
	func <- paste(".createWidget.", widget$type, sep="")
	if (exists(func,mode="function")) {

		#set override values
		if (!is.null(widget$value) && !is.null(widget$name)) {
			override<-unlist(tclHash$get(".PBS.defaults"))
			if (!is.null(override[[widget$name]]))
				widget[["value"]] <- override[[widget$name]]
		}
		
		return(do.call(func, list(tk, widget, tclHash)))
	}
	else {
		stop(paste("Don't know how to create '", widget$type, "' widget\n", sep=""))
		return()
	}
	return(tkWidget)
}

.createWidget.grid <- function(tk, widget, tclHash)
{
  #these "defaults" only apply to the first layer grid
	#because it is added as padding, and not parsed.
	#all other defaults are set in paramOrder list
	
	
  if (is.null(widget$borderwidth))
		widget$borderwidth <- 5
		
	if (is.null(widget$relief))
		widget$relief <- "flat"
			
	tkWidget <- tkframe(tk,borderwidth=widget$borderwidth,relief=widget$relief)
	.buildgrid(tkWidget, widget, tclHash)
	
	return(tkWidget)
}

.createWidget.check <- function(tk, widget, tclHash)
{
	widget$mode = "logical"
	
	if (widget$checked==TRUE)
		val <- 1
	else
		val <- 0
	variable <- tclHash$add(widget$name, tclvar=tclVar(val), widget=widget)$tclvar
	if (is.null(widget$font))
		widget$font <- ""
	if (widget$font=="")
		tkWidget <- tkcheckbutton(tk,variable=variable, text=widget$text,command=function(...) { .extractData(widget[["function"]], widget$action, tclHash)})
	else
		tkWidget <- tkcheckbutton(tk,variable=variable, text=widget$text,font=.createTkFont(widget$font), command=function(...) { .extractData(widget[["function"]], widget$action, tclHash)})
	
	return(tkWidget)
}

.createWidget.label <- function(tk, widget, tclHash)
{
	#see http://www.python.net/crew/fredrik/tkmanual/font.html for font info
	if (is.null(widget$font))
		widget$font=""
	if (widget$font!="") {
		font <- .createTkFont(widget$font)
		
		tkWidget<-tklabel(tk,text=widget$text,font=font)
		return(tkWidget)
	}
	else {
		tkWidget<-tklabel(tk,text=widget$text)
		return(tkWidget)
	}
}

.createWidget.null <- function(tk, widget, tclHash)
{
	tkWidget<-tklabel(tk,text="")
	return(tkWidget)
}

.createWidget.matrix <- function(tk, widget, tclHash)
{
	
	nrow <- widget$nrow
	ncol <- widget$ncol
	
	names <- widget$names
	#TODO - check all names are valid
	
	rowlabels <- widget$rowlabels
	rownames <- widget$rownames
	collabels <- widget$collabels
	colnames <- widget$colnames
	
	if (all(widget$values==""))
		values <- ""
	else {
		values <- widget$values
		#dim(values) <- c(nrow, ncol)
	}
		

	wid <- list() #new grid widget to create
	wid$data <- list() #elements in the grid
	wid$byrow <- TRUE
	
	nNames <- length(names)
	nRowlabels <- length(rowlabels)
	nRowNames <- length(rownames)
	nValues <- length(values)
	nCollabels <- length(collabels)
	nColNames <- length(colnames)

	
	#count names
	if (nNames!=1 && nNames!=(ncol*nrow))
    	.stopWidget(paste('"names" argument must contain 1 or', ncol*nrow, 'names seperated by whitespace.'), widget$.debug, tclHash)
  
	#count rowlabels
	if (nRowlabels!=1 && nRowlabels!=nrow)
		.stopWidget(paste('"rowlabels" argument should contain 1 or', nrow, 'labels.'), widget$.debug, tclHash)
	
	#count collabels
	if (nCollabels!=1 && nCollabels!=ncol)
		.stopWidget(paste('"collabels" argument should contain 1 or',ncol,'labels.'), widget$.debug, tclHash)

	#count rownames
	if (nRowNames!=1 && nRowNames!=nrow)
		.stopWidget(paste('"rownames" argument should contain 1 or',nrow,'labels.'), widget$.debug, tclHash)
	
	#count colnames
	if (nColNames!=1 && nColNames!=ncol)
		.stopWidget(paste('"colnames" argument should contain 1 or',ncol,'labels.'), widget$.debug, tclHash)
	
	#single labels should be displayed as the title
	if (nCollabels==1 && ncol>1) {
		wid$toptitle<-collabels[1]
		wid$topfont<-widget$font
		wid$toptitle.offset<-1 #to help center the label
		#have counting labels above each column
		wid$data[[1]] <- list()
		wid$data[[1]][[1]] <- list(type='label', text="")
		for(j in 1:ncol) {
			wid$data[[1]][[j+1]] <- list(type='label', text=j, font=widget$font)
		}
	}
	else {

		wid$data[[1]] <- list()
		wid$data[[1]][[1]] <- list(type='label', text="")
		for(j in 1:ncol) {
			wid$data[[1]][[j+1]] <- list(type='label', text=collabels[j], font=widget$font)
		}
	}
	
	#row title
	if (nRowlabels==1 && nrow>1) {
		wid$sidetitle<-rowlabels[1]
		wid$sidefont<-widget$font
		wid$sidetitle.offset<-1 #to help center the label
	}

	for(i in 1:nrow) {
		rowCount <- i #the first row of inputs should be 1 (even if there are labels ontop)
		i <- i + 1 #first row has labels
		wid$data[[i]] <- list()
		for(j in 1:(ncol+1)) {
			#first row is for labels
			if (j==1) {
				if (nRowlabels==1 && nrow>1) {
					text <- as.character(rowCount)
				}
				else
					text <- rowlabels[rowCount]

				wid$data[[i]][[j]] <- list(type='label', text=text, font=widget$font)
			}
			else {
				if (nNames==1) #single name given
					name <- paste(names,'[',rowCount,',',j-1,']',sep="")
				else #many names given
					if (widget$byrow)
						name <- names[j-1+(ncol*(i-2))]
					else
						name <- names[i-1+(nrow*(j-2))]
				if (nValues==1) {
					value <- values
				}
				else {
					if (widget$byrow)
						value <- values[j-1+(ncol*(i-2))]
					else
						value <- values[i-1+(nrow*(j-2))]
				}
				
				if (widget$mode=="logical") {
					#display a checkbox
					if (is.na(as.logical(value)))
						checked=FALSE
					else if (as.logical(value)==TRUE)
						checked=TRUE
					else
						checked=FALSE
					wid$data[[i]][[j]] <- list(
						  type='check',
						  mode="logical",
						  name=name,
						  text="",
						  "function"=widget[["function"]],
						  action=widget$action,
						  checked=checked
					)
				}
				else {
					#display a entry box
					wid$data[[i]][[j]] <- list(
						  type='entry', 
						  name=name,
						  "function"=widget[["function"]],
						  action=widget$action,
						  enter=widget$enter,
						  value=value,
						  width=widget$width,
						  mode=widget$mode
					)
				
				}
			}
		}	
	}
	

	tkWidget <- .createWidget.grid(tk, wid, tclHash)
	
	return(tkWidget)
}

.createWidget.vector <- function(tk, widget, tclHash)
{
	
	#variable <- tclHash$add(widget$names, tclVar(6))
	#tkWidget <- tkcheckbutton(tk,variable=variable, text='test')
	
	names <- unlist(strsplit(widget$names, "( |\t)+"))
	labels <- widget$labels
	
	if (all(labels==""))
		labels <- names
		
	if (all(widget$values==""))
		values <- ""
	else
		values <- widget$values
	
	n <- widget$length
	wid <- list() #new grid widget to create
	wid$byrow = widget$vertical #pass byrow param to grid

	
	nNames <- length(names)
	nLabels <- length(labels)
	
	if (n==0) {
		if (nNames != nLabels && nNames != 1 && nLabels != 1)
			.stopWidget('"labels" and "names" arguments should have the same amount of substrings.', widget$.debug, tclHash)
		if (nNames == 1 && nLabels == 1) {
			n<-1
		}
		else if (nNames != 1)
			n<-nNames
		else
			.stopWidget('missing "length" argument', widget$.debug, tclHash)
	}

	if (widget$vertical) {
		wid$nrow <- n
		wid$ncol <- 2
		wid$toptitle.offset=1
		wid$toptitle=""
	}
	else {
		wid$nrow <- 2
		wid$ncol <- n
	
	}
		
	#count names
	if (nNames!=1 && nNames!=n)
		.stopWidget(paste("names argument must contain 1 or",n,"names seperated by spaces.\nreceived:", widget$names), widget$.debug, tclHash)
  
	#count labels
	if (nLabels!=1 && nLabels!=n)
		.stopWidget(paste('labels argument should contain 1 or',n,'labels.'), widget$.debug, tclHash)
			
	#single labels should be displayed as the title
	if (nLabels==1 && n!=1) {
		wid$toptitle=labels[1]
		wid$topfont<-widget$font
	}
  
	nValues <- length(values)
	if (nValues!=1 && nValues!=n)
		.stopWidget(paste('values argument should contain 1 or',n,'values seperated by whitespace.'), widget$.debug, tclHash)
	
	#create children to be placed in the grid
	wid$data <- list()
	for(i in 1:n) {
		wid$data[[i]] <- list()
	
		#create label
		if (nLabels==1 && n!=1)
			text <- as.character(i)
		else
			text <- labels[i]
	
		wid$data[[i]][[1]] <- list(type='label', text=text, font=widget$font)

		#create entry
		if (nNames==1)
			name <- paste(names, '[', i, ']',sep="")
		else
			name <- names[i]
		
		if (nValues==1)
			value <- values[1]
		else
			value <- values[i]

		if (widget$mode=="logical") {
			#display a checkbox
			if (is.na(as.logical(value)))
				checked=FALSE
			else if (as.logical(value)==TRUE)
				checked=TRUE
			else
				checked=FALSE
			wid$data[[i]][[2]] <- list(
				  type='check',
				  mode="logical",
				  name=name,
				  text="",
				  "function"=widget[["function"]],
				  action=widget$action,
				  checked=checked
			)
		}
		else {
			#display a entry box
			wid$data[[i]][[2]] <- list(
				  type='entry', 
				  name=name,
				  "function"=widget[["function"]],
				  action=widget$action,
				  enter=widget$enter,
				  value=value,
				  width=widget$width,
				  mode=widget$mode
			)
		
		}

	}

	tkWidget <- .createWidget.grid(tk, wid, tclHash)
	return(tkWidget)
}

.createWidget.data <- function(tk, widget, tclHash)
{

	nrow <- widget$nrow
	ncol <- widget$ncol
	
	names <- unlist(strsplit(widget$names, "( |\t)+"))
	modes <- unlist(strsplit(widget$modes, "( |\t)+"))

	
	rowlabels <- widget$rowlabels
	rownames <- widget$rownames
	collabels <- widget$collabels
	colnames <- widget$colnames
	if (all(widget$values==""))
		values <- ""
	else {
		values <- widget$values
		#dim(values) <- c(nrow, ncol)
	}

	wid <- list() #new grid widget to create
	wid$data <- list() #elements in the grid
	wid$byrow <- TRUE
	
	nNames <- length(names)
	nModes <- length(modes)
	nRowlabels <- length(rowlabels)
	nRowNames <- length(rownames)
	nValues <- length(values)
	nCollabels <- length(collabels)
	nColNames <- length(colnames)
	
	#count names
	if (nNames!=1 && nNames!=(ncol*nrow))
		.stopWidget(paste('names argument must contain 1 or',ncol*nrow,'names seperated by whitespace.'), widget$.debug, tclHash)

	#count modes
	if (nModes!=1 && nModes!=ncol)
		.stopWidget(paste('modes argument must contain 1 or',ncol,'modes seperated by whitespace.'), widget$.debug, tclHash)
    	
	#count rowlabels
	if (nRowlabels!=1 && nRowlabels!=nrow)
		.stopWidget(paste('rowlabels should contain 1 or',nrow,'labels.'), widget$.debug, tclHash)
	
	#count rownames
	if (nRowNames!=1 && nRowNames!=nrow)
		.stopWidget(paste('rownames argument should contain 1 or',nrow,'labels.'), widget$.debug, tclHash)
	
	#count collabels
	if (nCollabels!=1 && nCollabels!=ncol)
		.stopWidget(paste('collabels argument should contain 1 or',ncol,'labels.'), widget$.debug, tclHash)
	
	#count colnames
	if (nColNames!=1 && nColNames!=ncol)
		.stopWidget(paste('colnames argument should contain 1 or',ncol,'labels.'), widget$.debug, tclHash)
	
	#single labels should be displayed as the title
	if (nCollabels==1 && ncol>1) {
		wid$toptitle<-collabels[1]
		wid$topfont<-widget$font
		wid$toptitle.offset<-1 #to help center the label
		#have counting labels above each column
		wid$data[[1]] <- list()
		wid$data[[1]][[1]] <- list(type='label', text="")
		for(j in 1:ncol) {
			wid$data[[1]][[j+1]] <- list(type='label', text=j, font=widget$font)
		}
	}
	else {
		wid$data[[1]] <- list()
		wid$data[[1]][[1]] <- list(type='label', text="")
		for(j in 1:ncol) {
			wid$data[[1]][[j+1]] <- list(type='label', text=collabels[j], font=widget$font)
		}
	}
	
	#row title
	if (nRowlabels==1 && nrow>1) {
		wid$sidetitle<-rowlabels[1]
		wid$sidefont<-widget$font
		wid$sidetitle.offset<-1 #to help center the label
	}

	for(i in 1:nrow) {
		rowCount <- i #the first row of inputs should be 1 (even if there are labels ontop)
		i <- i + 1 #first row has labels
		wid$data[[i]] <- list()
		for(j in 1:(ncol+1)) {
			#first row is for labels
			if (j==1) {
				if (nRowlabels==1 && nrow>1) {
					text <- as.character(rowCount)
				}
				else
					text <- rowlabels[rowCount]

				wid$data[[i]][[j]] <- list(type='label', text=text, font=widget$font)
			}
			else {
				if (nNames==1) #single name given
					name <- paste(names,'[',rowCount,',',j-1,']d',sep="")
				else #many names given
					if (widget$byrow)
						name <- names[j-1+(ncol*(i-2))]
					else
						name <- names[i-1+(nrow*(j-2))]
				if (nValues==1) {
					value <- values
				}
				else {
					if (widget$byrow)
						value <- values[j-1+(ncol*(i-2))]
					else
						value <- values[i-1+(nrow*(j-2))]
				}
				if (nModes==1)
					mode <- modes[1]
				else
					mode <- modes[j-1] #columns are offset by one
				
				#TODO get column mode if multiple modes were given.
				
				if (mode=="logical") {
					#display a checkbox
					if (is.na(as.logical(value)))
						checked=FALSE
					else if (as.logical(value)==TRUE)
						checked=TRUE
					else
						checked=FALSE
					wid$data[[i]][[j]] <- list(
						  type='check',
						  mode="logical",
						  name=name,
						  text="",
						  "function"=widget[["function"]],
						  action=widget$action,
						  checked=checked
					)
				}
				else {
					#display a entry box
					wid$data[[i]][[j]] <- list(
						  type='entry', 
						  name=name,
						  "function"=widget[["function"]],
						  action=widget$action,
						  enter=widget$enter,
						  value=value,
						  width=widget$width,
						  mode=mode
					)
				}
			}
		}	
	}
	
	tkWidget <- .createWidget.grid(tk, wid, tclHash)
	return(tkWidget)
}

.createWidget.entry <- function(tk, widget, tclHash)
{
	if (!is.null(widget$label))
	if (widget$label!="") {
		#if label is set, then create a 2x1 grid
		label <- widget$label
		widget$label <- "" #blank it out, inf loop if not.
		newgridwidget <-
		list(type="grid", nrow=1, ncol=2, font="", byrow=TRUE, borderwidth=1, relief="flat", padx=0, pady=0, data=
	    	list(
				list(
					list(type="label", text=label, padx=0, pady=0, font=widget$font), 
					widget
				)
			)
		)
		return(.createWidget.grid(tk, newgridwidget, tclHash))
	}
	variable<-tclHash$add(widget$name, tclvar=tclVar(widget$value), widget=widget)$tclvar
	tkWidget <- tkentry(tk,textvariable=variable, width=widget$width)
	
	enter <- !is.null(widget$enter)
	if (enter)
		enter <- widget$enter
	
	if (enter) {
		tkbind(tkWidget,"<KeyRelease>",function(...) { .extractData(NULL, widget$action, tclHash)});
		tkbind(tkWidget,"<KeyPress-Return>",function(...) { .extractData(widget[["function"]], widget$action, tclHash)});
	}
	else
		tkbind(tkWidget,"<KeyRelease>",function(...) { .extractData(widget[["function"]], widget$action, tclHash)});
	return(tkWidget)
}

.createWidget.radio <- function(tk, widget, tclHash)
{
	variable<-tclHash$add(widget$name, tclvar=tclVar(widget$value), widget=widget)$tclvar

	if (is.null(widget$font))
		widget$font <- ""
	if (widget$font=="")
		tkWidget <- tkradiobutton(tk, text=widget$text, value=widget$value, variable=variable, command=function(...) { .extractData(widget[["function"]], widget$action, tclHash)})
	else
		tkWidget <- tkradiobutton(tk, text=widget$text, font=.createTkFont(widget$font), value=widget$value, variable=variable, command=function(...) { .extractData(widget[["function"]], widget$action, tclHash)})

	return(tkWidget)
}

.createWidget.slide <- function(tk, widget, tclHash)
{
	
	if (is.null(widget$value)) {
		widget$value <- widget$to
	}
	
	variable<-tclHash$add(widget$name, tclvar=tclVar(widget$value), widget=widget)$tclvar
	tkWidget <- tkscale(tk, from=widget$from, to=widget$to, orient=widget$orientation, showvalue=widget$showvalue, variable=variable, command=function(...) { .extractData(widget[["function"]], widget$action, tclHash)})
	return(tkWidget)
}

.createWidget.slideplus <- function(tk, widget, tclHash)
{
	#initial widget$value defaults to <from> argument
	if (is.na(widget$value))
		widget$value <- widget$from
	
	#to remember last valid number
	lastMinVal <- ""
	lastCurVal <- ""
	lastMaxVal <- ""
	
	#command to update min/max changes
	updateSlideBounds <- function(slider, slideVar, curVar, minVar, maxVar, widget, tclHash)
	{
		minVal <- tclvalue(minVar)
		curVal <- tclvalue(curVar)
		maxVal <- tclvalue(maxVar)

		#change min
		if (any(grep("^-?(([0-9]+(\\.[0-9]*)?)|([0-9]*\\.[0-9]+))$",minVal))) {
			minVal<-as.numeric(minVal)
			tkconfigure(slider,from=minVal/widget$by);
			tclvalue(minVar)<-minVal
		}
		else {
			#reset min to the last valid "-from" parameter of the slider
			if (!any(grep("^-?\\.?$",minVal)))
				tclvalue(minVar)<-lastMinVal 
			
		}
		
		#change max
		if (any(grep("^-?(([0-9]+(\\.[0-9]*)?)|([0-9]*\\.[0-9]+))$",maxVal))) {
			maxVal<-as.numeric(maxVal)
			tkconfigure(slider,to=maxVal/widget$by);
			tclvalue(maxVar)<-maxVal
		}
		else {
			#reset max to the last valid "-to" parameter of the slider
			if (!any(grep("^-?\\.?$",maxVal)))
				tclvalue(maxVar)<-lastMaxVal
		}

		#change current
		if (any(grep("^-?(([0-9]+(\\.[0-9]*)?)|([0-9]*\\.[0-9]+))$",curVal))) {
			tclvalue(slideVar)<-round(as.numeric(curVal)/widget$by)
			.extractData(widget[["function"]], widget$action, tclHash)
		}
		else {
			#reset max to the last valid "-to" parameter of the slider
			if (!any(grep("^-?\\.?$",maxVal)))
				tclvalue(maxVar)<-lastMaxVal
		}
		if (!any(grep("^-?[0-9]*(\\.[0-9]*$)?",curVal))) {
			tclvalue(curVar)<-lastCurVal
		}
	}
	
	saveSlideBounds <- function(slider, curVar, minVar, maxVar)
	{
		minVal <<- tclvalue(minVar)
		curVal <<- tclvalue(curVar)
		maxVal <<- tclvalue(maxVar)
		
		if (any(grep("^-?[0-9]*$",minVal)))
			lastMinVal <<- minVal
		if (any(grep("^-?[0-9]*$",curVal)))
			lastCurVal <<- curVal
		if (any(grep("^-?[0-9]*$",maxVal)))
			lastMaxVal <<- maxVal
	}
	
	convertCurVal <- function(widget, slideVar, curVar)
	{
		tclvalue(curVar) <- as.numeric(tclvalue(slideVar))*widget$by
	}

	#calculate fractional values
	from <- widget$from / widget$by
	to <- widget$to / widget$by
	
	if (is.null(widget$value)) {
		value <- to
		widget$value <- widget$to
	}
	else {
		value <- widget$value / widget$by
	}
	
	curVar<-tclHash$add(widget$name, tclvar=tclVar(widget$value), widget=widget, itestyou="fooppy")$tclvar #this one is the fractional value
	slideVar<-tclHash$add(paste(".", widget$name, ".slide", sep=""), tclvar=tclVar(value), widget=widget)$tclvar #interger
	minVar<-tclHash$add(paste(widget$name, ".min", sep=""), tclvar=tclVar(widget$from), widget=widget)$tclvar
	maxVar<-tclHash$add(paste(widget$name, ".max", sep=""), tclvar=tclVar(widget$to), widget=widget)$tclvar
	
	#hold the widgets in this frame
	tkWidget <- tkframe(tk)
	
	slider <- tkscale(tkWidget, from=from, to=to, orient="horizontal", showvalue=FALSE, variable=slideVar, command=function(...) { convertCurVal(widget, slideVar, curVar); .extractData(widget[["function"]], widget$action, tclHash)})

	#insert slider
	tkgrid(slider, columnspan=5, row=1, column=1)
	
	#create entries
	#tkWidget <- tkentry(tk,textvariable=variable, width=widget$width)
	#
	minWid <- tkentry(tkWidget,textvariable=minVar, width=5)
	curWid <- tkentry(tkWidget,textvariable=curVar, width=5)
	maxWid <- tkentry(tkWidget,textvariable=maxVar, width=5)
	
	if (widget$enter) {
		tkbind(minWid,"<KeyRelease-Return>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, tclHash));
		tkbind(maxWid,"<KeyRelease-Return>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, tclHash));
		tkbind(curWid,"<KeyRelease-Return>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, tclHash));
	}
	else {
		#capture value before press (incase new value isnt valid)
		tkbind(minWid,"<KeyPress>",function() saveSlideBounds(slider, curVar, minVar, maxVar));
		tkbind(maxWid,"<KeyPress>",function() saveSlideBounds(slider, curVar, minVar, maxVar));
		tkbind(curWid,"<KeyPress>",function() saveSlideBounds(slider, curVar, minVar, maxVar));
		
		#capture value after key is received
		tkbind(minWid,"<KeyRelease>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, tclHash));
		tkbind(maxWid,"<KeyRelease>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, tclHash));
		tkbind(curWid,"<KeyRelease>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, tclHash));
	}
	
	#bind functions for setwinval() changes
	tclHash$set(paste(widget$name, ".min", sep=""), onChange=function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, tclHash))
	tclHash$set(paste(widget$name, ".max", sep=""), onChange=function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, tclHash))
	tclHash$set(widget$name, onChange=function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, tclHash))
	
	
	#place widgets in grid
	tkgrid(tklabel(tkWidget, text="Min->"), row=2, column=1)
	tkgrid(minWid, row=2, column=2)
	tkgrid(curWid, row=2, column=3)
	tkgrid(maxWid, row=2, column=4)
	tkgrid(tklabel(tkWidget, text="<-Max"), row=2, column=5)
	
	return(tkWidget)
}

.createWidget.button <- function(tk, widget, tclHash)
{
	param <- list(parent=tk, text=widget$text)
	if (widget$font != "")
		param$font=.createTkFont(widget$font)
	if (is.numeric(widget$width) && widget$width > 0)
		param$width=widget$width
		
	if (widget[["function"]]!="")
		param$command=function(...) { .extractData(widget[["function"]], widget$action, tclHash) }
	
	return(do.call(tkbutton, param))
}

.createWidget.text <- function(tk, widget, tclHash)
{
	tk <- tkframe(tk)
	
	param <- list(
	              parent=tk, 
	              bg=widget$bg, 
	              height=widget$height, 
	              width=widget$width,
	              relief=widget$relief,
	              yscrollcommand=function(...)tkset(scrollBar,...)
	              )
	if (widget$font != "")
		param$font=.createTkFont(widget$font)
	
	scrollBar <- tkscrollbar(tk, repeatinterval=5, command=function(...)tkyview(txtBox,...))
	txtBox <- do.call(tktext, param)

	tclHash$add(widget$name, tclwidget=txtBox, widget=widget)
	tkinsert(txtBox,"end",widget$value)
	
	if (widget$edit==FALSE)
		tkconfigure(txtBox, state="disabled")
	
	tkgrid(txtBox,scrollBar)
	tkgrid.configure(scrollBar,sticky="ns")
	
	return(tk)
}

#history widget creates a bunch of standard PBS widgets, which call the appropriate functions
#as if a power-user created history
.createWidget.history <- function(tk, widget, tclHash)
{
	indexname=paste("PBS.history.", widget$name, ".index", sep="") #widget name that stores/displays the index number
	sizename=paste("PBS.history.", widget$name, ".size", sep="") #widget name that displays the size of history
	
	initPBShistory(widget$name, indexname=indexname, sizename=sizename) #initialize a list to be used once the window is created
	
	historyGrid <- list(
	type="grid", nrow=6, ncol=2, font="", byrow=TRUE, borderwidth=1, relief="sunken", padx=widget$padx, pady=widget$pady, data=
    	list(
			list(
				list(type="button", text="<- Back", font="", width=9, "function"="backPBShistory", action=widget$name, sticky="", padx=0, pady=0),
				list(type="button", text="Forward ->", font="", width=9, "function"="forwPBShistory", action=widget$name, sticky="", padx=0, pady=0)
			),
			list(
				list(type="label", text="Index", font="", sticky="", padx=0, pady=0),
				list(type="label", text="Size", font="", sticky="", padx=0, pady=0)
			),
			list(
				list(type="entry", name=indexname, value="0", width=9, label="", font="", "function"="jumpPBShistory", action=widget$name, enter=TRUE, mode="numeric", padx=0, pady=0),
				list(type="entry", name=sizename, value="0", width=9, label="", font="", action="", enter=TRUE, mode="numeric", padx=0, pady=0),
			),
			list(
				list(type="button", text="Save", font="", width=9, "function"="addPBShistory", action=widget$name, sticky="", padx=0, pady=0),
				list(type="button", text="Remove", font="", width=9, "function"="rmPBShistory", action=widget$name, sticky="", padx=0, pady=0)
			),
			list(
				list(type="button", text="Import", font="", width=9, "function"="importPBShistory", action=widget$name, sticky="", padx=0, pady=0),
				list(type="button", text="Export", font="", width=9, "function"="exportPBShistory", action=widget$name, sticky="", padx=0, pady=0)
			),
			list(
				list(type="button", text="Clear All", font="", width=9, "function"="clearPBShistory", action=widget$name, sticky="", padx=0, pady=0),
				list(type="label", text="", sticky="", padx=0, pady=0)
			)
		)
	)
	
	if (!widget$archive) {
		historyGrid$nrow=5
		historyGrid$data[[5]] <- historyGrid$data[[6]]
		historyGrid$data[[6]] <- NULL
	}
	
	return(.createWidget.grid(tk, historyGrid, tclHash))
}


# ***********************************************************
# backPBShistory:
#   move back in history
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
backPBShistory <- function(hisname="")
{
	if (hisname=="") hisname <- PBS.win$action[1]
	
	if (!is.list(PBS.history)) 
		stop("History not intialized - see initPBShistory function help")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initPBShistory", sep=""))
	
	i <- PBS.history[[hisname]][[1]]$index
	if (i < 2) {
		cat("history widget: warning, current position is already at front of history list.\n")
		return()
	}
	PBS.history[[hisname]][[1]]$index <<- i <- i-1
	setWinVal(PBS.history[[hisname]][[i+1]]) #i is always one lower
	.updatePBShistory(hisname)
}


# ***********************************************************
# forwPBShistory:
#   move forward in history
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
forwPBShistory <- function(hisname="")
{
	if (hisname=="") 
		hisname <- PBS.win$action[1]
	
	if (!is.list(PBS.history)) 
		stop("History not intialized - see initPBShistory")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initPBShistory", sep=""))
	
	i <- PBS.history[[hisname]][[1]]$index
	if (i >= (length(PBS.history[[hisname]])-1)) {
		cat("history widget: warning, current position is already at end of history list.\n")
		return()
	}
	PBS.history[[hisname]][[1]]$index <<- i <- i+1
	setWinVal(PBS.history[[hisname]][[i+1]]) #i is always one lower
	.updatePBShistory(hisname)
}


# ***********************************************************
# addPBShistory:
#   need history name
#   and what index to jump to - or what entry to pull it out of
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
jumpPBShistory <- function(hisname="", index="")
{
	if (hisname=="") 
		hisname <- PBS.win$action[1]
	
	if (!is.list(PBS.history)) 
		stop("History not intialized - see initPBShistory")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initPBShistory", sep=""))
	
	if (is.numeric(index))
		i <- index
	else if (index=="")
		i <- as.numeric(getWinVal(PBS.history[[hisname]][[1]]$indexname))
	else
		i <- as.numeric(getWinVal(index))
			
	
	if (i > length(PBS.history[[hisname]])-1 || i <= 0) {
		cat("Error: history index is out of bounds.\n")
		return()
	}
	
	PBS.history[[hisname]][[1]]$index <<- i #update index
	setWinVal(PBS.history[[hisname]][[i+1]]) #i is always one lower
	.updatePBShistory(hisname)	
}


# ***********************************************************
# addPBShistory:
#   save history
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
addPBShistory <- function(hisname="")
{
	if (hisname=="") 
		hisname <- PBS.win$action[1]
	
	if (!is.list(PBS.history)) 
		stop("History not intialized - see initPBShistory")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initPBShistory", sep=""))
	
	
	x <- PBS.history[[hisname]] #old history
	
	insert <- PBS.history[[hisname]][[1]]$index + 1 #make it a real index
	
	#insert to the right of current index
	PBS.history[[hisname]][[insert+1]] <<- getWinVal()
	if (insert<length(x)) {
		for(i in (insert+1):length(x)) {
			PBS.history[[hisname]][[i+1]] <<- x[[i]]
		}
	}
	
	PBS.history[[hisname]][[1]]$index <<- insert
	.updatePBShistory(hisname)
}


# ***********************************************************
# rmPBShistory:
#   if index is numeric - delete history in that spot
#   else delete the history where the current index points to 
#   (and not the value of the current index box - as a user might not have pushed enter)
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
rmPBShistory <- function(hisname="", index="")
{
	if (hisname=="") 
		hisname <- PBS.win$action[1]
	
	if (!is.list(PBS.history)) 
		stop("History not intialized - see initPBShistory")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initPBShistory", sep=""))
	
	if (is.numeric(index))
		i <- index
	else
		i <- PBS.history[[hisname]][[1]]$index
		
	if (length(PBS.history[[hisname]]) == 1) {
		cat("History list is already empty.\n")
		return()
	}
		
	PBS.history[[hisname]] <<- PBS.history[[hisname]][-(i+1)]
	#change index if it was the last element
	if (i > length(PBS.history[[hisname]])-1)
		PBS.history[[hisname]][[1]]$index <<- length(PBS.history[[hisname]])-1 #set index to size
	
	#change values to current index
	i <- PBS.history[[hisname]][[1]]$index
	if (i > 0)
	setWinVal(PBS.history[[hisname]][[i+1]]) #i is always one lower
	
	.updatePBShistory(hisname)
}


# ***********************************************************
# clearPBShistory:
#   remove all history elements from
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
clearPBShistory <- function(hisname="")
{
	if (hisname=="") 
		hisname <- PBS.win$action[1]
	
	if (!is.list(PBS.history)) 
		stop("History not intialized - see initPBShistory")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initPBShistory", sep=""))
	
	len <- length(PBS.history[[hisname]])
	if (len > 1) {
		for(i in 2:len) {
			#PBS.history[[hisname]][[i]] <<- NULL #something weird is happening here
			rmPBShistory(hisname)
		}
	}
	
	.updatePBShistory(hisname)
}


# ***********************************************************
# .updatePBShistory:
#   update widget values
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
.updatePBShistory <- function(hisname)
{
	indexname <- PBS.history[[hisname]][[1]]$indexname
	sizename  <- PBS.history[[hisname]][[1]]$sizename
	x<-list()
	
	if (!is.null(indexname))
		x[[indexname]]<-PBS.history[[hisname]][[1]]$index
	
	if (!is.null(sizename))	
		x[[sizename]]<-length(PBS.history[[hisname]])-1

	setWinVal(x)
}

# ***********************************************************
# initPBShistory:
#   setup the PBShistory "list"
# Arguments:
#   hisname   - history instance name if multiple are active
#   indexname - customized index widget name
#   sizename  - customized size widget name
#   overwrite - retain old history?
# -----------------------------------------------------------
initPBShistory <- function(hisname, indexname=NULL, sizename=NULL, overwrite=TRUE)
{
	
	if (!exists("PBS.history", env = .GlobalEnv))
		PBS.history <- list()
	else
		PBS.history <- get("PBS.history", env = .GlobalEnv)
		
	if (!is.list(PBS.history))
		assign("PBS.history", list(), env = .GlobalEnv)

	
	if (!is.list(PBS.history[[hisname]]) || overwrite) {
		PBS.history[[hisname]] <- list(0)
		PBS.history[[hisname]][[1]] <- list(index=0) #the first element is the index, all other elements are history items
	}
	#save names of entry boxes
	PBS.history[[hisname]][[1]]$indexname <- indexname
	PBS.history[[hisname]][[1]]$sizename <- sizename
	assign("PBS.history", PBS.history, env = .GlobalEnv)
	
}


# ***********************************************************
# exportPBShistory:
#   save PBS history to a file
# Arguments:
#   hisname - history instance name if multiple are active
#   fname   - initial filename to save under
# -----------------------------------------------------------
exportPBShistory <- function(hisname="", fname="")
{
	if (hisname=="") hisname <- PBS.win$action[1]
	
	if (!is.list(PBS.history[[hisname]]))
		stop("unable to export history. Incorect history name given.")
	
	if (fname=="")
		fname <- promptSaveFile(initialfile=paste(hisname,".PBShistory.r", sep=""))
	if (fname=="")
		stop("no filename given.")
	
	writeList(PBS.history[[hisname]], fname)
}


# ***********************************************************
# importPBShistory:
#   import PBS history from a file
# Arguments:
#   hisname - history instance name if multiple are active
#   fname   - initial filename to open from
# -----------------------------------------------------------
importPBShistory <- function(hisname="", fname="")
{
	if (hisname=="") hisname <- PBS.win$action[1]
	
	if (!is.list(PBS.history[[hisname]]))
		stop("unable to import history. Incorect history name given. It must still be intialized.")
	
	if (fname=="")
		fname <- promptOpenFile()
	if (fname=="")
		stop("no filename given.")
	
	newHist <- readList(fname)
	len <- length(newHist)-1
	i <- PBS.history[[hisname]][[1]]$index + 1
	
	for(j in length(PBS.history[[hisname]]):(i+1)) {
		PBS.history[[hisname]][[j+len]] <<- PBS.history[[hisname]][[j]]
	}
	
	for (j in 1:len) {
		PBS.history[[hisname]][[i+j]] <<- newHist[[j+1]]
	}
	

	#update with new history settings
	#setWinVal(PBS.history[[hisname]][[i+1]])
	.updatePBShistory(hisname)
	return(invisible(PBS.history[[hisname]]))
}


# ***********************************************************
# func:
#   get a list of called functions
# Arguments:
#   data - widget lists
# -----------------------------------------------------------
.extractFuns <- function(data)
{
	retData <- c()
	for(i in 1:length(data)) {
		if (!is.null(data[[i]][["widget"]][["function"]]))
			retData <- c(retData, data[[i]][["widget"]][["function"]])
	}
	return(retData)
}


# ***********************************************************
# .extractData:
#   called by TK on button presses (or binds, onchanges, slides, ...)
# Arguments:
#   command - user command to call (ie function argument of widget)
#   action  - action value
#   tclHash - tcl pointer hash
# -----------------------------------------------------------
.extractData <- function(command, action, tclHash)
{
	PBS.win <- .getPBS.win(tclHash, action=action)
	assign("PBS.win", PBS.win, env = .GlobalEnv)

	
	#call the real function
	if (is.null(command))
		return()

	if (command=="")
		return()
		
	if (exists(command,mode="function")) {
		do.call(command, list())
	}
	else {
		cat(paste("Warning: cannot find function '", command, "'.\n", sep=""))
	}
}


# ***********************************************************
# .getPBS.win:
#   generates a new PBS.win list
# Arguments:
#   tclHash - contains tcl pointers
#   action  - what caused this update?
# -----------------------------------------------------------
.getPBS.win <- function(tclHash, action=NULL)
{
	x <- unlist(tclHash$get(".PBS.functions"))
	funs <- names(x$getAll())

	vars <- .extractVar(tclHash)
	win <- list(
	            vars    = vars,
	            funs    = funs,
	            actions = action,
	            windowname = tclHash$get(".PBS.windowname")[[1]]
	       )

	if (exists('PBS.win', envir= .GlobalEnv)) {
		globalWin <- get('PBS.win', envir= .GlobalEnv) #old global PBS.win
		maxaction <- tclHash$get(".PBS.maxaction")
		if (length(globalWin$actions)==maxaction)
			win$actions = c(action, globalWin$actions[-length(globalWin$actions)]) #remove last item
		else
			win$actions = c(action, globalWin$actions)
	}

	return(win)
}


# ***********************************************************
# setWinVal:
#   updates a widget with a new value
# Arguments:
#   vars       - named list or vector specifying new values
#   windowname - which window to update if multiple are active
# -----------------------------------------------------------
setWinVal <- function(vars, windowname="")
{
	if (windowname=="")
		windowname <- PBS.win$windowname
	if (.isReallyNull(.PBS.tclHash, windowname))
		stop(paste("unable to find .PBS.tclHash associated with window:", windowname))
	
	if (!length(vars))
		return(vars)
	
	tclHash <- .PBS.tclHash[[windowname]]
	name <- names(vars)
	for(i in 1:length(vars)) {
		#only update on the way out of the loop
		updatePBSVar <- (i==length(vars))

		if (is.list(vars))
			.setWinValHelper(name[i], vars[[i]], tclHash, updatePBSVar)
		else if (is.vector(vars))
			.setWinValHelper(name[i], vars[i], tclHash, updatePBSVar)
	}
}

.setWinValHelper <- function(varname, value, tclHash, updatePBSVar=TRUE)
{
	nameHash <- unlist(tclHash$get(".PBS.names"))
	x<-tclHash$get(varname)
	if (is.null(x)) {
		x<-nameHash$get(varname)
		if (is.null(x))	
			stop(paste('unable to set "', varname, '": not found.', sep=""))
	}
	
	#special case for matrix
	if (x$widget$type=="matrix") {
		if (length(x$widget$names)==1) {
			if (!is.matrix(value))
				stop(paste('unable to set "', varname, '": supplied value is not a matrix.', sep=""))
			for(i in 1:nrow(value))
				for(j in 1:ncol(value))
					.setWinValHelper(paste(varname,"[",i,",",j,"]",sep=""), value[i,j], tclHash, FALSE)
			return(value)
		}
	}

	#special case for data
	if (x$widget$type=="data") {
		if (length(x$widget$names)==1) {
			#todo: if not a data.frame
			#	stop(paste('unable to set "', varname, '": supplied value is not a dataframe.', sep=""))
			for(i in 1:nrow(value))
				for(j in 1:ncol(value))
					.setWinValHelper(paste(varname,"[",i,",",j,"]d",sep=""), value[i,j], tclHash, FALSE)
			return(value)
		}
	}
	
	#special case for vector
	if (x$widget$type=="vector") {
		if (length(x$widget$names)==1) {
			if (length(value)!=x$widget$length)
				stop(paste('unable to set "', varname, '": supplied vector should have length ', x$widget$length, sep=""))
			for(i in 1:length(value))
				.setWinValHelper(paste(varname,"[",i,"]",sep=""), value[i], tclHash, FALSE)
			return(value)
		}
	}
	
	
	#if tclvar is known, we can set it.
	if (!is.null(x$tclvar)) {

		#value should only be length 1
		if (length(value)!=1)
			stop(paste('unable to set "', varname, '": value given should be of length 1. given length: ',length(value), sep=""))

		if (!is.logical(value))
			value <- as.character(value)
		tclvalue(x$tclvar) <- value
		
		#some widgets must update other widgets(or functions) when they change
		if (!is.null(x$onChange)) {
			if (is.function(x$onChange)) {
				do.call(x$onChange, list())
			}
			if (is.character(x$onChange)) #function names are accepted as strings
				if (exists(x$onChange,mode="function"))
					do.call(x$onChange, list())
		}	
		
		#dont do this if its recursive
		if (updatePBSVar)
			assign("PBS.win", .getPBS.win(tclHash), env = .GlobalEnv)
		return(value)
	}
	else if (!is.null(x$tclwidget)) {
		#special case for text boxes
		if (x$widget$type=="text") {
			if (x$widget$edit==FALSE)
				tkconfigure(x$tclwidget, state="normal")
			
			if (length(value)>1)
				value = paste(value,collapse="\n")
			
			tkdelete(x$tclwidget, "0.0", "end") #clear text widget
			tkinsert(x$tclwidget,"0.0",value) #update
			
			if (x$widget$edit==FALSE)
				tkconfigure(x$tclwidget, state="disabled")
			return(value)
		}
		stop(paste("unhandled widget type", x$tclwidget))
	}
	else {
		stop(paste("unable to update\"", varname, "\" - no tclvar or tclwidget found.", sep=""))
	}

	if (is.null(x$widget))
		stop('widget is not part of the list')
	
	wid <- x$widget

	#custon widgets have more than one sub-widget that needs updating
	if (wid$type=="vector") {
		len <- length(value)
		if (wid$length != len && len != 1)
			stop(paste('value should be a vector with length', wid$length, 'or 1'))
		for (i in 1:wid$length) {
			if (len==1)
				index<-1
			else
				index<-i
			.setWinValHelper(paste(varname, "[", i, "]", sep=""), value[index], updatePBSVar=FALSE)
		}
	}
	else if (wid$type=="matrix") {
		lenExpected <- wid$nrow * wid$ncol
		len <- length(value)
		if (len != lenExpected && len != 1)
			stop(paste('value should be a vector with length', lenExpected, 'or 1'))
		for (i in 1:wid$nrow) {
			for (j in 1:wid$ncol) {
				if (len == 1) {
					index <- 1 #only a single value
				}
				else if (wid$byrow==TRUE)
					index <- (i-1)*wid$ncol + j
				else
					index <- (j-1)*wid$nrow + i
					
				.setWinValHelper(paste(varname, "[", i, ",", j, "]", sep=""), value[index], updatePBSVar=FALSE)
			}
		}
	}
	else if (wid$type=="slideplus") {
		#TODO: slideplus changes - maybe a list(from=,to=,current=)
		stop(paste('unable to change slideplus - still needs to be implemented'))
	}
	else {
		stop(paste('unable to set "', varname, '"', sep=""))
	}
}


# ***********************************************************
# getWinVal:
#   all variables starting with "PBS." will not be returned by default
#   since they should really be hidden by the user in most cases.
# Arguments:
#   v          - values to get
#   scope      - "L" for local, "G" for global, "" for return list only
#   asvector   - if T return a vector, if F, return list
#   windowname - specify a specific window if more than one are in use
# -----------------------------------------------------------
getWinVal <- function(v=NULL, scope="", asvector=FALSE, windowname="")
{
	if (windowname=="")
		windowname <- PBS.win$windowname

	if (.isReallyNull(.PBS.tclHash, windowname))
		stop("supplied window name not found")
	#update PBS.win
	PBS.win <- .getPBS.win(.PBS.tclHash[[windowname]])

	if (is.null(v)) {
		v <- names(PBS.win$vars)
		if (is.null(v))
			return(list()) #no widgets with values found
		v <- v[substr(v,1,4)!="PBS."]
		if (!length(v))	
			return(list()) #no widgets with values found
	}
	if (asvector)
		vals <- vector()
	else
		vals <- list()
	for(i in 1:length(v)) {
		key <- v[i]
		if (asvector)
			vals[key] <- PBS.win$vars[[key]]
		else
			vals[[key]] <- PBS.win$vars[[key]]
		if (scope=="L")
			assign(key,PBS.win$vars[[key]],pos=parent.frame(1))
		else if (scope=="G")
			assign(key, PBS.win$vars[[key]], env = .GlobalEnv)
	}
	return(vals)
}


# ***********************************************************
# clearWinVal:
#   removes any global variables that have a name
#   which corresponds to a name in the window desc file
# -----------------------------------------------------------
clearWinVal <- function() 
{
	objs <- names(PBS.win$vars);
	globs <- ls(all.names=TRUE,pos=".GlobalEnv");
	rmlist <- intersect(objs,globs);
	rm(list=rmlist,pos=".GlobalEnv");
	invisible(rmlist);
};


# ***********************************************************
# .convertMode:
#   converts a variable into a mode without showing any warnings
# Arguments:
#   x    - variable to convert
#   mode - mode to convert to
# -----------------------------------------------------------
.convertMode <- function(x, mode)
{
	if (length(x)>1) {
		ret <- c() #new vector to hold new mode
		for(i in 1:length(x)) {
			ret[i]<-.convertMode(x[i], mode)
		}
		return(ret)
	}

	if (mode=="logical") {
		if (any(grep("^(T|TRUE|F|FALSE)$", x)))
			x <- as.logical(x)
		else
			x <- NA
	}
	else if (mode=="numeric" || mode=="interget") {
		if (any(grep("^(\\-|\\+)?[0-9]*(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?$", x)) && x!="-")
	    	x <- as.numeric(x)
	    else
	    	x <- NA
	}
	else if (mode=="complex") {
		if (x!="-" && any(grep("^(\\-|\\+)?[0-9]*(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?(((\\-|\\+)?[0-9]+|[0-9]*)(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?i)?$", x)))
			x <- as.complex(x)
		else
			x <- NA
	}
	#otherwise it is character and nothing needs converting
	return(x)
}


# ***********************************************************
# .autoConvertMode:
#   converts x into a numeric mode, if it looks like a valid number
# Arguments:
#   x - variable to convert
# -----------------------------------------------------------
.autoConvertMode <- function(x)
{
	#nice regular expression to see if it could be logical
	if (length(grep("^(T|TRUE|F|FALSE)$", x))==length(x)) {
		x <- as.logical(x)
	}
	#ugly regular expression to see if it looks like a numeric
	else if (length(grep("^(\\-|\\+)?[0-9]*(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?$", x))==length(x) 
	    && all(x!="-")) {

    	x <- as.numeric(x)
    }
    #uglier regular expression to see if its complex
    else if (length(grep("^(\\-|\\+)?[0-9]*(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?(((\\-|\\+)?[0-9]+|[0-9]*)(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?i)?$", x))==length(x)
             && all(x!="-")) {

		x <- as.complex(x)
	}	
	return(x)
}