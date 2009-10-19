#taken from R Curl - merge x and y, if elements match in both x and y, then elements from y are stored (overwriting x's elements)
.mergeLists <- function( x, y ) 
{
    if (length(x) == 0) 
        return(y)
    if (length(y) == 0) 
        return(x)
    i = match(names(y), names(x))
    i = is.na(i)
    if (any(i)) 
        x[names(y)[which(i)]] = y[which(i)]
    x
}

setClass ("PBSoptions", representation( instance = "list" ) )

setMethod( f="initialize", signature="PBSoptions",
definition=function(.Object, filename, initial.options = list(), gui.prefix = "option" )
{
	if( missing( filename ) ) stop( "class initializer requires a filename" )
	if( is.character( filename ) == FALSE ) stop( "filename must be a character vector (string)" )
	if( is.list( initial.options ) == FALSE ) stop( "initial.options must be a list" )
	if( is.character( gui.prefix ) == FALSE ) stop( "GUI prefix must be a character vector (string)" )

	options <- initial.options

	#create functions within this namespace - so that each time a new instance of this class is created
	#these functions will point to a new set of variables (i.e. just how non-static variables of a c++ class work)
	load <- function( fname, prompt = FALSE )
	{
		old_filename <- filename
		if( missing( fname ) == FALSE )
			setFileName( fname )
		if( prompt == TRUE ) {
			f <- selectFile( initialfile=filename, mode="open" )
			if( is.null( f ) == TRUE ) {
				setFileName( old_filename ) #restore old filename if user cancels
				return()
			}
			setFileName( f )
		}
		if( file.exists( filename ) == FALSE )
			return()
		tmp <- readList( filename )
		options <<- .mergeLists( options, tmp )
	}

	setFileName <- function( name )
	{
		filename <<- name
	}

	getFileName <- function()
	{
		return( filename )
	}

	saveAs <- function() #prompts user to select a file name
	{
		selected <- selectFile( mode="save")
		if( length( selected ) == 0 || selected == "" )
			return()

		#TODO alert if file already exists - do we really want to overwrite?

		filename <<- selected
		save()
	}
	
	#silent save
	save <- function( fname, prompt = FALSE )
	{
		old_filename <- filename
		if( missing( fname ) == FALSE )
			setFileName( fname )
		if( prompt == TRUE ) {
			f <- selectFile( initialfile=filename, mode="save" )
			if( is.null( f ) == TRUE ) {
				setFileName( old_filename ) #restore old filename if user cancels
				return()
			}
			setFileName( f )
		}
		writeList( options, fname = filename )
	}

	#get( some_key ) or get() for all key/values
	get <- function( key )
	{
		if( missing( key ) )
			return( options )
		return( options[[ key ]] )
	}

	#set( key = value )
	set <- function( ... )
	{
		v <- list( ... )
		if( length( v ) == 0 ) return()
		if( is.null( names( v ) ) || any( names( v ) == "" ) )
			stop( "values must be named" )

		for( i in 1:length( v ) ) {
			options[ names(v)[ i ] ] <<- list( v[[ i ]] )
		}
		
	}

	#save GUI values to this option list (R)
	saveGUI <- function()
	{
		values <- getWinVal()
		widgets <- names( values )
		opts <- paste( gui.prefix, names( get() ), sep="" )
		m <- match( widgets, opts )
		to_update = widgets[ !is.na( m ) ] #names of widgets which correspond to a value

		for( w in to_update ) {
			k <- substring( w, nchar(gui.prefix)+1 ) #remove prefix to get option key
			options[[ k ]] <<- values[[ w ]]
		}
			
	}

	#load GUI values from this option list (R)
	loadGUI <- function()
	{
		values <- getWinVal()
		widgets <- names( values )
		opts <- paste( gui.prefix, names( get() ), sep="" )
		m <- match( widgets, opts )
		to_update = widgets[ !is.na( m ) ] #names of widgets which correspond to a value

		opts <- list()
		for( w in to_update ) {
			k <- substring( w, nchar(gui.prefix)+1 ) #remove prefix to get option key
			opts[[ w ]] <- get( k )
		}
		setWinVal( opts )
	}

	#get the gui prefix
	getPrefix <- function() { return( gui.prefix ) }

	#set the gui prefix
	setPrefix <- function( prefix ) { gui.prefix <<- prefix }

	#load all functions into the instance list to return
	instance <- list()
	items <- ls() 
	for( i in items ) {
		v <- base::get( i )
		if( is.function( v ) == FALSE ) next
		instance[[ i ]] <- v
	}

	load()
	.Object@instance <- instance
	return( .Object )
}
)

.showOptions <- function( object )
{
	cat( "filename:", object@instance$getFileName(), "\n" )
	cat( "gui.prefix:", object@instance$getPrefix(), "\n" )
	opts <- object@instance$get()
	if( length( opts ) == 0 )
		cat( "Options: None\n" )
	else {
		cat( "Options:\n" )
		str( opts, no.list=T)
	}
}

setMethod( "print", signature="PBSoptions",
definition=function( x, ... )
{
	.showOptions( x )
}
)

setMethod( "show", signature="PBSoptions",
definition=function( object )
{
	.showOptions( object )
}
)


getOptions <-function( option.object, key )
{
	if( is( option.object, "PBSoptions" ) == FALSE ) stop( "option.object must be a pbsmodelling option class" )
	option.object@instance$get( key )
}
getOptionsFileName <-function( option.object )
{
	if( is( option.object, "PBSoptions" ) == FALSE ) stop( "option.object must be a pbsmodelling option class" )
	option.object@instance$getFileName()
}
getOptionsPrefix <-function( option.object )
{
	if( is( option.object, "PBSoptions" ) == FALSE ) stop( "option.object must be a pbsmodelling option class" )
	option.object@instance$getPrefix()
}
loadOptions <-function( option.object, fname, prompt = FALSE )
{
	if( is( option.object, "PBSoptions" ) == FALSE ) stop( "option.object must be a pbsmodelling option class" )
	option.object@instance$load( fname, prompt )
}
loadOptionsGUI <-function( option.object )
{
	if( is( option.object, "PBSoptions" ) == FALSE ) stop( "option.object must be a pbsmodelling option class" )
	option.object@instance$loadGUI()
}
saveOptions <-function( option.object, fname, prompt = FALSE )
{
	if( is( option.object, "PBSoptions" ) == FALSE ) stop( "option.object must be a pbsmodelling option class" )
	option.object@instance$save( fname, prompt )
}
saveOptionsGUI <-function( option.object )
{
	if( is( option.object, "PBSoptions" ) == FALSE ) stop( "option.object must be a pbsmodelling option class" )
	option.object@instance$saveGUI()
}
setOptions <-function( option.object, ... )
{
	if( is( option.object, "PBSoptions" ) == FALSE ) stop( "option.object must be a pbsmodelling option class" )
	option.object@instance$set( ... )
}
setOptionsFileName <-function( option.object, name )
{
	if( is( option.object, "PBSoptions" ) == FALSE ) stop( "option.object must be a pbsmodelling option class" )
	option.object@instance$setFileName( name )
}
setOptionsPrefix <-function( option.object, prefix )
{
	if( is( option.object, "PBSoptions" ) == FALSE ) stop( "option.object must be a pbsmodelling option class" )
	option.object@instance$setPrefix( prefix )
}

