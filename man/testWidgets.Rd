\name{testWidgets}
\alias{testWidgets}
\alias{widgets}
\title{Displays Sample GUIs and Source Code}

\description{
  Displays an interactive demo GUI to provide several sample GUIs along with 
  window description source code. It is possible to modify the sample
  source code in the provided text box which can then be recreated with the button below.

  The window description source files can be found in the PBSmodelling/testWidgets directory
  located in the R library.
}

\usage{testWidgets()}

\details{
  The following are the widgets and default values supported by PBS Modelling. 
  See Appendix B for a detailed description.

  \preformatted{
    button text="Calculate" font="" width=0 function="" action="button" 
    sticky="" padx=0 pady=0
  
    check name checked=FALSE text="" font="" function="" action="check" 
    sticky="" padx=0 pady=0
  
    data nrow ncol names modes="numeric" rowlabels="" collabels="" 
    rownames="X" colnames="Y" font="" values="" byrow=TRUE function="" 
    enter=TRUE action="data" width=6 sticky="" padx=0 pady=0
  
    entry name value="" width=20 label="" font="" function="" enter=TRUE 
    action="entry" mode="numeric" sticky="" padx=0 pady=0
  
    grid nrow=1 ncol=1 toptitle="" sidetitle="" topfont="" sidefont="" 
    byrow=TRUE borderwidth=1 relief="flat" sticky="" padx=0 pady=0
  
    history name="default" archive=TRUE sticky="" padx=0 pady=0
  
    label text="" font="" sticky="" padx=0 pady=0
  
    matrix nrow ncol names rowlabels="" collabels="" rownames="" colnames="" 
    font="" values="" byrow=TRUE function="" enter=TRUE action="matrix" 
    mode="numeric" width=6 sticky="" padx=0 pady=0
  
    menu nitems=1 label font=""
  
    menuitem label font="" function action="menuitem"
  
    null padx=0 pady=0
  
    radio name value text="" font="" function="" action="radio" 
    mode="numeric" sticky="" padx=0 pady=0
  
    slide name from=0 to=100 value=NA showvalue=FALSE orientation="horizontal" 
    function="" action="slide" sticky="" padx=0 pady=0
  
    slideplus name from=0 to=1 by=0.01 value=NA function="" enter=FALSE 
    action="slideplus" sticky="" padx=0 pady=0
  
    text name height=8 width=30 edit=FALSE bg="white" mode="character" 
    font="" value="" borderwidth=1 relief="sunken" edit=TRUE padx=0 pady=0
  
    vector names length=0 labels="" values="" font="" vertical=FALSE 
    function="" enter=TRUE action="vector" mode="numeric" width=6 sticky="" 
    padx=0 pady=0 window name="window" title="" vertical=TRUE
  }

}

\seealso{
  \code{\link{createWin}},
  \code{\link{showArgs}}
}

\keyword{utilities}
