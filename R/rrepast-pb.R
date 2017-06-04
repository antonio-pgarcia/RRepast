##================================================================================
## This file is part of the R/Repast package - R/Repast
##
## (C)2016, 2017 Antonio Prestes Garcia <@>
## For license terms see DESCRIPTION and/or LICENSE
##
## @file: rrepast-pb.R
##
## This file contains the progress bar functions
##================================================================================

#' @title PB.set
#'
#' @description  Ses the progress bar descriptor
#'
#' @param obj -- The progress bar descriptor
#'
#' @export
PB.set<- function(obj) {
  assign("pkg.progressbar", obj, pkg.globals)
}

#' @title PB.get
#'
#' @description  Gets the the progress bar descriptor
#'
#' @export
PB.get<- function() {
  return(get("pkg.progressbar", pkg.globals))
}

#' @title PB.enable
#'
#' @description  Enables the progress bar visualization
#'
#' @export
PB.enable<- function() {
  assign("pkg.progressbar.enabled", TRUE, pkg.globals)
}

#' @title PB.disable
#'
#' @description  Disable the progress bar visualization
#'
#' @export
PB.disable<- function() {
  assign("pkg.progressbar.enabled", FALSE, pkg.globals)
}

#' @title PB.isEnabled
#'
#' @description Returns the global value indicating if progress bar
#' is enabled.
#'
#' @return Boolean TRUE if progress bar must be shown
#'
#' @export
PB.isEnabled<- function() {
  return(get("pkg.progressbar.enabled", pkg.globals))
}

#' @title PB.init
#'
#' @description Initialize progress bar for model
#' execution.
#'
#' @param psets -- The total number of paramter sets being simulated
#' @param replications -- The number of replications per simulation round
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
PB.init<- function(psets, replications) {
  ## -- Check if init function has already been called from RunExperiment
  called.from<- function(callstack) {
    result<- FALSE
    v<- grep("(RunExperiment\\s*\\()|(^Run\\s*\\()",callstack)
    if(length(v) == 2) {
      if(v[2] == (v[1] + 1) )  {
        result<- TRUE
      }
    } 
    result
  }
  
  if(called.from(sys.calls())) {
    ##print(grep("(RunExperiment\\s*\\()|(^Run\\s*\\()",sys.calls()))
    return()
  }
  
  ## -- print(grep("(RunExperiment\\s*\\()|(Run\\s*\\()",sys.calls(),value=TRUE))
  ## -- print("PB.init")
  
  if(PB.isEnabled()) {
    total<- psets * replications
    pbar<- txtProgressBar(min = 0, max = total, style = 3)
    pbar$pset<- 1
    pbar$replications<- replications
    PB.set(pbar)
  }
  return(sys.calls())
}

#' @title PB.close
#'
#' @description Close the progress bar descriptor
#'
#' @export
PB.close<- function() {
  ## -- Check if init function has already been called from RunExperiment
  if(length(grep("(RunExperiment\\s*\\()|(Run\\s*\\()",sys.calls())) == 2) {
    return()
  }
  
  if(PB.isEnabled()) {
    pbar<- PB.get()
    if(!is.null(pbar)) {
      close(pbar)
      PB.set(NULL)
    } else {
      stop("Progress bar has not been initialized!")
    }
  }
}

#' @title PB.pset
#'
#' @description Update pset value
#'
#' @param v The current parameter set being simulated
#'
#' @export
PB.pset<- function(v) {
  if(PB.isEnabled()) {
    pbar<- PB.get()
    if(!is.null(pbar)) {
      pbar$pset<- v
      PB.set(pbar)
    }  else {
      stop("Progress bar has not been initialized!")
    }
  }
}

#' @title PB.update
#'
#' @description Update progress bar
#'
#' @param r The current replication number
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
PB.update<- function(r) {
  if(PB.isEnabled()) {
    pbar<- PB.get()
    if(!is.null(pbar)) {
      setTxtProgressBar(pbar, (pbar$pset-1)*pbar$replications + r)
      
    }  else {
      stop("Progress bar has not been initialized!")
    }
  }
}