##================================================================================
## This file is part of the R/Repast package - R/Repast
##
## (C)2016, 2017 Antonio Prestes Garcia <@>
## For license terms see DESCRIPTION and/or LICENSE
##
## @file: rrepast-plots.R
##
## This file contains the plot functions
##================================================================================

#' @title Plot stability of output
#' 
#' @description Generate plot for visually access the stability of 
#' coefficient of variation as function of simulation sample size.
#' 
#' @param obj An instance of Morris Object \code{\link{AoE.Morris}}
#' @param title Chart title, may be null
#' 
#' @return The resulting ggplot2 plot object
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_line ggtitle labs geom_bar geom_errorbar theme element_text
#' @export
Plot.Stability<- function(obj, title= NULL) {
  
  if(is.null(obj$RSD)) {
    stop("Invalid object instance!")
  }
  
  d<- obj
  
  p<- ggplot(d, with(d,aes(sample, RSD))) 
  
  p<- p + labs(y = expression("RSD"))
  p<- p + labs(x = expression("sample size"))
  
  if(!is.null(title)) {
    p<- p + ggtitle(title) +  theme(plot.title = element_text(hjust = 0.5))
  }
  
  p<- p + with(d,aes(shape = group)) 
  p<- p + geom_line( with(d,aes(colour = group)), size = 1)
  
  return(p)
}

#' @title Plot of Morris output
#' 
#' @description Generate plot for Morris's screening method
#' 
#' @param obj An instance of Morris Object \code{\link{AoE.Morris}}
#' @param type The chart type (mu*sigma|musigma|mu*mu)
#' @param title Chart title, may be null
#' 
#' @return The resulting ggplot2 plot object
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_line ggtitle labs geom_bar geom_errorbar scale_shape_manual theme element_text
#' @export
Plot.Morris<- function(obj, type, title= NULL) {
  # --- Check if we received a valid morris object
  if(is.null(obj$call)) {
    stop("Invalid Morris object instance!")
  }
  
  d<- AoE.GetMorrisOutput(obj)
  
  switch(type,
         "mu*sigma" = { 
           p<- ggplot(d, with(d,aes(mu.star, sigma)))  
           p<- p + labs(y = expression(sigma))
           p<- p + labs(x = expression(paste(mu,"*")))
         },
         
         "musigma" = { 
           p<- ggplot(d, with(d,aes(mu, sigma)))
           p<- p + labs(y = expression(sigma))
           p<- p + labs(x = expression(mu))
         },
         
         "mu*mu" = {
           p<- ggplot(d, with(d,aes(mu.star, mu)))
           p<- p + labs(y = expression(mu))
           p<- p + labs(x = expression(paste(mu,"*")))
         },
         
         stop("Invalid chart type!")
  )
  
  if(!is.null(title)) {
    p<- p + ggtitle(title)  + theme(plot.title = element_text(hjust = 0.5))
  }
  
  p<- p + with(d,aes(shape = group)) 
  p<- p + geom_point( with(d,aes(colour = group)), size = 4)
  ##p<- p + geom_point(colour="grey90", size = 1.5) + scale_shape_manual(values=LETTERS[1:nrow(d)])
  p<- p + geom_point(colour="grey90", size = 1.5) + scale_shape_manual(values=c(1:nrow(d)))
  
  return(p)
}

#' @title Plot of Sobol output
#' 
#' @description Generate plot for Sobol's GSA
#' 
#' @param obj An instance of Sobol Object \code{\link{AoE.Sobol}}
#' @param type The chart type 
#' @param title Chart title, may be null
#' 
#' @return The resulting ggplot2 plot object
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_line ggtitle labs geom_bar geom_errorbar theme element_text
#' @export
Plot.Sobol<- function(obj, type, title= NULL) {
  # --- Check if we received a valid sobol object
  if(is.null(obj$S)) {
    stop("Invalid Sobol object instance!")
  }
  
  switch(type,
         # --- First order indices
         "1" = { 
           d<- obj$S
           # --- Add the group column based on rownames
           d$group<- rownames(obj$S)
           
           y.label<- labs(y = expression(S[i]))
         },
         # --- Total order indices
         "2" = { 
           d<- obj$T
           # --- Add the group column based on rownames
           d$group<- rownames(obj$T)
           
           y.label<- labs(y = expression(S[Ti]))
         },
         
         stop("Invalid chart type!")
  )
  
  # --- Create plot object
  p<- ggplot(d, with(d,aes(group,original)))
  p<- p + y.label
  p<- p + labs(x = expression(paste("parameter")))
  
  ## --- p<- p + geom_bar(stat="identity",aes(fill = group))
  p<- p + geom_bar(stat="identity")
  p<- p + geom_errorbar( with(d, aes(ymin=`min. c.i.`, ymax=`max. c.i.`)), colour="black", width=.1)
  
  if(!is.null(title)) {
    p<- p + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }
  
  return(p)
}

#' @title Plot of calibration 
#' 
#' @description Generate plot for parameter sets 
#' providing best fit
#' 
#' @param obj An instance of calibration Object
#' @param key The column name
#' @param title Chart title, may be null
#' 
#' @return The resulting ggplot2 plot object
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_line ggtitle labs geom_bar geom_errorbar
#' @export
Plot.Calibration<- function(obj, key, title= NULL) {
  # --- Check if we received a valid sobol object
  if(is.null(obj$pset)) {
    stop("Invalid calibration object instance")
  }
  
  d<- obj
  d$pset<- factor(d$pset, levels = d$pset)
  
  y.label<- labs(y = expression("Goodness of fit"))
  
  # --- Create plot object
  p<- ggplot(d, with(d,aes(pset,d[,key])))
  p<- p + y.label
  p<- p + labs(x = expression(paste("Parameter set")))
  
  ## --- p<- p + geom_bar(stat="identity",aes(fill = group))
  p<- p + geom_bar(stat="identity")
  
  if(!is.null(title)) {
    p<- p + ggtitle(title)
  }
  
  return(p)
}