##================================================================================
## This file is part of the R/Repast package - R/Repast
##
## (C)2016, 2017 Antonio Prestes Garcia <@>
## For license terms see DESCRIPTION and/or LICENSE
##
## @file: rrepast-aoe.R
##
## This file contains the Analysis of Experiments methods
##================================================================================

#' @title AoE.RMSD
#'
#' @description  A simple Root-Mean-Square Deviation
#' calculation.
#'
#' @param xs The simulated data set
#' @param xe The experimental data set
#'
#' @return The RMSD value for provided datasets
#' @export
AoE.RMSD<- function(xs, xe) {
  return(sqrt(mean((xs - xe)^2, na.rm = TRUE)))
}

#' @title AoE.MAE
#'
#' @description Calculates the average-error
#' magnitude (MAE)
#'
#' @param xs The simulated data set
#' @param xe The experimental data set
#'
#' @return The MAE value for provided datasets
#' @export
AoE.MAE<- function(xs, xe) {
  return(mean((abs(xs - xe)), na.rm = TRUE))
}

#' @title AoE.NRMSD
#'
#' @description  A simple Normalized Root-Mean-Square
#' Deviation calculation using max and min values.
#' NRMSD = RMSD(x) / (max(x) - min(x))
#'
#' @param xs The simulated data set
#' @param xe The experimental data set
#'
#' @return The NRRMSD value for provided datasets
#'
#' @export
AoE.NRMSD<- function(xs, xe) {
  ##divisor<- abs(max(xe,na.rm = TRUE) - min(xe,na.rm = TRUE))
  return(sqrt(mean((xs - xe)^2, na.rm = TRUE))/mean(xe, na.rm = TRUE))
}

#' @title AoE.CoV
#'
#' @description A simple funcion for calculate the
#' Coefficient of Variation
#'
#' @param d The data collection
#' @return The coefficient of variation for data
#'
#' @importFrom stats sd
#'
#' @export
AoE.CoV<- function(d) {
  return((sd(d,na.rm = TRUE)/mean(d,na.rm = TRUE)) * 100)
}

#' @title AoE.ColumnCoV
#'
#' @description This function Calculates the relative squared
#' deviation (RSD or CoV) for an used provided column name \code{key}
#' in the parameter \code{dataset}.
#'
#' @param dataset A model output dataset
#' @param key Column name from output dataset
#'
#' @return A data frame with Coefficient of variations
#'
#' @export
AoE.ColumnCoV<- function(dataset, key) {
  m.run<- dataset$run
  if(is.null(m.run)) {
    stop("The dataset is not an instance of model output!")
  }
  
  result<- c()
  m.max<- max(m.run)
  
  for(i in 1:m.max) {
    m.data<- with(dataset,dataset[run %in% seq(1,i), key])
    result<- rbind(result,cbind(i, AoE.CoV(m.data)))
  }
  result<- as.data.frame(result)
  names(result)<- c("sample","RSD")
  return(result)
}

#' @title AoE.Stability
#'
#' @description This function verifies the stability
#' of CoV for all columns given by parameter \code{keys}
#' or all dataset columns if keys is empty.
#'
#' @param dataset A model output dataset
#' @param keys A list of column names
#'
#' @return A data frame with Coefficient of variations
#'
#' @export
AoE.Stability<- function(dataset, keys=c()) {
  if(length(keys) == 0) {
    keys<- setdiff(names(dataset), c("pset","random_seed","run","Time"))
  }
  
  results<- c()
  for(k in keys) {
    v<- AoE.ColumnCoV(dataset,k)
    v$group<- k
    results<- rbind(results,v)
  }
  return(results)
}

#' @title AoE.Base
#'
#' @description The Design Of Experiments Base function
#'
#' @param m The base design matrix
#' @param factors A subset of model parameters
#' @param fun The function which will be applied to m
#'
#' @return The design matrix
#'
#' @export
AoE.Base<- function(m, factors=c(), fun=NULL) {
  k<- GetFactorsSize(factors)
  if(k == 0) {
    stop("Empty factor collection!")
  }
  
  tmp.factors<- factors
  if(!is.null(fun)) {
    tmp.factors[,"lambda"]<- fun
  }
  
  # --- Apply the desired range
  design<- ApplyFactorRange(m, tmp.factors)
  return(design)
}

#' @title AoE.LatinHypercube
#'
#' @description Generate a LHS sample for model parameters
#'
#' @details Generate the LHS sampling for evaluating
#' the parameters of a model.
#'
#' @param n The number of samples
#' @param factors The model's parameters which will be evaluated
#' @param convert Adjust experiment matrix to parameter scale
#'
#' @return The LHS design matrix for provided parameters
#'
#' @examples \dontrun{
#'  f<- AddFactor(name="cyclePoint",min=40,max=90)
#'  f<- AddFactor(factors=f, name="conjugationCost",min=1,max=80)
#'  d<- DoE.LatinHypercube(2,f)}
#'
#' @importFrom lhs randomLHS
#' @export
AoE.LatinHypercube<- function(n=10, factors=c(), convert= TRUE) {
  k<- GetFactorsSize(factors)
  
  # --- Generate design matrix
  if(convert) {
    design<- AoE.Base(randomLHS(n, k), factors)
  } else {
    design<- randomLHS(n, k)
  }
  design
}

#' @title AoE.FullFactorial design generator
#'
#' @description Generate a Full Factorial sampling for evaluating
#' the parameters of a model.
#'
#' @param n The number of samples
#' @param factors The model's parameters which will be evaluated
#'
#' @return The Full Factorial design matrix for provided parameters
#'
#' @examples \dontrun{
#'  f<- AddFactor(name="cyclePoint",min=40,max=90)
#'  f<- AddFactor(factors=f, name="conjugationCost",min=1,max=80)
#'  d<- AoE.FullFactorial(2,f)}
#'
#' @export
AoE.FullFactorial<- function(n=10, factors=c()) {
  k<- GetFactorsSize(factors)
  
  # --- calculate n for the aproximate number of samples
  #n<- round(n^(1/k))
  n<- ceiling(n^(1/k))
  
  # --- Generate design matrix
  design<- AoE.Base(matrix(nrow = n, ncol = k, seq(1,n)), factors, "SequenceItem")
  design<-  expand.grid(design)
  return(design)
}

#' @title AoE.RandomSampling experiment desing generator
#'
#' @description Generate a Simple Random Sampling experiment design
#' matrix.
#'
#' @param n The number of samples
#' @param factors The model's parameters which will be evaluated
#'
#' @return The random sampling design matrix
#'
#' @examples \dontrun{
#'  f<- AddFactor(name="cyclePoint",min=40,max=90)
#'  f<- AddFactor(factors=f, name="conjugationCost",min=1,max=80)
#'  d<- AoE.RandomSampling(2,f)}
#'
#' @export
AoE.RandomSampling<- function(n=10, factors=c()) {
  k<- GetFactorsSize(factors)
  m<- c()
  for(i in 1:k) {
    m<- cbind(m,runif(n))
  }
  design<- AoE.Base(m, factors)
  return(design)
}

#' @title AoE.Morris
#'
#' @description This is a wrapper for performing Morris's  screening
#' method on repast models. We rely on morris method from sensitivity
#' package.
#'
#' @param k The factors for morris screening.
#' @param p The number of levels for the model's factors.
#' @param r Repetitions. The number of random sampling points of Morris Method.
#'
#' @references Gilles Pujol, Bertrand Iooss, Alexandre Janon with contributions from Sebastien Da Veiga, Jana Fruth,
#' Laurent Gilquin, Joseph Guillaume, Loic Le Gratiet, Paul Lemaitre, Bernardo Ramos and Taieb Touati (2015).
#' sensitivity: Sensitivity Analysis. R package version 1.11.1.
#' https://CRAN.R-project.org/package=sensitivity
#'
#' @importFrom sensitivity morris
#' @export
AoE.Morris<- function(k=c(),p=5,r=4) {
  
  k.v<- GetFactorsSize(k)
  if(k.v == 0) {
    stop("Empty factor collection!")
  }
  
  p.min<- as.numeric(k[,"min"])
  p.max<- as.numeric(k[,"max"])
  p.design<- list(type = "oat", levels = p, grid.jump = ceiling(p/2))
  v<- morris(NULL, k[,"name"], r, p.design, p.min, p.max, scale=TRUE)
  return(v)
}

#' @title AoE.GetMorrisOutput
#'
#' @description  Returns a dataframe holding the Morris
#' result set
#'
#' @param obj A reference to a morris object instance
#'
#' @return The results of Morris method
#'
#' @importFrom stats sd
#' @export
AoE.GetMorrisOutput<- function(obj) {
  mu <- apply(obj$ee, 2, mean)
  mu.star <- apply(obj$ee, 2, function(x) mean(abs(x)))
  sigma <- apply(obj$ee, 2, sd)
  m<- t(rbind(mu,mu.star,sigma))
  tmp<- as.data.frame(m,row.names=seq(1,nrow(m)))
  tmp$group<- rownames(m)
  return(tmp)
}

#' @title AoE.Sobol
#'
#' @description This is a wrapper for performing Global Sensitivity
#' Analysis using the Sobol Method provided by sensitivity
#' package.
#'
#' @details This function is not intended to be used directly from
#' user programs.
#'
#' @references Gilles Pujol, Bertrand Iooss, Alexandre Janon with contributions from Sebastien Da Veiga, Jana Fruth,
#' Laurent Gilquin, Joseph Guillaume, Loic Le Gratiet, Paul Lemaitre, Bernardo Ramos and Taieb Touati (2015).
#' sensitivity: Sensitivity Analysis. R package version 1.11.1.
#' https://CRAN.R-project.org/package=sensitivity
#'
#' @param n The number of samples
#' @param factors The model's parameters which will be evaluated
#' @param o Maximum order in the ANOVA decomposition
#' @param nb Number of bootstrap replicates
#' @param fun.doe The sampling function to be used for sobol method
#' @param fun.sobol The sobol implementation
#'
#'
#' @importFrom sensitivity sobol sobolmartinez sobol2007
#' @export
AoE.Sobol<- function(n=100, factors=c(), o=2, nb=100, fun.doe=AoE.LatinHypercube, fun.sobol=sobolmartinez) {
  p.x1<- fun.doe(n, factors)
  p.x2<- fun.doe(n, factors)
  v<- fun.sobol(model = NULL, X1 = p.x1,X2 = p.x2, order = o, nboot = nb, conf=0.95)
  return(v)
}