##================================================================================
## This file is part of the R/Repast package - R/Repast
##
## (C)2016, 2017 Antonio Prestes Garcia <@>
## For license terms see DESCRIPTION and/or LICENSE
##
## @file: rrepast-parallel.R
##
## This file contains the parallelized API 
##================================================================================

#' @title ParallelInit
#' 
#' @description Initialize the parallel execution environment for R/Repast
#' 
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' 
#' @export
ParallelInit<- function() {
  ## --- Prepare the parallel environment for running 
  run.cluster<<- makeCluster((detectCores() - 1))
  registerDoParallel(run.cluster)  
}

#' @title ParallelClose
#' 
#' @description Finalize the parallel execution environment for R/Repast
#' 
#' @importFrom parallel stopCluster
#' 
#' @export
ParallelClose<- function() {
  ## --- Clean up cluster
  stopCluster(run.cluster)
}
  
#' @title ParallelRun
#'
#' @description Run simulations in parallel. This function 
#' executes the time steps of an instantiated model. The number 
#' of replications of model runs can be specified by the 
#' function parameter. The seed parameter may be 
#' omitted and will be generated internally. If provided, 
#' the seed collection, must contain the same
#' number of \code{r} parameter.
#'
#' @param modeldir The installation directory of some repast model
#' @param maxtime The total simulated time
#' @param datasource The name of any model aggregate dataset
#' @param r The number of experiment replications
#' @param seed The random seed collection
#' @param design The desing matrix holding parameter sampling
#' @param default The alternative values for parameters which should be kept fixed
#'
#' @return The model output dataset
#'
#' @examples \dontrun{
#'    md<- "/usr/models/your-model-directory"
#'    output<- ParallelRun(modeldir= md, maxtime = 360, dataset= ds, r=4)} 
#'
#' @importFrom stats runif
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @export
ParallelRun<- function(modeldir, maxtime, datasource, r=1, seed=c(), design=NULL, default=NULL) {
  # The default behaviour is if seed set was
  # not provided generate a suitable set of
  # random seeds for the number of replications.
  if(length(seed) == 0) {
    seed= runif(r,-10^8,10^8)
  } else if(length(seed) != r) {
    stop("The provided set of random numbers doesn't match replications!")
  }
  
  e<- Model(modeldir, maxtime, datasource, load=TRUE)
  # Gets the current set of parameters
  p<- GetSimulationParameters(e)
  
  # Clear result repository
  ClearResults()
  
  SetResultsParameters(p)
  
  ## --- Progress Bar initialization
  PB.init(1, r)
  
  results<- c()
  
  ## --- The parallel body
  run.body<- function(modeldir, maxtime, datasource, i, r, seed, design, default= NULL) {
    e<- Model(modeldir, maxtime, datasource, load=TRUE)
    if(r > 1) {
      ## --- Setting the random seed for experiment replication
      Engine.setParameter(e,getKeyRandom(), as.integer(seed[i]))
    }
    
    ## --- Update the default parameters if needed
    if(!is.null(default)) {
      UpdateDefaultParameters(e, default)  
    }

    # -- Update model parameters using the desing row
    if(!is.null(design)) {
      SetSimulationParameters(e, design)
    }    
    
    
    ## --- Pass the control to Repast to run simulation
    Engine.RunModel(e)
    ## --- Retry output data
    data<- GetOutput(e)
    ## --- Sets the current run number
    data$run<- i
    ## --- Add replication data
    AddResults(data)
    ## --- Update progress bar
    PB.update(i)
    data
  }
  
  ## --- The test parallel body
  ftest<- function(modeldir, maxtime, datasource, i, r, seed) {
    Sys.sleep(10)
    i
  }
  
  ## --- The parallel loop
  results<- foreach(i=1:r, .combine=rbind, .packages=c('rJava')) %dopar% {  
    run.body(modeldir, maxtime, datasource, i ,r, seed, design, default)
  }

  ## --- Progress Bar clean up
  PB.close()
  
  return(results)
}



#' @title ParallellRunExperiment
#'
#' @description Run the model multiple times for different parameters
#' given by design matrix function parameter.
#'
#' @details The FUN function must return zero for perfect fit and values
#' greater than zero otherwise.
#'
#' @param modeldir The installation directory of some repast model
#' @param maxtime The total simulated time
#' @param datasource The name of any model aggregate dataset
#' @param r The number of experiment replications
#' @param design The desing matrix holding parameter sampling
#' @param FUN THe calibration function.
#' @param default The alternative values for parameters which should be kept fixed
#'
#' @examples \dontrun{
#'    my.cost<- function(params, results) { # your best fit calculation, being 0 the best metric.  }
#'    d<- "/usr/models/your-model-directory"
#'    f<- AddFactor(name="cyclePoint",min=40,max=90)
#'    f<- AddFactor(factors=f, name="conjugationCost",min=1,max=80)
#'    d<- AoE.LatinHypercube(factors=f)
#'    v<- ParallellRunExperiment()}
#'
#' @return A list with output and dataset
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @export
ParallellRunExperiment<- function(modeldir, maxtime, datasource, r=1, design, FUN, default=NULL) {
  paramset<- c()
  output<- c()
  dataset<- c()
  
  psets<- nrow(design)
  
  e<- Model(modeldir, maxtime, datasource, load=TRUE)
  p<- GetSimulationParameters(e)
  design<- BuildParameterSet(design, p)
  
  ## --- Progress Bar initialization
  PB.init(psets, r)
  
  ## --- The parallel body
  run.body<- function(modeldir, maxtime, datasource, pset, r, design, FUN, default=NULL) {
    PB.pset(pset) # -- Update progress bar pset value
    this.design<- design[pset,]
    results<- ParallelRun(modeldir, maxtime, datasource, r, c(), this.design, default) 
    cbind(pset, results)
  }
  
  ## --- The parallel loop
  dataset<- foreach(i=1:psets, .combine=rbind, .packages=c('rJava')) %dopar% {  
    run.body(modeldir, maxtime, datasource, i , r, design, FUN, default)
  }
  
  for(pset in 1:psets) {
    d<- design[pset,]
    results<- dataset[dataset$pset == pset,]
    calibration<- FUN(d, results) # --- Calibration function
    
    if(is.null(calibration)) {
      stop("Invalid user provided calibration function!")
    }

    paramset<- rbind(paramset,cbind(pset,d))
    output<- rbind(output,cbind(pset,calibration))

  }
  
  ## --- Progress Bar clean up
  PB.close()
  
  return(list(paramset=paramset, output=output, dataset=dataset))
}
