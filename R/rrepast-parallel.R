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
# @importFrom doSNOW registerDoSNOW
#' @importFrom parallel makeCluster
#' @importFrom parallel detectCores
#' 
#' @export
ParallelInit<- function() {
  ## --- Prepare the parallel environment for running 
  v<- makeCluster(getpkgcores()) #, outfile="")
  doSNOW::registerDoSNOW(v)  
  #registerDoParallel(v)
  assign("pkg.runcluster", v, pkg.globals)
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
  stopCluster(get("pkg.runcluster", pkg.globals))
}

#' @title getpkgdefaultcores
#' 
#' @description Provides the package default parallelism level which is 80\% of total cores available
#' 
#' @return Cores used by R/Repast
#'  
#' @export
getpkgdefaultcores<- function() {
  v<- trunc(parallel::detectCores()*0.8)
  ifelse(v < 1, 1, v)
}

#' @title setpkgcores
#' 
#' @description Configures the maximum number of cores to be used in parallel computations
#' 
#' @param v The number of cores
#'  
#' @export
setpkgcores<- function(v) {
  assign("pkg.maxcores", v, pkg.globals)
}

#' @title getpkgcores
#' 
#' @description Returns the maximum number of cores to be used in parallel computations
#' 
#' @return The number of cores
#'  
#' @export
getpkgcores<- function() {
  get("pkg.maxcores", pkg.globals)  
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
#' @param datasource The name of any model aggregate dataset
#' @param maxtime The total simulated time
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
#' @export
ParallelRun<- function(modeldir, datasource, maxtime, r=1, seed=c(), design=NULL, default=NULL) {
  i<- c()
  
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
  run.body<- function(modeldir, datasource, maxtime, ii, r, seed, design, default= NULL) {
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
    data$run<- ii
    ## --- Add replication data
    AddResults(data)
    data
  }
  
  ## --- The test parallel body
          
  ftest<- function(modeldir, datasource, maxtime, ii, r, seed, design, default= NULL) {
    Sys.sleep(10)
    ii
  }
  
  ## --- Progress bar function
  progress<- function(n) { 
    #print(sprintf("PB.update(%d)",n))
    PB.update(n) 
  }
  opts<- list(progress=progress)

  ## --- The parallel loop
  results<- foreach(i=1:r, .combine=rbind, .packages=c('rJava'), .inorder=FALSE, .options.snow=opts) %dopar% {  
    run.body(modeldir, datasource, maxtime, i,r, seed, design, default)
    # (mock test) --- ftest(modeldir, datasource, maxtime, i ,r, seed, design, default)
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
#' @param datasource The name of any model aggregate dataset
#' @param maxtime The total simulated time
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
## @importFrom doParallel registerDoParallel
#' @importFrom utils sessionInfo
#' @export
ParallellRunExperiment<- function(modeldir, datasource, maxtime, r=1, design, FUN, default=NULL) {
  i<- c()
  paramset<- c()
  output<- c()
  dataset<- c()
  
  fpackages= names(sessionInfo()$otherPkgs)
  
  psets<- nrow(design)
  
  e<- Model(modeldir, maxtime, datasource, load=TRUE)
  p<- GetSimulationParameters(e)
  experimental.design<- BuildParameterSet(design, p)
  
  ## --- Progress Bar initialization
  PB.init(psets, r)
  
  ## --- The parallel body
  run.body<- function(modeldir, datasource, maxtime, pset, r, design, FUN, default=NULL) {
    this.design<- design[pset,]
    results<- WrapperRun(modeldir, datasource, maxtime, r, c(), this.design, default, FALSE) 
    cbind(pset, results)
  }

  ## --- Progress bar function
  progress<- function(n) { 
    #print(sprintf("PB.pset(%d)",n))
    PB.pset(n)
    PB.update() 
  }
  opts<- list(progress=progress)
  
  
  ## --- The parallel loop
  dataset<- foreach(i=1:psets, .combine=rbind, .packages=c('rJava'),  .inorder=FALSE, .options.snow=opts) %dopar% { 
    run.body(modeldir, datasource, maxtime, i , r, experimental.design, FUN, default)
  }
  

  body.calibration<- function(pset, dataset) {
    results<- dataset[dataset$pset == pset,]  
    calibration<- FUN(d, results)             # --- Calibration function
    if(is.null(calibration)) {
      stop("Invalid user provided calibration function!")
    }
    cbind(pset,calibration)
  }
  
  output<- foreach(i=1:psets, .combine=rbind, .packages=c(fpackages)) %dopar% { 
    body.calibration(i, dataset)
  }
  
  for(pset in 1:psets) {
    d<- design[pset,]
    paramset<- rbind(paramset,cbind(pset,d))
  }
  
  ## --- Progress Bar clean up
  PB.close()
  return(list(paramset=paramset, output=output, dataset=dataset))
}



#' @title WrapperRun
#'
#' @description Wrapper for the Run and ParallelRun functions
#'
#' @param modeldir The installation directory of some repast model
#' @param datasource The name of any model aggregate dataset
#' @param maxtime The total simulated time
#' @param r The number of experiment replications
#' @param seed The random seed collection
#' @param design The desing matrix holding parameter sampling
#' @param default The alternative values for parameters which should be kept fixed
#' @param multi allows forcing single core execution, default is using multi-core
#'
#' @return The model output dataset
#'
#' @export
WrapperRun<- function(modeldir, datasource, maxtime,  r=1, seed=c(), design=NULL, default=NULL, multi=TRUE) {
  if(!parallelize() || !multi) {
    my.model<- Model(modeldir, maxtime, datasource, load=TRUE)  
    ## --- Update if needed the default parameters
    if(!is.null(default)) {
      UpdateDefaultParameters(my.model, default)  
    }
    v<- Run(my.model, r)
  } else {
    ParallelInit()
    v<- ParallelRun(modeldir, datasource, maxtime, r, seed=c(), design, default)
    ParallelClose()
  } 
  v
}

#' @title WrapperRunExperiment
#'
#' @description Wrapper for the RunExperiment and ParallelRunExperiment 
#' functions
#'
#' @param modeldir The installation directory of some repast model
#' @param datasource The name of any model aggregate dataset
#' @param maxtime The total simulated time
#' @param r The number of experiment replications
#' @param design The desing matrix holding parameter sampling
#' @param FUN The objective function.
#' @param default The alternative values for parameters which should be kept fixed
#'
#' @return The model output dataset
#'
#' @export
WrapperRunExperiment<- function(modeldir, datasource, maxtime, r=1, design, FUN, default=NULL) {
  if(!parallelize()) {
    my.model<- Model(modeldir, maxtime, datasource, load=TRUE)
    ## --- Update if needed the default parameters
    if(!is.null(default)) {
      UpdateDefaultParameters(my.model, default)  
    }
    ## --- Get the model declared paramters
    parameters<- GetSimulationParameters(my.model)
    # Build the experimental parameter set
    experiment<- BuildParameterSet(design, parameters)
    v<- RunExperiment(my.model, r, experiment, FUN)
  } else {
    ParallelInit()
    v<- ParallellRunExperiment(modeldir, datasource, maxtime, r, design, FUN, default)
    ParallelClose()
  }
  v
}
