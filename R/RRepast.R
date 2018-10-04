##================================================================================
## This file is part of the rrepast package - R/Repast interface API
##
## (C)2015 Antonio Prestes Garcia <@>
## For license terms see DESCRIPTION and/or LICENSE
##
## $Id$
##================================================================================


# ------------------------------------------------------------
# .onLoad, Hook for loading package namespace
#
# ------------------------------------------------------------
.onLoad<- function(libname, pkgname) {
  #packageStartupMessage("R/Repast: Integrating Repast Models into R\n")
  assign("pkg.globals", new.env(), envir=parent.env(environment()))

  # Internal variables
  assign("pkg.basedir", NA, pkg.globals)
  assign("pkg.modeldir", NA, pkg.globals)
  assign("pkg.scenariodir", NA, pkg.globals)
  assign("pkg.modellibdir", NA, pkg.globals)
  assign("pkg.id", NA, pkg.globals)

  # global simulation results
  assign("pkg.parameters", data.frame(), pkg.globals)
  assign("pkg.results", data.frame(), pkg.globals)

  # progress bar internals
  assign("pkg.progressbar", NULL, pkg.globals)
  assign("pkg.progressbar.enabled", FALSE, pkg.globals)
  
  # stats
  assign("pkg.stats.calls", 0, pkg.globals)
  
  # parallelize
  assign("pkg.parallelize", FALSE, pkg.globals)
  assign("pkg.runcluster", NULL, pkg.globals)
  assign("pkg.maxcores", getpkgdefaultcores(), pkg.globals)

  # default values for model
  assign("pkg.outputdir",paste0(Sys.getenv("TMP"),"/rrepast-deployment/"), pkg.globals)
  assign("pkg.repastlibdir", "/repast.simphony/", pkg.globals)
  assign("pkg.java.parameters","-server -Xms512m -Xmx1024m", pkg.globals)

  # default key for Repast random seed
  assign("pkg.randomSeed","randomSeed", pkg.globals)

  # The Random Seed. You may want to change this.
  set.seed(exp(1)*10^6)

  # Define funcions which are not present in ond R versions
  compatibility()
}


#' @title The easy API for model initilization
#'
#' @description Instantiate a repast model from the model dir without
#' loading the scenario file.
#'
#' @details This is the entry point for model execution. Typically
#' any model execution will start with this function which encapsulates
#' all low level calls for model initialization. In order to perform
#' simulations with repast from R code only \code{Model} and a
#' few more function calls are required: \code{\link{Load}},
#' \code{\link{Run}}. Finally the output of model is managed with
#' functions \code{\link{GetResults}} and \code{\link{SaveSimulationData}}.
#'
#' @param modeldir The installation directory of some repast model
#' @param maxtime The total simulated time
#' @param dataset The name of any model aggregate dataset
#' @param load If true instantiate model and load scenario
#'
#' @return Returns the instance of repast model
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Model(d)}
#'
#' @references
#' [1] North, M.J., N.T. Collier, and J.R. Vos, "Experiences Creating Three Implementations of the Repast Agent Modeling Toolkit," ACM Transactions
#' on Modeling and Computer Simulation, Vol. 16, Issue 1, pp. 1-25, ACM,
#' New York, New York, USA (January 2006).
#' @export
Model<- function(modeldir="",maxtime=300,dataset="none", load=FALSE) {
  if(dir.exists(modeldir)) {
    # Configure all required directory based on their default locations
    configModelDirs(modeldir)

    # Initilialized JVM
    # Setup classpath inferring default values from modeldir
    jvm.init()

    # Creating an engine instance
    e<- Engine()

    # Configure the total amount of time to be simulated
    Engine.endAt(e,maxtime)

    # Configure the dataset
    Engine.SetAggregateDataSet(e,dataset)

    if(load == TRUE) {
      Load(e)
    }

    return(e)
  } else {
    stop(paste0("The model directory does not exist: ", modeldir))
  }
}

#' @title The Scenario loader
#'
#' @description Loads the model's scenario. This function must be
#' called before running the model.
#'
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Model(d)
#'    Load(m)}
#'
#' @param e An engine object instance
#'
#' @export
Load<- function(e) {
  Engine.LoadModel(e,getScenarioDir())
  setId(Engine.getId(e))
}

#' @title Run simulations
#'
#' @description This function executes the time steps of an
#' instantiated model. The number of replications of model
#' runs can be specified by the function parameter. The seed
#' parameter may be omitted and will be generated internally.
#' If provided, the seed collection, must contain the same
#' number of \code{r} parameter.
#'
#' @param e An engine object instance
#' @param r The number of experiment replications
#' @param seed The random seed collection
#'
#' @return The model output dataset
#'
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Model(d)
#'    Load(m)
#'    Run(m,r=2) # or Run(m,r=2,seed=c(1,2))}
#'
#' @importFrom stats runif
#' @export
Run<- function(e,r=1,seed=c()) {
  # The default behaviour is if seed set was
  # not provided generate a suitable set of
  # random seeds for the number of replications.
  if(length(seed) == 0) {
    seed= runif(r,-10^8,10^8)
  } else if(length(seed) != r) {
    stop("The provided set of random numbers doesn't match replications!")
  }

  # Gets the current set of parameters
  p<- GetSimulationParameters(e)

  # Clear result repository
  ClearResults()

  SetResultsParameters(p)

  ## --- Progress Bar initialization
  PB.init(1, r)

  results<- c()

  for(i in 1:r) {

    if(r > 1) {
      ## --- Setting the random seed for experiment replication
      Engine.setParameter(e,getKeyRandom(),as.integer(seed[i]))
    }

    ## --- Pass the control to Repast to run simulation
    Engine.RunModel(e)

    data<- GetOutput(e)

    ## --- Just for debug: print(paste0("run= ", i, " / rows=", nrow(data)))

    # Sets the current run number
    data$run<- i

    # Add replication data
    AddResults(data)

    results<- rbind(results,data)

    # Update progress bar
    PB.update(i)

    ##Engine.resetModelOutput(e)
  }
  ## --- Progress Bar clean up
  PB.close()

  return(results)
}

#' @title Run an experimental setup
#'
#' @description Run the model multiple times for different parameters
#' given by design matrix function parameter.
#'
#' @details The FUN function must return zero for perfect fit and values
#' greater than zero otherwise.
#'
#' @param e An engine object instance
#' @param r The number of experiment replications
#' @param design The desing matrix holding parameter sampling
#' @param FUN THe calibration function.
#'
#' @examples \dontrun{
#'    my.cost<- function(params, results) { # your best fit calculation, being 0 the best metric.  }
#'    d<- "c:/usr/models/your-model-directory"
#'    m<- Model(d,dataset="ds::Output")
#'    Load(m)
#'    f<- AddFactor(name="cyclePoint",min=40,max=90)
#'    f<- AddFactor(factors=f, name="conjugationCost",min=1,max=80)
#'    d<- LatinHypercube(factors=f)
#'    p<- GetSimulationParameters(e)
#'    exp.design<- BuildParameterSet(d,p)
#'    v<- RunExperiment(e,r=1,exp.design,my.cost) }
#'
#' @return A list with output and dataset
#'
#' @export
RunExperiment<- function(e, r=1, design, FUN) {
  paramset<- c()
  output<- c()
  dataset<- c()

  psets<- nrow(design)

  ## --- Progress Bar initialization
  PB.init(psets, r)

  for(i in 1:psets) {
    d<- design[i,]

    # -- Set parameters for next model 'Run'
    SetSimulationParameters(e, d)

    # -- Update progress bar pset value
    PB.pset(i)

    # -- Run model with current parameter set
    results<- Run(e,r)

    ## ----- [2017/04/01] Possible bug
    ##results<- GetResults()

    # -- The user provided calibration function.
    # -- Calibration function must return 0 for perfect fit between
    # -- observed and experimental data.
    calibration<- FUN(d,results)

    if(is.null(calibration)) {
      stop("Invalid user provided calibration function!")
    }

    # -- The 'pset' is the paramter set id
    pset<- i

    paramset<- rbind(paramset,cbind(pset,d))
    output<- rbind(output,cbind(pset,calibration))
    dataset<- rbind(dataset,cbind(pset,results))
  }

  ## --- Progress Bar clean up
  PB.close()

  return(list(paramset=paramset, output=output, dataset=dataset))
}

#' @title Helper function to get experiment \code{paramset}
#'
#' @description The RunExperiment function returns a list holding
#' the \code{paramset}, \code{output} and \code{dataset} collection.
#' The \code{paramset} collection contains the parameters used for
#' running the experimental setup. The \code{output} has the results
#' from user provided calibration function. The \code{dataset}
#' collection has the raw output of 'Repast' aggregated dataset.
#'
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Model(d)
#'    ...
#'    e<- RunExperiment(e,r=1,exp.design,my.cost)
#'    p<- getExperimentParamSet(e)}
#'
#' @param e The experiement object returned by \code{\link{RunExperiment}}
#'
#' @return The reference to \code{output} container.
#' @export
getExperimentParamSet<- function(e) {
  v<- e$paramset
  return(v)
}

#' @title Helper function to get experiment \code{output}
#'
#' @description The RunExperiment function returns a list holding
#' the \code{paramset}, \code{output} and \code{dataset} collection.
#' The \code{paramset} collection contains the parameters used for
#' running the experimental setup. The \code{output} has the results
#' from user provided calibration function. The \code{dataset}
#' collection has the raw output of 'Repast' aggregated dataset.
#'
#' @param e The experiement object returned by \code{\link{RunExperiment}}
#'
#' @return The reference to \code{output} container.
#' @export
getExperimentOutput<- function(e) {
  v<- e$output
  return(v)
}

#' @title Helper function to get experiment \code{dataset}
#'
#' @description The RunExperiment function returns a list holding
#' the \code{paramset}, \code{output} and \code{dataset} collection.
#' The \code{paramset} collection contains the parameters used for
#' running the experimental setup. The \code{output} has the results
#' from user provided calibration function. The \code{dataset}
#' collection has the raw output of 'Repast' aggregated dataset.
#'
#' @param e The experiement object returned by \code{\link{RunExperiment}}
#'
#' @return The reference to \code{dataset} container.
#' @export
getExperimentDataset<- function(e) {
  v<- e$dataset
  return(v)
}

#' @title Gets the output
#'
#' @description  Returns the results of a model a data.frame from the last
#' RUN. Should be used only if model replication is equal to 1,
#' otherwise GetResults must be used.
#'
#' @param e An engine object instance
#'
#' @return Returns a data.frame with output data
#'
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Model(d)
#'    ...
#'    data<- GetOutput(m)}
#'
#' @importFrom utils read.csv
#' @export
GetOutput<- function(e) {
  c<- textConnection(Engine.GetModelOutput(e))
  read.csv(c)
}

#' @title GetSimulationParameterType
#'
#' @description Returns the declared parameter type.
#'
#' @param e An instance of 'Engine' object
#' @param k The parameter name
#'
#' @return The parameter type as string
#' @export
GetSimulationParameterType<- function(e, k) {
  Engine.getParameterType(e, k)
}

#' @title UpdateDefaultParameters
#'
#' @description Modify the value of the default parameters
#' which should be kept fixed
#'
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Model(d)
#'    Load(m)
#'
#'    p<- c(name1=value1, name2=2)
#'    UpdateDefaultParameters(m,p)}
#'
#' @param e An engine object instance
#' @param p The collection of model fixed paramters to change
#'
#' @export
UpdateDefaultParameters<- function(e, p) {
  if(is.null(e)) {
    stop("Engine object is null!")
  }

  parameters<- GetSimulationParameters(e)
  for(key in names(p)) {
    if(key %in% names(parameters)) {
      value<- sprintf("%s",p[key])
      SetSimulationParameter(e, key, value)
    } else {
      print(sprintf("The model does not have a paramter with name [ %s ]",key))
    }
  }
}

#' @title Set parameters for running model
#'
#' @description Modify the repast model parameters with
#' values provided in parameter 'p' which is a data frame
#' with just one row.
#'
#' @param e An engine object instance
#' @param p A data frame with simulation parameters
#'
#' @export
SetSimulationParameters<- function(e, p) {
  if(is.null(e)) {
    stop("Engine object is null!")
  }

  for(key in names(p)) {
    value<- p[1,key]
    if(is.factor(value)) {
      value<- levels(value)
    }

    ## Modify the default set of parameters
    SetSimulationParameter(e, key, value)
  }
}

#' @title SetSimulationParameter
#'
#' @description Modify model's default parameter collection
#'
#' @param e An engine object instance
#' @param key The paramter name
#' @param value The parameter value
#'
#' @export
SetSimulationParameter<- function(e, key, value) {
  if(is.null(e)) {
    stop("Engine object is null!")
  }

  keys<- names(GetSimulationParameters(e))

  # Verify that "key" is a valid model parameter
  if(key %in% keys) {
  # Try to coerce the value to a type for safety
    switch(typeof(value),
      double = {
        #print(paste0("double", key,"<- ",value))
        value<- as.double(value)
      },

      integer = {
        #print(paste0("integer", key,"<- ",value))
        value<- as.integer(value)
      },

      character = {
         #print(paste0("character", key,"<- ",value))
         value<- as.character(value)
       })
    Engine.setParameter(e,key,value)
  }
}

#' @title Gets the simulation parameters
#'
#' @description Returns a dataframe with the current set of input
#' parameters for the last model run.
#'
#' @param e An engine object instance
#'
#' @return A data frame with simulation parameters
#'
#' @export
GetSimulationParameters<- function(e) {
  keys<- ""
  values<- ""
  names<- Engine.getParameterNames(e)
  for(n in names) {
    v<- Engine.getParameterAsString(e,n)
    if(nchar(keys) == 0){
      keys<- n
      values<- v
    } else {
      keys<- paste0(keys,",",n)
      values<- paste0(values,",",v)
    }
  }
  b<- rbind(keys,values)
  c<- textConnection(b)
  read.csv(c)
}

#' @title Clear the results data.frame
#'
#' @description This function is called automatically every
#' time Run method is called.
#'
#' @export
ClearResults<- function() {
  assign("pkg.results", data.frame(), pkg.globals)
  assign("pkg.parameters", data.frame(), pkg.globals)
}

#' Returns the model results
#'
#' @export
GetResults<- function() {
  return(get("pkg.results", pkg.globals))
}

#' Stores a data.frame
#'
#' @param d A data frame containing one replication data
#'
#' @export
SetResults<- function(d) {
  assign("pkg.results", d, pkg.globals)
}

#' @title Concatenate results of multiple runs
#'
#' @description This function stores the output
#' of the last model execution and it is intended
#' to be used internally.
#'
#' @param d A data frame containing one replication data
#'
#' @export
AddResults<- function(d) {
  r<- GetResults()
  SetResults(rbind(r,d))
}

#' @title Gets the parameters
#'
#' @description Returns the current set of paramters used
#' for the last model run.
#'
#' @return A data.frame with parameters of the model.
#'
#' @export
GetResultsParameters<- function() {
  return(get("pkg.parameters", pkg.globals))
}

#' @title Sets the parameters
#'
#' @description Save the current set of paramters used
#' for the last model run.
#'
#' @param d A data.frame with parameter values
#'
#' @export
SetResultsParameters<- function(d) {
  assign("pkg.parameters", d, pkg.globals)
}

#' @title Saving simulation output
#'
#' @description Saves the simulation results of last call to Run(e)
#' function.
#'
#' @details The model must have been initialized or user must call
#' \code{setId} explicitelly.
#'
#' @param as The desired output type, must be csv or xls
#' @param experiment The experiment output
#'
#' @return The id of saved data
#'
## @importFrom xlsx write.xlsx
#' @importFrom digest digest
#' @importFrom utils write.csv
#' @export
SaveSimulationData<- function(as="csv", experiment=NULL) {
  # Creating output dir if needed
  createOutputDir()
  filename<- getId()
  if(is.na(filename)) {
    stop("Model was not initialized correctly!")
  }

  paramset<- NULL
  output<- NULL
  dataset<- NULL

  if(!is.null(experiment)) {
    paramset<- getExperimentParamSet(experiment)
    output<- getExperimentOutput(experiment)
    dataset<- getExperimentDataset(experiment)

  } else {
    # The parameters of current simultation output
    paramset<- GetResultsParameters()

    # The results of simulation run
    dataset<- GetResults()
  }



  hash <- digest(Sys.time(), algo="crc32")
  f0<- paste0(getOutputDir(),tolower(filename),"-paramset-",hash)
  f1<- paste0(getOutputDir(),tolower(filename),"-output-",hash)
  f2<- paste0(getOutputDir(),tolower(filename),"-dataset-",hash)

  switch(as,
         csv = {
           f0<- paste0(f0,".csv")
           f1<- paste0(f1,".csv")
           f2<- paste0(f2,".csv")
           write.csv(paramset, f0, row.names=FALSE)
           if(!is.null(output)) {
             write.csv(output, f1, row.names=FALSE)
           }
           write.csv(dataset, f2, row.names=FALSE)
         },
         xls = {
           f0<- paste0(f0,".xlsx")
           f1<- paste0(f1,".xlsx")
           f2<- paste0(f2,".xlsx")

           xlsx::write.xlsx(paramset, f0)
           if(!is.null(output)) {
             xlsx::write.xlsx(output, f1)
           }
           xlsx::write.xlsx(dataset, f0)
         })
  return(hash)
}

#' @title Adds a paramter to factor collection
#'
#' @description Builds up the factor collection.
#'
#' @param factors The current factor collection
#' @param lambda The function to apply FUN(p,min,max)
#' @param name The name of factor
#' @param min The minimun of parameter p
#' @param max The maximun of parameter p
#' @param int Boolean for truncating the factor value
#'
#' @examples \dontrun{
#'    f<- AddFactor(name="Age",min=20,max=60)
#'    f<- AddFactor(factors=f, name="Weight",min=50,max=120)}
#'
#' @return The collection of created factors
#'
#' @export
AddFactor<- function(factors=c(), lambda="qunif",name, min, max, int=FALSE) {
  if(max < min) {
    stop("Invalid factor range!")
  }

  # if parameter already existe replace the current value
  rrow<- c(lambda=lambda,name=name,min=min,max=max, int=int)
  rownames(rrow)<- NULL
  if(length(factors) > 0 && any(factors[,"name"] == name)) {
    i<- which(factors[,"name"] == name)
    factors[i,]<- c(rrow)
  } else {
    factors<- rbind(factors,c(rrow))
  }
  return(factors)
}

#' @title AddFactor0
#'
#' @description Creates or appends the factor collection
#'
#' @param factors The current factor collection
#' @param ... The variadic parameter list
#'
#' @examples \dontrun{
#'    f<- AddFactor0(name="Age",min=20,max=60)
#'    f<- AddFactor0(factors=f, name="Weight",min=50,max=120)}
#'
#' @return The factor collection
#'
#' @export
AddFactor0<- function(factors=c(), ...) {
  argv<- list(...)
  
  ## Default value 
  v.lambda<- ifelse(is.null(lget(argv,"lambda")), "qunif", lget(argv,"lambda"))
  v.int<- ifelse(is.null(lget(argv,"int")), FALSE, lget(argv,"int"))
  
  ## Inner auxiliary functions
  is.range<- function(v) {
    (lcontains(v,"name") && lcontains(v,"min") && lcontains(v,"max") && (!lcontains(v,"levels")))    
  }
  is.levels<- function(v) {
    (lcontains(v,"name") && lcontains(v,"levels") && !(lcontains(v,"min") || lcontains(v,"max")))    
  }
  
  
    
  ## Getting the values 
  if(is.range(argv)) {
    name<- lget(argv,"name")  
    v.min<- lget(argv,"min")
    v.max<- lget(argv,"max")  
    if(v.max < v.min) {
      stop("Invalid factor range!")
    }
    
    rrow<- c(lambda=v.lambda,name=name,min=v.min,max=v.max, int=v.int)
  } else {
    if (is.levels(argv)) {
      name<- lget(argv,"name")  
      v.levels<- lget(argv,"levels")
      
      rrow<- c(lambda=v.lambda,name=name,levels=as.list(v.levels))
    } else {
      stop("Invalid parameter combination!")
    }
  }
  
  # Add or replace the value
  rownames(rrow)<- NULL
  if(length(factors) > 0 && any(factors[,"name"] == name)) {
    i<- which(factors[,"name"] == name)
    factors[i,]<- c(rrow)
  } else {
    factors<- rbind(factors,c(rrow))
  }
  
  factors
}

#' @title GetFactorLevels
#'
#' @description Returns the fator's levels
#'
#' @param factors The current factor collection
#' @param name The factor name
#'
#' @examples \dontrun{
#'    f<- AddFactor0(name="Age",levels=c(25,30,40,65))
#'    f<- AddFactor0(factors=f, name="Weight",levels=c(60,70,80,90))
#'    
#'    GetFactorLevels(factors=f, "Age")}
#'
#' @return Levels
#'
#' @export
GetFactorLevels<- function(factors, name) {
  mylevels<- c()
  if(length(factors) > 0 && any(factors[,"name"] == name)) {
    i<- which(factors[,"name"] == name)
    n<- ncol(factors) 
    for(j in (which(colnames(factors) == "levels1")):n) {
      mylevels<- c(mylevels, factors[[i, j]])
    }
  }  
  mylevels
}

#' @title Get the number of factors
#'
#' @description Returns the total number of factors
#'
#' @param factors A collection of factors created with AddFactor
#'
#' @return The number of parameters in factors collection
#'
#' @export
GetFactorsSize<- function(factors) {
  n<- nrow(factors)
  if(is.null(n)) n<- 0
  return(n)
}

#' @title Corrects the LHS design matrix
#'
#' @description Correct the LHS sampling matrix for a
#' specific range applying the lambda function. The default
#' value of 'lambda' is 'qunif'.
#'
#' @param design The LHS design matrix
#' @param factors THe collection of factors
#'
#' @return The corrected design matrix
#'
#' @export
ApplyFactorRange<- function(design, factors) {
  # trunc if flag 'f' is true
  trunciftrue<- function(v, f) {
    if(f) {
      v<- trunc(v)
    }
    v
  }
  
  k<- GetFactorsSize(factors)
  d<- sapply(1:k, function(p) {trunciftrue(match.fun( factors[p,"lambda"])(design[,p],as.numeric(factors[p,"min"]),as.numeric(factors[p,"max"])), factors[p,"int"])})

  if(is.null(nrow(d))) {
    ## --- Handle the case where sample size is 1
    d<- as.data.frame(t(d))
  } else {
    ## --- Handle the case where sample size > 1
    d<- as.data.frame(d)
  }

  names(d)<- factors[,"name"]
  return(d)
}

#' @title Builds the simulation parameter set
#'
#' @description Merges the design matrix with parameters which
#' will be keep fixed along simulation runs.
#'
#' @param design The experimental desing matrix for at least one factor
#' @param parameters All parameters of the repast model.
#'
#' @return A data frame holding all parameters required for running the model
#'
#' @examples \dontrun{
#'    modeldir<- "c:/usr/models/BactoSim(HaldaneEngine-1.0)"
#'    e<- Model(modeldir=modeldir,dataset="ds::Output")
#'    Load(e)
#'
#'    f<- AddFactor(name="cyclePoint",min=40,max=90)
#     f<- AddFactor(factors=f, name="conjugationCost",min=1,max=80)
#'
#'    p<- GetSimulationParameters(e)
#'
#'    d<- AoE.LatinHypercube(factors=f)
#'
#'    p1<- BuildParameterSet(d,p)}
#'
#' @export
BuildParameterSet<- function(design, parameters) {
  v<- as.data.frame(design)
  tmp.p<- parameters
  for(n in names(v)) {
    # Drop parameters columns which are in design matrix
    tmp.p[n]<- NULL
  }

  # Now join two data frames
  for(i in 1:length(names(tmp.p))) {
    v<- cbind(tmp.p[i],v)
  }
  return(v)
}
