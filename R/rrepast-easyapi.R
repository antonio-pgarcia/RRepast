##================================================================================
## This file is part of the R/Repast package - R/Repast
##
## (C)2016, 2017 Antonio Prestes Garcia <@>
## For license terms see DESCRIPTION and/or LICENSE
##
## @file: rrepast-easyapi.R
##
## This file contains the easy api methods.
##================================================================================


#' @title Easy.getChart
#' 
#' @description Returns the chart instance
#' 
#' @param obj A reference to the output of Easy.Stability
#' @param key The param name
#' 
#' @return The plot instance
#' @export
Easy.getChart<- function(obj, key) {
  if(is.null(obj$charts)) {
    stop("Not an instance of Easy API result!")
  }
  charts<- obj$charts
  chart<- charts[charts[,1] ==  key,]
  return(chart)
}

#' @title Easy.getPlot
#' 
#' @description Returns the chart instance
#' 
#' @param obj A reference to the output of an "Easy" API method
#' @param c The output name
#' @param key The param name
#' 
#' @return The plot instance
#' @export
Easy.getPlot<- function(obj, c, key) {
  if(is.null(obj$charts)) {
    stop("Not an instance of Easy API result!")
  }
  p<- NULL
  
  ## ----- It is a Morris plot
  if(colnames(obj$charts)[2] %in% c("mu.star","mu","mumu")) {
    i<- as.numeric(which(obj$charts[,"criteria"] == c))  
    p<- obj$charts[i, key]  
  }

  p
}

#' @title Easy API for running a model
#' 
#' @description This function provides a simple wrapper for performing a single 
#' or replicated model execution with a single set of parameters.
#' 
#' @param m.dir The installation directory of some repast model
#' @param m.ds The name of any model aggregate dataset
#' @param m.time The total simulated time
#' @param r The number of replications
#' @param default The alternative values for the default model parameters
#' 
#' @export
Easy.Run<- function(m.dir, m.ds, m.time=300, r=1, default=NULL) {
  # --> my.model<- Model(modeldir= m.dir, maxtime = m.time, dataset= m.ds, load=TRUE)  
  
  ## --- Update if needed the default parameters
  # --> if(!is.null(default)) {
  # -->   UpdateDefaultParameters(my.model, default)  
  # --> }
  
  # --> v<- Run(my.model, r)
  # --> v
  WrapperRun(m.dir, m.ds, m.time, r, c(), NULL, default)
}

#' @title Easy API for output stability
#' 
#' @description This functions run model several times in order to determine 
#' how many experiment replications are required for model's output being stable
#' (i.e. the convergence of standard deviation)
#' 
#' @param m.dir The installation directory of some repast model
#' @param m.ds The name of any model aggregate dataset
#' @param m.time The total simulated time
#' @param parameters The factors or model's parameter list
#' @param samples The number of factor samples.
#' @param tries The number of experiment replications
#' @param vars The model's output variables for compute CoV
#' @param FUN The calibration function.
#' @param default The alternative values for parameters which should be kept fixed
#' 
#' @return A list with holding experimnt, object and charts 
#' 
#' @export
Easy.Stability<- function(m.dir, m.ds, m.time=300, parameters, samples=1, tries=100, vars= c(), FUN, default=NULL) {
  my.model<- Model(modeldir=m.dir,maxtime = m.time, dataset=m.ds)
  Load(my.model)
  
  ## --- Update if needed the default parameters
  if(!is.null(default)) {
    UpdateDefaultParameters(my.model, default)  
  }
  
  ## --- Sample the parameter space
  sampling<- AoE.RandomSampling(samples, parameters)
  
  ## --- Get the model declared paramters
  parms<- GetSimulationParameters(my.model)
  
  ## --- Build the experimental parameter set
  exp.design<- BuildParameterSet(sampling, parms)
  
  ## --- Run the experimental setup
  exp<- RunExperiment(my.model,r=tries,exp.design,FUN)
  
  ## --- Get the raw data set for evaluate the Coefficient of Variation
  d<- getExperimentDataset(exp)
  
  ## --- Calculate the coefficient of variation
  rsd<- AoE.Stability(d, vars)
  
  charts<- c()
  for(group in unique(rsd$group)) {
    chart<- Plot.Stability(rsd[rsd$group == group, ],"Simulation output stability")  
    charts<- rbind(charts, list(group=group,plot=chart))
  }
  
  if(length(vars) != 0) {
    chart<- Plot.Stability(rsd,"Simulation output stability")
    charts<- rbind(charts, list(group="all",plot=chart))
  }
  
  results<- list(experiment=exp, object=rsd, charts=charts)
  return(results)
  
}

#' @title Easy API for Morris's screening method
#' 
#' @description This function wraps all calls to perform Morris method.
#' 
#' @param m.dir The installation directory of some repast model
#' @param m.ds The name of any model aggregate dataset
#' @param m.time The total simulated time
#' @param parameters The factors for morris screening.
#' @param mo.p The number of levels for the model's factors.
#' @param mo.r Repetitions. The number of random sampling points of Morris Method.
#' @param exp.r The number of experiment replications
#' @param FUN The calibration function.
#' @param default The alternative values for parameters which should be kept fixed
#' 
#' @return A list with holding experimnt, object and charts 
#' 
#' @importFrom sensitivity tell
#' 
#' @export
Easy.Morris<- function(m.dir, m.ds, m.time=300, parameters, mo.p, mo.r, exp.r, FUN, default=NULL) {
  my.model<- Model(modeldir=m.dir,maxtime = m.time, dataset=m.ds)
  Load(my.model)
  
  ## --- Update if needed the default parameters
  if(!is.null(default)) {
    UpdateDefaultParameters(my.model, default)  
  }

  ## --- Create Morris object
  v.morris<- AoE.Morris(parameters,p=mo.p,r=mo.r)
  
  
  ## --- Get the model declared paramters
  parms<- GetSimulationParameters(my.model)
  
  ## --- Build the experimental parameter set
  exp.design<- BuildParameterSet(v.morris$X,parms)
  
  ## --- Run the experimental setup
  exp<- RunExperiment(my.model,r=exp.r,exp.design,FUN)
  
  charts<- c()
  o<- getExperimentOutput(exp)
  for(k in colnames(o)) {
    if(k != "pset") {
      m<- as.vector(df2matrix(getExperimentOutput(exp),c(k)))
      tell(v.morris,m)
      
      ## --- Plot Morris output
      mustar<- Plot.Morris(v.morris,"mu*sigma", sprintf("output(%s)",k))
      musigma<- Plot.Morris(v.morris,"musigma", sprintf("output(%s)",k))
      mumu<- Plot.Morris(v.morris,"mu*mu", sprintf("output(%s)",k))
      charts<- rbind(charts,list(criteria=k,mu.star=mustar,mu=musigma,mumu=mumu))
    } 
    ### ---> results<- list(experiment=exp, object=v.morris, charts=charts)
  }
  
  results<- list(experiment=exp, object=v.morris, charts=charts)
  return(results)
}

#' @title Easy API for Sobol's SA method
#' 
#' @description This functions wraps all required calls to perform 
#' Sobol method for global sensitivity analysis.
#' 
#' @param m.dir The installation directory of some repast model
#' @param m.ds The name of any model aggregate dataset
#' @param m.time The total simulated time
#' @param parameters The input factors
#' @param exp.n The experiment sample size
#' @param exp.r The number of experiment replications
#' @param bs.size The bootstrap sample size for sobol method
#' @param FUN The objective function.
#' @param default The alternative values for parameters which should be kept fixed
#' @param fsobol The alternative function for calculating sobol indices
#' @param fsampl The function for sampling data
#' 
#' @return A list with holding experimnt, object and charts 
#' 
#' @importFrom stats IQR quantile
#' @importFrom sensitivity tell
#' @importFrom sensitivity sobol sobol2002 sobol2007 sobolmartinez soboljansen
#' 
#' @export
Easy.Sobol<- function(m.dir, m.ds, m.time=300, parameters,exp.n = 500, bs.size = 200, exp.r=1, FUN, default=NULL, fsobol=sobol2002, fsampl=AoE.LatinHypercube) {
  ## --- Instantiate the model
  # (2017/06/10) -----> my.model<- Model(modeldir=m.dir,maxtime = m.time, dataset=m.ds)
  # (2017/06/10) -----> Load(my.model)
  
  ## --- Update if needed the default parameters
  # (2017/06/10) -----> if(!is.null(default)) {
  # (2017/06/10) ----->   UpdateDefaultParameters(my.model, default)  
  # (2017/06/10) -----> }
  
  fix.outliers<- function(x, na.rm = TRUE, ...) {
    qnt<- quantile(x, probs=c(.5, .95), na.rm = na.rm, ...)
    H<- 1.5 * IQR(x, na.rm = na.rm)
    y<- x
    y[x < (qnt[1] - H)]<- (qnt[1] - H)
    y[x > (qnt[2] + H)]<- (qnt[2] + H)
    y
  }
  
  if(!is.function(FUN)) { stop("Invalid objective function!") }
  if(!is.function(fsobol)) { stop("Invalid sobol function!") }  
  
  ## --- Get the model declared paramters
  # (2017/06/10) -----> parms<- GetSimulationParameters(my.model)
  
  ## --- Create a Sobol object
  my.obj<- AoE.Sobol(n= exp.n, parameters, nb=bs.size, fun.doe = AoE.LatinHypercube, fun.sobol=fsobol)
  
  # Build the experimental parameter set
  # (2017/06/10) -----> exp.design<- BuildParameterSet(my.obj$X,parms)
  
  ## --- Run the experimental setup
  # (2017/06/10) -----> exp<- RunExperiment(my.model,r=exp.r,exp.design,FUN)
  
  exp<- WrapperRunExperiment(m.dir, m.ds, m.time, exp.r, my.obj$X, FUN, default)
  
  charts<- c()
  o<- getExperimentOutput(exp)
  for(k in colnames(o)) {
    if(k != "pset") {
      m<- t(df2matrix(getExperimentOutput(exp),c(k)))
      #tell(my.obj,  fix.outliers(m))
      #tell(my.obj, (m-mean(m))/sd(m))
      tell(my.obj, m)
      
      # -- First order indexes
      chart_0<- Plot.Sobol(my.obj, 1, paste("Sobol indexes for", k))
      
      # -- Total order indexes
      chart_1<- Plot.Sobol(my.obj, 2, paste("Sobol indexes for", k))
      
      charts<- rbind(charts,list(chart=chart_0))
      charts<- rbind(charts,list(chart=chart_1))
    } 
    ### ---> results<- list(experiment=exp, object=my.obj, charts=charts)
  }
  results<- list(experiment=exp, object=my.obj, charts=charts)
  return(results)
}

#' @title Easy.Setup
#' 
#' @description This function configures the deployment directory 
#' where logs and output dataset will be generated.  By default 
#' the deployment directory will be created under the model 
#' installation directory. The output generated by the Repast model 
#' will be redirected to the SystemOut.log file.  
#' 
#' @details If the deployment directory is empty the installation 
#' directory given by the parameter \code{model} is used instead as 
#' the base directory. The deployment directory is \code{/rrepast-deployment/}.
#' 
#' @param model The base directory where Repast model is installed.
#' @param multicore Bolean flag indicating to use multiplecore.
#' @param deployment The directory to save the output and logs.
#' 
#' @export
Easy.Setup<- function(model, multicore=FALSE, deployment=c()){
  ## Check if model has been configured with the integration code
  if(!config.check(model)) {
    if(!config.copylib(model)) {
      stop("Error deploying integration libraries!")
    }
    if(!config.scenario(model)) {
      stop("Unable to configure integration code!")
    }
  }
  
  ## Multicore selection
  parallelize(multicore)
  
  if(length(deployment) == 0) {
    deployment<- paste0(model,"/rrepast-deployment/")
  }
  
  setOutputDir(deployment)
  
  ## -- Create output dir if required
  createOutputDir()
  
  jvm.init()
  jvm.setOut("SystemOut.log")
  PB.enable()
  
  ## -- Reset stats
  enginestats.reset()
}

#' @title Easy.Calibration
#' 
#' @description Search for the best set of parameters trying to 
#' minimize the calibration function provided by the user. The function 
#' has to operational models, the first based on the experimental setup 
#' where all parameters are defined a priori and the second using 
#' optimization techniques. Currently the only supported optimization 
#' technique is the particle swarm optimization.
#' 
#' @param m.dir The installation directory of some repast model
#' @param m.ds The name of any model aggregate dataset
#' @param m.time The total simulated time
#' @param parameters The input factors
#' @param exp.n The experiment sample size
#' @param exp.r The number of experiment replications
#' @param smax The number of solutions to be generated
#' @param design The sampling scheme ["lhs"|"mcs"|"ffs"]
#' @param FUN The calibration function.
#'
#' @return A list with holding experiment, object and charts 
#' 
#' @examples \dontrun{
#'  my.cost<- function(params, results) {
#'    criteria<- c()
#'    Rate<- AoE.RMSD(results$X.Simulated,results$X.Experimental)
#'    G<- AoE.RMSD(results$G.T.,52)
#'    total<- Rate + G
#'    criteria<- cbind(total,Rate,G)
#'    return(criteria)
#'  }
#'  
#'  Easy.Setup("/models/BactoSim")
#'  v<- Easy.Calibration("/models/BactoSim","ds::Output",360,
#'                        f,exp.n = 1000, exp.r=1, smax=4, 
#'                        design="mcs", my.cost)
#'  
#' }
#' 
#' @export
Easy.Calibration<- function(m.dir, m.ds, m.time=300, parameters, exp.n = 100, exp.r=1, smax=4, design="lhs", FUN) {
  ## --- Sample the parameter space
  
  method<- "simple"
  switch(method,
         simple = {
           v<- simple.fitting(m.dir, m.ds, m.time, parameters, exp.n, exp.r, design , smax, FUN)      
         }, 
         stop("Valid calibration methods are [simple|pso]")
  )
  
  return(v)
}


##
## ----- Below calibration support methods
##


#' @title simple.fitting
#' 
#' @description Simple calibration method. Run an experimental setup and select the 
#' the best results minimizing the calibration function
#' 
#' @param m.dir The installation directory of some repast model
#' @param m.ds The name of any model aggregate dataset
#' @param m.time The total simulated time
#' @param parameters The input factors
#' @param samples The experiment sample size
#' @param tries The number of experiment replications
#' @param design The sampling scheme ["lhs"|"mcs"|"ffs"]
#' @param smax The number of solutions to be generated
#' @param objective The calibration function.
#'
#' @importFrom gridExtra ttheme_default tableGrob arrangeGrob
#' @export
simple.fitting<- function(m.dir, m.ds, m.time=300, parameters, samples=100, tries=1, design="lhs" , smax=4, objective) {
  ## --- Instantiate the model
  my.model<- Model(modeldir=m.dir,maxtime = m.time, dataset=m.ds,load = TRUE)
  
  
  ## --- Sample the parameter space
  switch(design,
         lhs = {
           sampling<- AoE.LatinHypercube(samples, parameters)  
         },
         
         mcs = {
           sampling<- AoE.RandomSampling(samples, parameters)
         },
         
         ffs = {
           sampling<- AoE.FullFactorial(samples, parameters)
         },
         
         stop("Valid sampling types are [mcs|lhs|ffs]")
         
  )
  
  
  ## --- Get the model declared paramters
  parms<- GetSimulationParameters(my.model)
  
  ## --- Build the experimental parameter set
  exp.design<- BuildParameterSet(sampling, parms)
  
  ## --- Run the experimental setup
  exp<- RunExperiment(my.model,r=tries,exp.design,objective)
  
  ## --- Add a totalization column
  exp$output<- col.sum(exp$output)
  
  tbl.theme<- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  
  ##tmp<- c()
  charts<- c()
  obj<- c()
  fittest.max<- smax 
  
  o<- getExperimentOutput(exp)
  for(k in colnames(o)) {
    if(k != "pset") {
      p<- getExperimentParamSet(exp)
      best<- dffilterby(o,"pset",pick.fittest(o,goals=c(k),fittest.max)$pset)
      chart<- Plot.Calibration(best,k,paste0("Best parameters for ", k))
      
      best.p<- dffilterby(p,"pset",pick.fittest(o,goals=c(k),fittest.max)$pset)
      tbl.data<- best.p[,c("pset",parameters[,"name"])]
      tbl.data<- dfround(tbl.data,2)
      tbl.table<- tableGrob(tbl.data, rows=NULL, theme= tbl.theme)
      my.chart<- arrangeGrob(chart, tbl.table, nrow=2, as.table=TRUE, heights=c(3,1))
      
      charts<- rbind(charts,list(variable=k,both=my.chart,chart=chart,table=tbl.table))
      obj<- rbind(obj,list(variable=k,parameters=best.p,objective=best))
    }
  }
  
  ###obj$data<- tmp
  ###obj$keys<- unlist(obj$data[,"variable"])
  ###obj$parameters<- function(k) {obj$data[(obj$data[,"variable"] == k),"parameters"][[1]]}
  ###obj$objective<- function(k) {obj$data[(obj$data[,"variable"] == k),"objective"][[1]]}
  
  results<- list(experiment=exp, object=obj, charts=charts)
  return(results)
}

#' @title Easy.ShowModelParameters
#' 
#' @description Returns the list current model parameters
#' 
#' @param v The installation directory of some repast model
#'
#' @return The model parameters
#' @export
Easy.ShowModelParameters<- function(v) {
  e<- Model(modeldir=v, maxtime=10, dataset="ds::Output", load=TRUE)
  GetSimulationParameters(e)
}


##
## ----- Functions for accessing result object members
##

#' @title Results.GetExperiment
#'
#' @description Simplify the access to the experiment member
#'
#' @param obj An instance of the object returned by \code{Easy} methods
#'
#' @return The experiment element inside results
#' @export
Results.GetExperiment<- function(obj) {
  if(is.null(obj$experiment)) {
    stop("Not an instance of Easy API result!")
  }
  obj$experiment
}

#' @title Results.GetObject
#'
#' @description Simplify the access to the object member
#'
#' @param obj An instance of the object returned by \code{Easy} methods
#'
#' @return The object element inside results
#' @export
Results.GetObject<- function(obj) {
  if(is.null(obj$object)) {
    stop("Not an instance of Easy API result!")
  }
  obj$object
}

#' @title Results.GetCharts
#'
#' @description Simplify the access to the charts member
#'
#' @param obj An instance of the object returned by \code{Easy} methods
#'
#' @return The charts element inside results
#' @export
Results.GetCharts<- function(obj) {
  if(is.null(obj$charts)) {
    stop("Not an instance of Easy API result!")
  }
  obj$charts
}
