##================================================================================
## This file is part of the R/Repast package - R/Repast
##
## (C)2016, 2017 Antonio Prestes Garcia <@>
## For license terms see DESCRIPTION and/or LICENSE
##
## @file: rrepast-engine.R
##
## This file contains the low level repast engine functions
##================================================================================

# ----- internal functions

# Define some required functions when not available from current R version
compatibility<- function() {
  if(getRversion() <= "3.1") {
    # dir.exists function is only available from R 3.2
    f<- function(d) {
      cat("my dir.exists()")
      v<- file.info(d)$isdir
      return(ifelse(is.na(v), FALSE, v))
    }
    
    # This is a trick to get the reference to the current environment
    e<- as.environment(environment(enginejar))
    assign("dir.exists", f, e, immediate = FALSE)
  }
}

# Returns the xml integration string
xml.integration<- function() {
  "<model.initializer class=\"org.haldane.rrepast.ModelInitializerBroker\" />"
}

# Returns the wrapper classes jar file location.
enginejar<- function() {
  
  # Try to guess the rrepast-engine.jar location
  for(p in .libPaths()) {
    f0<- paste0(p,"/rrepast/java/rrepast-engine.jar")
    f1<- paste0(p,"/rrepast/inst/java/rrepast-engine.jar")
    if(file.exists(f0)) {
      f<- f0
      break
    } else if(file.exists(f1)) {
      f<- f1
      break
    }
  }
  return(f)
}

#' @title jarfile
#'
#' @description The jarfile returns the full path to some jar file
#' available inside rrpast package
#'
#' @param fjar The name of jar file
#'
#' @return The full path to jar file
#' @export
jarfile<- function(fjar) {
  for(p in .libPaths()) {
    f0<- paste0(p,"/rrepast/java/",fjar)
    f1<- paste0(p,"/rrepast/inst/java/",fjar)
    
    if(file.exists(f0)) {
      f<- f0
      break
    } else if(file.exists(f1)) {
      f<- f1
      break
    }
  }
  return(f)
}

#' @title config.scenario
#'
#' @description Add the integration library to the model's configuration
#'
#' @param modelpath The path where model is installed
#' @param uninstall If TRUE restore original scenario.xml file
#' 
#' @return A logical TRUE if the model's scenario file has been modified
#'
#' @export
config.scenario<- function(modelpath, uninstall=FALSE) {
  status<- FALSE
  i<- 1
  v<- c()
  if(!check.scenario(modelpath)) {
    fscenario<- paste0(getScenarioDir(),"/","scenario.xml")
    if(file.exists(fscenario)) {
      ## Backup the original scenario file
      if(file.copy(fscenario,paste0(fscenario,".ORIGINAL"))) {
        txts<- readLines(fscenario, warn= FALSE)
        for(j in 1:length(txts)) {
          v[i]<- txts[j]
          if(length(grep("<Scenario.*>",txts[j]))) {
            v[(i<- i + 1)]<- xml.integration()
            status<- TRUE
          }
          i<- i + 1
        }
        ## Write the new scenario file when integration succeed
        writeLines(v,fscenario)
        
      }
    }
  } else {
    ## Restore the original scenario file
    if(uninstall) {
      fscenario<- paste0(getScenarioDir(),"/","scenario.xml")
      status<- file.copy(paste0(fscenario,".ORIGINAL"),fscenario)
    }
  }
  return(status)
}

#' @title config.copylib
#'
#' @description Install or uninstall the integration jar file. This function
#' manages the installation process of required jars to the model lib dir.
#'
#' @param modelpath The path where model is installed
#' @param uninstall If TRUE uninstall integration jar
#' 
#' @return TRUE if install operation succed
#'
#' @export
config.copylib<- function(modelpath, uninstall=FALSE) {
  configModelDirs(modelpath)
  jjar<- "rrepast-integration.jar"
  ljar<- paste0(getModelLibDir(),"/",jjar)
  sjar<- jarfile(jjar)
  v<- FALSE
  if(file.exists(ljar)) {
    if(uninstall) {
      v<- file.remove(ljar)
    }
  } else {
    v<- file.copy(sjar,ljar)
  }
  return(v)
}

#' @title config.check
#'
#' @description Verify if the installed model is correctelly configurated.
#'
#' @param modelpath The path where model is installed
#'
#' @return TRUE when all requisites are met
#' @export
config.check<- function(modelpath) {
  return(check.scenario(modelpath) && check.integration(modelpath))
}

#' @title check.scenario
#'
#' @description Check if the scenario.xml is configured with the rrepast
#' itegration code
#'
#' @param modelpath The path where model is installed
#'
#' @return TRUE if scenario is properly configured
#' @export
check.scenario<- function(modelpath) {
  v<- FALSE
  configModelDirs(modelpath)
  fscenario<- paste0(getScenarioDir(),"/","scenario.xml")
  if(file.exists(fscenario)) {
    if(length(grep(xml.integration(),readLines(fscenario, warn= FALSE)))) {
      v<- TRUE
    }
  }
  return(v)
}

#' @title check.integration
#'
#' @description Check if the integration jar library is correctelly installed
#' in the model lib directory
#'
#' @param modelpath The path where model is installed
#'
#' @return TRUE if the integration code is correctelly deployed
#' @export
check.integration<- function(modelpath) {
  configModelDirs(modelpath)
  return(file.exists(paste0(getModelLibDir(),"/","rrepast-integration.jar")))
}


# Return the name of repast engine class name.
engineclazz<- function() {
  return("org.haldane.rrepast.RepastEngine")
}

# Returns the repast simphony library dir
simphonylib<- function() {
  b<- get("pkg.basedir", pkg.globals)
  l<- get("pkg.repastlibdir", pkg.globals)
  return(paste0(b,l))
}

# Configure all model directories based on default installation values
configModelDirs<- function(s) {
  d<- basename(s)
  setBaseDir(s)
  setModelDir(paste0(paste0(s,"/"),d))
  setScenarioDir(paste0(paste0(paste0(getModelDir(),"/"),d),".rs"))
  setModelLibDir(paste0(getModelDir(),"/lib"))
}

# Stats ----------

#' @title enginestats.reset
#' @description  Reset internal statistics
#'
#' @export
enginestats.reset<- function() {
  assign("pkg.stats.calls", 0, pkg.globals)
}

#' @title enginestats.calls
#' @description  Return the current calls to the 'Engine.RunModel' function
#' 
#' @param increment A flag telling to increment and update the counter
#'
#' @return The number of calls to 'Engine.RunModel' 
#' @export
enginestats.calls<- function(increment=FALSE) {
  v<- get("pkg.stats.calls", pkg.globals)
  if(increment) {
    v<- v + 1
    assign("pkg.stats.calls", v, pkg.globals)  
  }
  v
}


# Setters and Getters ----------

#' @title Sets the model name
#' @description  Set the name of the model currently instantiated.
#'
#' @param s The model name
#'
#' @export
setId<- function(s) {
  assign("pkg.id", s, pkg.globals)
}

#' @title Gets the model name
#' @description  Provides the name of the model currently instantiated.
#'
#' @export
getId<- function() {
  return(get("pkg.id", pkg.globals))
}

#' @title Sets Repast randomSeed name
#' @description Configures a non-default value for Repast randomSeed
#' parameter name.
#'
#' @param k The string with an alternative name for randomSeed
#'
#' @export
setKeyRandom<- function(k){
  assign("pkg.randomSeed",k, pkg.globals)
}

#' @title Gets Repast randomSeed name
#' @description Returns the Repast randomSeed parameter name.
#'
#' @return A string value holding the randomSeed name.
#'
#' @export
getKeyRandom<- function() {
  return(get("pkg.randomSeed", pkg.globals))
}

#' @title parallelize
#' @description Tells R/Repast to use multicore. Default 
#' is using just one core.
#' 
#' @param v A Bollean value telling if use multiple cores. 
#' When null just returns the current setting
#'
#' @return Boolean with current state
#'
#' @export
parallelize<- function(v=NULL) {
  if(!is.null(v)) {
    assign("pkg.parallelize", v, pkg.globals)
  }
  get("pkg.parallelize", pkg.globals)
}

#' @title Sets output directory
#'
#' @description Configure the desired directoy to save model
#' output data.
#'
#' @param s The full path for output directory
#'
#' @export
setOutputDir<- function(s) {
  assign("pkg.outputdir", s, pkg.globals)
}

#' @title Gets output directory
#'
#' @description Returns the value of module variable for
#' storing the current output directory.
#'
#' @export
getOutputDir<- function() {
  return(get("pkg.outputdir", pkg.globals))
}

#' @title getLogDir()
#'
#' @description Returns the value for log directory
#'
#' @export
getLogDir<- function() {
  return(paste0(getOutputDir(),"Log/"))
}

#' @title Create output directory
#'
#' @description A simple function to make a directory to save the
#' model's data.
#'
#' @details Create the, if required, the directory to save the
#' output data generate by the model. It is intended for internal
#' use.
#'
#' @export
createOutputDir<- function() {
  lambda<- function(d) {
    if(!dir.exists(d)) {
      dir.create(d)
    }
  }
  
  ## -- Create required directories
  lambda(getOutputDir())
  lambda(getLogDir())
}



# Set the directory where repast model is installed
setBaseDir<- function(s) {
  assign("pkg.basedir", s, pkg.globals)
}

# Gets the directory where repast model is installed
getBaseDir<- function() {
  return(get("pkg.basedir", pkg.globals))
}

# Sets the directory where repast model is installed which normally
# is a subdirectory below installation base directory
setModelDir<- function(s) {
  assign("pkg.modeldir", s, pkg.globals)
}

# Sets the directory where repast model is installed which normally
# is a subdirectory below installation base directory
getModelDir<- function() {
  return(get("pkg.modeldir", pkg.globals))
}

# Sets the model's scenario directory
setScenarioDir<- function(s) {
  assign("pkg.scenariodir", s, pkg.globals)
}

# Gets the model's scenario directory
getScenarioDir<- function() {
  return(get("pkg.scenariodir", pkg.globals))
}

# Sets the model's lib directory
setModelLibDir<- function(s) {
  assign("pkg.modellibdir", s, pkg.globals)
}

# Gets the model's lib directory
getModelLibDir<- function() {
  return(get("pkg.modellibdir", pkg.globals))
}

# Traverse the lib dir to build up the classpath
repastlibs<- function() {
  libdir<- simphonylib()
  for(d in dir(libdir)) {
    # On bin dir we expect unpackaged class files
    bin<- paste0(libdir,paste0(d,"/bin"))
    lib<- paste0(libdir,paste0(d,"/lib"))
    # adding the bin dir to rjava classpath
    rJava::.jaddClassPath(bin)
    
    repastjars(paste0(libdir,d))
    repastjars(lib)
    repastjars(getModelLibDir())
  }
}

# Search for jar files inside lib dir and then add it to classpath
repastjars<- function(lib) {
  jars<- dir(lib,pattern="*.jar")
  for(j in jars) {
    jar<- paste0(paste0(lib,"/"),j)
    # adding jar file to classpath
    rJava::.jaddClassPath(jar)
  }
}


# ----- Exposed package API functions

#' @title jvm.enablejmx
#'
#' @description Enable jmx for the current R/rJava session
#'
#' @details Configures the JMX subsystem for the current session of R/rJava. 
#' This function must be called before any other function which initializes 
#' r/Java such as \code{\link{Easy.Setup}} or \code{\link{Model}} otherwise it will have no effect. 
#'
#'
#' @examples \dontrun{
#'    jvm.enablejmx()}
#'
#' @export
jvm.enablejmx<- function() {
  s<- jvm.get_parameters()
  
  s<- paste(s,"-Dcom.sun.management.jmxremote -Dcom.sun.management.jmxremote.ssl=false -Dcom.sun.management.jmxremote.authenticate=false -Djava.rmi.server.hostname=0.0.0.0 -Dcom.sun.management.jmxremote.port=9100")
  s<- jvm.set_parameters(s)
}

#' @title jvm.getruntime
#'
#' @description A wrapper for \code{System.getRuntime()}
#'
#' @details A simple wrapper for \code{System.getRuntime()} java method
#'
#' @export
jvm.getruntime<- function() {
  rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;","getRuntime")  
}

#' @title jvm.memory
#'
#' @description JVM memory state
#'
#' @details Provides information about the memory used by the JVM subsystem 
#'
#' @export
jvm.memory<- function() {
  mb<- 2014^2
  runtime<- jvm.getruntime() 
  
  maxMemory<- rJava::.jcall(runtime, "J", "maxMemory")/mb 
  freeMemory<- rJava::.jcall(runtime, "J", "freeMemory")/mb
  totalMemory<- rJava::.jcall(runtime, "J", "totalMemory")/mb
  
  cbind(maxMemory, freeMemory, totalMemory)
}

#' @title jvm.runtimegc
#'
#' @description A wrapper for \code{Runntime.gc()}
#'
#' @details Forces the execution of the JVM garbage collector  
#'
#' @export
jvm.runtimegc<- function() {
  rJava::.jcall(jvm.getruntime(), "J", "gc")  
}

#' @title jvm.set_parameters
#'
#' @description Configures the jvm parameters
#'
#' @details Set the underlying parameters for java virtual machine. The default
#' values are "-server -Xms1024m -Xmx1024m". These defaults can be changed
#' to fit the model requirements.
#'
#' @param s The paramter string to be passed to the underlying JVM
#'
#' @examples \dontrun{
#'    jvm.set_parameters("-server -Xms512m -Xmx2048m")}
#'
#' @export
jvm.set_parameters<- function(s) {
  assign("pkg.java.parameters", s, pkg.globals)
}

#' @title jvm.get_parameters
#'
#' @description Returns the current java virtual machine parameters
#'
#' @return A string with JVM parameters.
#'
#' @export
jvm.get_parameters<- function() {
  return(get("pkg.java.parameters", pkg.globals))
}

#' @title Init R/JVM environment
#'
#' @description Initialize rJava and repast environment with classpath. This function
#' is called internally and it is not meant to be used directlly.
#'
#' @details The default parameters can be changed as needed calling the
#' primitive \code{\link{jvm.set_parameters}} befor instantiating the model
#' engine.
#'
#' @examples \dontrun{
#'      jvm.init()}
#'
#' @references
#' [1] rJava: Low-Level R to Java Interface. Low-level interface to Java VM
#' very much like .C/.Call and friends. Allows creation of objects,
#' calling methods and accessing fields.
#'
## @import rJava
jvm.init<- function() {
  # The default parameters can be changed as needed
  rJava::.jinit(force.init = TRUE, parameters= jvm.get_parameters() )
  #options(java.parameters=jvm.get_parameters())
  #.jpackage("rrepast")
  
  rJava::.jaddClassPath(enginejar())
  rJava::.jaddClassPath(paste0(getModelDir(),"/bin"))
  # ----- Repast base libraries
  repastlibs()
}

#' @title jvm.setOut
#'
#' @description Set the System.out filed to a file
#'
#' @param f The output file name
#'
#' @examples \dontrun{
#'    jvm.setOut("/tmp/SysteOut.log")}
#'
## @import rJava
#'
#' @export
jvm.setOut<- function(f) {
  ## -- Create the output dir if required
  createOutputDir()
  
  my.f<- paste0(getLogDir(),f)
  ## -- Java calls to redirect the output to a file
  obj.fos<- rJava::new(rJava::J("java.io.FileOutputStream"),my.f)
  obj.ps<- rJava::new(rJava::J("java.io.PrintStream"),obj.fos)
  rJava::.jcall("java/lang/System","V", "setOut",obj.ps)
}

#' @title jvm.resetOut
#'
#' @description Reset the System.out filed value to console output
#'
#' @examples \dontrun{
#'    jvm.resetOut()}
#'
## @import rJava
#'
#' @export
jvm.resetOut<- function() {
  obj.os<- rJava::.jfield("java/io/FileDescriptor", name="out")
  obj.fos<- rJava::new(rJava::J("java.io.FileOutputStream"),obj.os)
  obj.ps<- rJava::new(rJava::J("java.io.PrintStream"),obj.fos)
  rJava::.jcall("java/lang/System","V", "setOut",obj.ps)
}

# ----- Wrapper functions for Engine class method calls


#' @title Engine
#'
#' @description Creates an instance of Engine
#'
#' @details This function creates an instance of Repast model wrapper
#' class. Before invoking the function Engine, make sure that
#' environment was correctly initialized.
#'
#' @return An onject instance of Engine class
#'
#' @export
Engine<- function() {
  return(rJava::new(rJava::J(engineclazz())))
}

#' @title Engine.LoadModel
#'
#' @description Loads the model's scenario files
#'
#' @details This function loads the scenario of a Repast Model and
#' initialize de model.
#'
#' @param e An engine object instance
#' @param f The full path of scenario directory
#'
#' @export
Engine.LoadModel<- function(e,f) {
  rJava::.jcall(e,"V", "LoadModel",f)
}

#' @title Engine.SetAggregateDataSet
#'
#' @description Sets the model's dataset
#'
#' @details Configure a dataset with the desired output values
#' to be "drained" by the function Engine.GetModelOutput.
#'
#' @param e An engine object instance
#' @param k The repast model's data set name
#'
#' @examples \dontrun{
#'    d<- "C:/usr/models/your-model-directory"
#'    m<- Model(d)
#'    setAggregateDataSet(m,"dataset-name")}
#'
#' @export
Engine.SetAggregateDataSet<- function(e,k) {
  rJava::.jcall(e,"V","ModelDataSet",k)
}

#' @title Engine.getParameterNames
#'
#' @description Get the parameter names
#'
#' @details Returns the names of all declared model's parameters in
#' the parameter.xml file in the scenario directory.
#'
#' @param e An engine object instance
#'
#' @return A collection of parameter names
#'
#' @export
Engine.getParameterNames<- function(e) {
  names<- rJava::.jcall(e,"[S","getParameterNames")
  return(names)
}

#' @title Engine.getParameter
#'
#' @description The function gets the value of model
#' parameter \code{k} as java.lang.Object
#'
#' @param e An engine object instance
#' @param k The parameter name
#'
#' @return The parameter value
#'
#' @export
Engine.getParameter<- function(e,k) {
  v<- rJava::.jcall(e,"Ljava/lang/Object;","getParameter",k)
  return(v)
}

#' @title Engine.getParameterType
#'
#' @description Returns the declared type of a Repast
#' model parameter
#'
#' @param e An engine object instance
#' @param k The parameter name
#'
#' @return The parameter type string
#'
#' @export
Engine.getParameterType<- function(e,k) {
  v<- rJava::.jcall(e,"Ljava/lang/String;","getParameterType",k)
  v
}

#' @title Engine.getParameterAsString
#'
#' @description Get the value of model parameter \code{k}
#' as \code{java.lang.String}
#'
#' @param e An engine object instance
#' @param k The parameter name
#'
#' @return The parameter value as string
#'
#' @export
Engine.getParameterAsString<- function(e,k) {
  v<- rJava::.jcall(e,"Ljava/lang/String;","getParameterAsString",k)
  return(v)
}

#' @title Engine.getParameterAsNumber
#'
#' @description Get the value of model parameter
#' \code{k} as \code{java.lang.Number}
#'
#' @param e An engine object instance
#' @param k The parameter name
#'
#' @return The parmeter value as number
#'
#' @export
Engine.getParameterAsNumber<- function(e,k) {
  v<- rJava::.jcall(e,"Ljava/lang/Number;","getParameterAsNumber",k)
  return(v)
}

#' @title Engine.getParameterAsDouble
#'
#' @description Get the value of model parameter \code{k} as \code{java.lang.Double}
#'
#' @param e An engine object instance
#' @param k The parameter name
#'
#' @return The parmeter value as double
#'
#' @export
Engine.getParameterAsDouble<- function(e,k) {
  v<- rJava::.jcall(e,"D","getParameterAsDouble",k)
  return(v)
}

#' @title Engine.setParameter
#'
#' @description Set the value of model parameter
#'
#' @param e An engine object instance
#' @param k The parameter name
#' @param v The parameter value
#'
#' @export
Engine.setParameter<- function(e,k,v) {
  # Map the R type system to java object
  switch(Engine.getParameterType(e,k),
         java.lang.String = {
           value<- rJava::new(rJava::J("java.lang.String"), as.character(v))
         },
         
         double = {
           value<- rJava::new(rJava::J("java.lang.Double"), as.double(v))
         },
         
         int = {
           value<- rJava::new(rJava::J("java.lang.Integer"), as.integer(v))
         },
         
         boolean = {
           value<- rJava::new(rJava::J("java.lang.Boolean"), as.logical(v))
         })
  # Invoke the setParamter method
  rJava::.jcall(e,"V","setParameter",k,value)
}

#' @title Engine.endAt
#'
#' @description Configure the maximun simulated time for
#' the current model run
#'
#' @param e An engine object instance
#' @param v The number of Repast time ticks
#'
#' @export
Engine.endAt<- function(e,v) {
  rJava::.jcall(e,"V","endAt",v)
}

#' @title Returns the model id
#'
#' @description This function provides a wrapper to the method getId()
#' from repast context. The id is basically a String with the currently
#' instantiated model name.
#'
#' @param e An engine object instance
#'
#' @export
Engine.getId<- function(e) {
  id<- rJava::.jcall(e,"S","getId")
  if(nchar(id) == 0) stop("Model not initilized.")
  return(id)
}

#' @title Engine.RunModel
#'
#' @description Performs the execution of Repast model
#'
#' @param e An engine object instance
#'
#' @export
Engine.RunModel<- function(e) {
  enginestats.calls(TRUE)
  rJava::.jcall(e,"V","RunModel")
}

#' @title Engine.resetModelOutput
#'
#' @description Resets the the model output holder
#'
#' @param e An engine object instance
#'
#' @export
Engine.resetModelOutput<- function(e) {
  rJava::.jcall(e,"V","resetModelOutput")
}

#' @title Engine.GetModelOutput
#'
#' @description Gets the model output data as a CSV String array.
#' Calls the engine method GetModelOutput to drain model output
#' data.
#'
#' @param e An engine object instance
#'
#' @return An array of strings containing the model's output
#'
#' @examples \dontrun{
#'    d<- "c:/usr/models/your-model-directory"
#'    m<- Model(d)
#'    csv<- Engine.GetModelOutput(m)}
#' @importFrom utils read.csv
#' @export
Engine.GetModelOutput<- function(e) {
  rJava::.jcall(e,"[S","GetModelOutput")
}

#' @title Engine.Finish
#'
#' @description Performs a cleanup on a engine instance.Finalize
#' and destroy repast controller data.
#'
#' @param e An engine object instance
#'
#' @export
Engine.Finish<- function(e) {
  enginestats.reset()
  rJava::.jcall(e,"V","cleanUpBatch")
}

#' @title Set the log level to INFO
#'
#' @description Configures the underlying logging system
#'
#' @export
Logger.setLevelInfo<- function() {
  logger<- rJava::J("org.haldane.rrepast.RepastEngineLogger")
  rJava::.jcall(logger,"V","setLevelInfo")
}

#' @title Set the log level to WARNING
#'
#' @description Configures the underlying logging system
#'
#' @export
Logger.setLevelWarning<- function() {
  logger<- rJava::J("org.haldane.rrepast.RepastEngineLogger")
  rJava::.jcall(logger,"V","setLevelWarning")
}

#' @title ShowModelPaths
#'
#' @description Prints the paths. Shows the directories
#' currently used to load model scenario and lib. The output of
#' this function is informational only and can be used to check
#' whether model data is being loaded properly from
#' correct locations.
#'
#' @examples \dontrun{
#'    ShowModelPaths()}
#'
#' @export
ShowModelPaths<- function() {
  print(paste("Install dir.= ",getBaseDir()))
  print(paste("Model dir...= ",getModelDir()))
  print(paste("Scenario dir= ",getScenarioDir()))
  print(paste("Model lib...= ",getModelLibDir()))
}

#' @title ShowClassPath
#'
#' @description Shows the current classpath
#'
#' @return the current setting of JVM classpath
#'
#' @examples \dontrun{
#'    ShowClassPath()}
#'
#' @export
ShowClassPath<- function() {
  rJava::.jclassPath()
}
