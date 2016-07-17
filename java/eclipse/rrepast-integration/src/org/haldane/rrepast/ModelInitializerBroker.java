package org.haldane.rrepast;

import java.util.logging.Logger;

import repast.simphony.engine.environment.RunEnvironmentBuilder;
import repast.simphony.scenario.ModelInitializer;
import repast.simphony.scenario.Scenario;


/**
 * This file is part of the RRepast package - R/Repast interface API
 * 
 * (C)2015 Antonio Prestes Garcia <@> 
 * For license terms see DESCRIPTION and/or LICENSE
 *
 * ModelInitializerBroker.java
 * This class loads the R integration
 * 
 * To configure the RRepast in repast models just add the 
 * following line to the scenario.xml file.
 * 
 * <model.initializer class="org.haldane.rrepast.ModelInitializerBroker" />    
 * 
 * @author Antonio Prestes García
 * $Id$
 */

public class ModelInitializerBroker implements ModelInitializer {
	private final static String LOGGER= "rrepast";
	private static final Logger logger = Logger.getLogger(LOGGER);
	
  	@SuppressWarnings("rawtypes")
	public void initialize(Scenario scenario, RunEnvironmentBuilder builder) {
		logger.info("Running initialize method!");
		
		Class clazz = null;
	    Object object = null;
	    
	    String name= (String) System.getProperty("rrepast.modelinitializer");
	    if(name == null || name.isEmpty()) {
	    	logger.info("Could not found a CustomIntializer!");
	    	return;
	    }
	    
	    try {
			clazz = Class.forName(name);
			object = clazz.newInstance();
			((ModelInitializer) object).initialize(scenario, builder);
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InstantiationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

  }
}