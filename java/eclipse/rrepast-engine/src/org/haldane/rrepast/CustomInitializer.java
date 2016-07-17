package org.haldane.rrepast;

import java.util.logging.Logger;

import repast.simphony.context.Context;
import repast.simphony.data2.DataConstants;
import repast.simphony.data2.DataSetManager;
import repast.simphony.data2.DataSetRegistry;
import repast.simphony.data2.DataSink;
import repast.simphony.data2.builder.DataSetBuilder;
import repast.simphony.engine.controller.NullAbstractControllerAction;
import repast.simphony.engine.environment.RunEnvironmentBuilder;
import repast.simphony.engine.environment.RunState;
import repast.simphony.parameter.Parameters;
import repast.simphony.scenario.ModelInitializer;
import repast.simphony.scenario.Scenario;

/**
 * This file is part of the RRepast package - R/Repast interface API
 * 
 * (C)2015 Antonio Prestes Garcia <@> 
 * For license terms see DESCRIPTION and/or LICENSE
 *
 * CustomInitializer.java
 * This class is an implementation of a repast ModelInitializer and is called when 
 * a model's scenario is loaded. The initializer loads a CustomDataSink for an user
 * provided aggregate DataSet name.
 *    
 * 
 * @author Antonio Prestes García
 * $Id$
 */
public class CustomInitializer implements ModelInitializer {
	private static final Logger logger = RepastEngineLogger.getLogger();
	
	@SuppressWarnings("rawtypes")
	public void initialize(Scenario scenario, RunEnvironmentBuilder builder) {
		logger.info("CustomInitializer loaded succesfully!");
		
		scenario.addMasterControllerAction(new NullAbstractControllerAction() {
			private DataSink sink;

			@Override
			public void runInitialize(RunState runState, Context context, Parameters runParams) {
			}

			@Override
			public void batchInitialize(RunState runState, Object contextId) {
				// Instance of model output manager
				ModelOutput output= ModelOutputFactory.getModelOutputStorage();
				
				DataSetRegistry registry = (DataSetRegistry) runState.getFromRegistry(DataConstants.REGISTRY_KEY);
				DataSetManager manager = registry.getDataSetManager(contextId);
				
				// output.getDataSet returns the user provided dataset name
				DataSetBuilder<?> builder = manager.getDataSetBuilder(output.getDataSet());
				if(builder != null) {
					sink = new OutputWrapperDataSink();
					builder.addDataSink(sink);
				}
			}

			public String toString() {
				return "CustomInitializer";
			}

		});
  }
}
