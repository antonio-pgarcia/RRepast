package org.haldane.rrepast;

import java.io.File;
import java.util.Vector;
import java.util.logging.Logger;

import repast.simphony.batch.BatchScenarioLoader;
import repast.simphony.data2.DataSetManager;
import repast.simphony.engine.controller.DefaultController;
import repast.simphony.engine.environment.AbstractRunner;
import repast.simphony.engine.environment.ControllerRegistry;
import repast.simphony.engine.environment.DefaultRunEnvironmentBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.environment.RunState;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.Schedule;
import repast.simphony.parameter.ParameterSchema;
import repast.simphony.parameter.Parameters;
import repast.simphony.scenario.Scenario;
import repast.simphony.scenario.ScenarioLoadException;
import simphony.util.messages.MessageCenter;

/**
 * This file is part of the RRepast package - R/Repast interface API
 * 
 * (C)2015 Antonio Prestes Garcia <@> 
 * For license terms see DESCRIPTION and/or LICENSE
 *
 * RepastEngine.java
 * This class is the wrapper providing the interface used inside R 
 * project for running Repast models 
 *    
 * 
 * @author Antonio Prestes García
 * $Id$
 */
public class RepastEngine extends AbstractRunner { 
	private static final Logger logger = RepastEngineLogger.getLogger();
	private static MessageCenter msgCenter = MessageCenter.getMessageCenter(RepastEngine.class);
	
	private ModelOutput output = ModelOutputFactory.getModelOutputStorage();
	private Scenario scenario;
	private DataSetManager manager;
	private Parameters parameters;
	private ISchedule schedule;
	private Number endAt= 0;
	private int runcount= 1;

	public RepastEngine() {
		System.setProperty("rrepast.modelinitializer", "org.haldane.rrepast.CustomInitializer");
		
		environmentBuilder = new DefaultRunEnvironmentBuilder(this, true);
		controller = new DefaultController(environmentBuilder);
		controller.setScheduleRunner(this);
		
		logger.info("RepastEngine instantiated successfully!");
	}

	public void ModelDataSet(String s) {
		output.setDataSet(s);
	}
	
	/*
	 * Loads the model scenario
	 * 
	 */
	public void LoadModel(String s) {
		File file = new File(s); 
		
		/* 
		 * This inner class is a workaround to obtain the reference
		 * to loaded scenario because the method getScenario defined 
		 * inside the BatchScenarioLoader class rises an class cast 
		 * exception  
		 *
		 */
		class MyBatchScenarioLoader extends BatchScenarioLoader {
			public MyBatchScenarioLoader(File scenarioDir) {
				super(scenarioDir);
				// TODO Auto-generated constructor stub
			}
			
			public Scenario getMyScenario() {
				return scenario;
			}
		}
		
		MyBatchScenarioLoader loader;
		if (file.exists()) {
			loader = new MyBatchScenarioLoader(file);
			ControllerRegistry registry;
			try {
				registry = loader.load(environmentBuilder);
				controller.setControllerRegistry(registry);
			} catch (ScenarioLoadException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else {
			msgCenter.error("Scenario not found", new IllegalArgumentException(
					"Invalid scenario " + file.getAbsolutePath()));
			return;
		}
		
		parameters = loader.getParameters();
		controller.batchInitialize();
		controller.runParameterSetters(parameters);
		scenario =  loader.getMyScenario();

		logger.info("Scenario loaded successfully");
	}
	
	public Scenario getScenario() {
		return scenario;
	}
	
	public DataSetManager getDataSetManager() {
		return manager;
	}
	/*
	 * Returns the declared parameter names
	 */
	public String[] getParameterNames() {
		Iterable<String> names= parameters.getSchema().parameterNames(); 
		Vector<String> v = new Vector<String>();
		for(String n : names)
			v.add(n);
		
		return(v.toArray(new String[v.size()]));
	}
	
	public Object getParameter(String s) {
		return parameters.getValue(s);
	}
	
	public String getParameterType(String k) {
		String s = "";
		ParameterSchema schema= null;
		schema= parameters.getSchema().getDetails(k); //
		if(schema != null) {
			s= schema.getType().getName();
		}
		return s; 
	}
	
	public String getParameterAsString(String s) {
		return (String) parameters.getValueAsString(s);
	}
	
	public Number getParameterAsNumber(String s) {
		return (Number) parameters.getValue(s);
	}
	
	public double getParameterAsDouble(String s) {
		return ((Number) parameters.getValue(s)).doubleValue();
	}
	
	public boolean getParameterAsBoolean(String s) {
		return ((Boolean) parameters.getValue(s)).booleanValue();
	}

	public void setParameter(String s, String v) {
		parameters.setValue(s,v);
	}
	
	public void setParameter(String s, Number v) {
		parameters.setValue(s,v);
	}
	
	public void setParameter(String s, Boolean v) {
		parameters.setValue(s,v);
	}
	
	public void endAt(double v) {
		endAt = v;
	}
	
	public void endAt(Number v) {
		endAt = v;
	}
	
	public Number endAt() {
		return endAt;
	}
	
	// Returns the context id
	public String getId() {
		String id = "";
		if(scenario != null) {
			repast.simphony.scenario.data.ContextData c = scenario.getContext();
			if(c != null) {
				id = c.getId();
			}
		}
		return id;
	}
	
	@Override
	public void execute(RunState arg0) {
		// TODO Auto-generated method stub
	}
	
	
	// Initialize the execution
	public void batchInitialize(){
		controller.runInitialize(parameters);
		schedule = RunState.getInstance().getScheduleRegistry().getModelSchedule();
	}
	
	public void cleanUpRun() {
		controller.runCleanup();
	}
	
	public void cleanUpBatch(){
		controller.batchCleanup();
	}
	
	// returns the number of model actions on the schedule
	public int getModelActionCount() {
		return schedule.getModelActionCount();
	}

	// returns the number of non-model actions on the schedule
	public int getActionCount() {
		return schedule.getActionCount();
	}
		
	public void setFinishing(boolean b) {
		schedule.setFinishing(b);
	}

	// returns the tick count of the next scheduled item
	public double getNextScheduledTime() {
		return ((Schedule)RunEnvironment.getInstance().getCurrentSchedule()).peekNextAction().getNextTime();
	}
	
	// stop the schedule
	public void stop(){
		if(schedule != null)
			schedule.executeEndActions();
	}
		
	// Execute model's scheduled tasks
	public void step(){
		schedule.execute();
	}
		
	/*
	 * The model scheduler 
	 */
	public void RunModel() {
		if(runcount > 1) {
			// cleaning all storage but header and first row
			(ModelOutputFactory.getModelOutputStorage()).softReset();
		}
			
		batchInitialize();  // initialize the run
		
		// Loop through the simulated time 
		while (getNextScheduledTime() < endAt().doubleValue()) { 
			if (getActionCount() == 0 || getModelActionCount() == 0) {
				setFinishing(true);
			}
			step();
		}

		stop();          // execute any actions scheduled at run end
		cleanUpRun();
		runcount++;
	}
	
	public String[] GetMockModelOutput() {
		Vector<String> rows = new Vector<String>();
		rows.add("run,random_seed,Time,Plasmid,R,D,T,G(R),G(D),G(T),gamma0(D),gamma0(T),Horizontal(D),Horizontal(T),R0(D),R0(T),(Experimental),(Simulated)");
		rows.add("1,12345,1,pABC,100,200,2,42,43,55,0.5,0.2,0.1,0.20,1,1,2,2");
		rows.add("1,12345,2,pABC,100,200,2,42,43,55,0.5,0.2,0.1,0.20,1,1,2,2");
		return ((String[]) rows.toArray((new String[rows.size()])));
	}
	
	public String[] GetModelOutput() {
		return(output.getModelOutput());
	}

}
