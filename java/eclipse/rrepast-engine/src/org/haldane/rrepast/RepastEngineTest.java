package org.haldane.rrepast;

/**
 * This file is part of the RRepast package - R/Repast interface API
 * 
 * (C)2015 Antonio Prestes Garcia <@> 
 * For license terms see DESCRIPTION and/or LICENSE
 *
 * RepastEngineTest.java
 * This is a simple example of use for testing the RRpast API without R 
 *    
 * 
 * @author Antonio Prestes García
 * $Id$
 */
public class RepastEngineTest {
	private static String scenario = "c:/usr/svn-repository/repast/workspace/BactoSim(HaldaneEngine-1.0)/BactoSim(HaldaneEngine-1.0).rs";
	//private static String scenario = "C:/usr/models/BactoSim(HaldaneEngine-1.0)/BactoSim(HaldaneEngine-1.0)/BactoSim(HaldaneEngine-1.0).rs";
	
	public static void main(String[] args) {
		System.setProperty("rrepast.modelinitializer", "org.haldane.rrepast.CustomInitializer");
		
		//RepastEngineLogger.setLevelInfo();
		RepastEngine engine = new RepastEngine();
		
		engine.ModelDataSet("ds::Output");
		engine.LoadModel(scenario);
		
		engine.endAt(10);
		engine.RunModel();
		System.out.println(engine.GetModelOutput()[0]);
		
	}
}
