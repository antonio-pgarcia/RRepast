package org.haldane.rrepast;

import java.util.Vector;

/**
 * This file is part of the RRepast package - R/Repast interface API
 * 
 * (C)2015 Antonio Prestes Garcia <@> 
 * For license terms see DESCRIPTION and/or LICENSE
 *
 * ModelOutputFactory.java
 * This class returns a singleton instance of {@link ModelOutput}
 * 
 * @author Antonio Prestes García
 * $Id$
 */
public class ModelOutputFactory {
	public static ModelOutput getModelOutputStorage() {
		return ModelOutputImpl.getInstance();
	}
}

class ModelOutputImpl implements ModelOutput {
	private String dataSet = "ds::datasetname";
	private Vector<String> storage= new Vector<String>();
	private static ModelOutputImpl instance= new ModelOutputImpl(); 

	private ModelOutputImpl() {
	}
	
	public static ModelOutputImpl getInstance() {
		return instance;
	}

	@Override
	public Vector<String> getStorage() {
		// TODO Auto-generated method stub
		return storage;
	}

	@Override
	public String[] getModelOutput() {
		return(storage.toArray(new String[storage.size()]));
	}

	@Override
	public String getDataSet() {
		return dataSet;
	}

	@Override
	public void setDataSet(String s) {
		dataSet = s;
	}

	@Override
	public void softReset() {
		String header = storage.get(0);
		storage.clear();
		storage.add(header);
	}
	
}