package org.haldane.rrepast;

import java.util.List;
import java.util.Vector;

import repast.simphony.data2.DataSink;

/**
 * This file is part of the RRepast package - R/Repast interface API
 * 
 * (C)2015 Antonio Prestes Garcia <@> 
 * For license terms see DESCRIPTION and/or LICENSE
 *
 * OutputWrapperDataSink.java
 * This class provides a wrapper for storing the output of Repast Model's 
 * datasets. The output from the selected dataset is stored into a singleton
 * vector {@link ModelOutputFactory} as CSV.   
 * 
 * @author Antonio Prestes García
 * $Id$
 */
public class OutputWrapperDataSink implements DataSink {
	private Vector<String> v =  (ModelOutputFactory.getModelOutputStorage()).getStorage();
	private boolean firstrow = true;
	private String header = "";
	private String row = "";
	

	@Override
	public void append(String arg0, Object arg1) {
		if(firstrow) {	
			header=  arg0 + "," + header;
		}
		if(arg1 != null) {
			row= arg1 + "," + row;
		} else {
			row= "NA" + "," + row;
		}
	}
	
	@Override
	public void flush() {
		// TODO Auto-generated method stub
	}

	@Override
	public void open(List<String> arg0) {
		// Every time the open method is called we clean the storage space
		v.clear();
	}
	
	@Override
	public void close() {
		// TODO Auto-generated method stub
	}

	@Override
	public void recordEnded() {
		// TODO Auto-generated method stub
	}

	@Override
	public void rowEnded() {

		if(firstrow) {
			header = header.replaceAll(",$", "");
			v.add(header);
			firstrow = false;
		}
		row = row.replaceAll(",$", "");
		v.add(row);
		row = "";
	}

	@Override
	public void rowStarted() {
		// TODO Auto-generated method stub
	}

}


