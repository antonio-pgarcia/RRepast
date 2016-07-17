package org.haldane.rrepast;

import java.util.Vector;

/**
 * This file is part of the RRepast package - R/Repast interface API
 * 
 * (C)2015 Antonio Prestes Garcia <@> 
 * For license terms see DESCRIPTION and/or LICENSE
 * 
 * @author Antonio Prestes García
 * $Id$
 */
public interface ModelOutput {
	public String getDataSet();
	public void setDataSet(String s);
	public Vector<String> getStorage();
	public String[] getModelOutput();
	public void softReset();
}
