package org.haldane.rrepast;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This file is part of the RRepast package - R/Repast interface API
 * 
 * (C)2015 Antonio Prestes Garcia <@> 
 * For license terms see DESCRIPTION and/or LICENSE
 *
 * RepastEngineLogger.java
 * This class is an implementation of a centralized logger controller for RRepast
 * 
 * @author Antonio Prestes García
 * $Id$
 */

public class RepastEngineLogger {
	private final static String LOGGER= "rrepast";
	private static final Logger logger = Logger.getLogger(LOGGER);
	static {
		System.setProperty("java.util.logging.SimpleFormatter.format","%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS %4$-6s %2$s %5$s%6$s%n");
		setLevelWarning();
	}
	
	public static Logger getLogger() {
		return logger;
	}
	
	public static void setLevelSevere() {
		logger.setLevel(Level.SEVERE);
	}
	
	public static void setLevelWarning() {
		logger.setLevel(Level.WARNING);
	}
	
	public static void setLevelInfo() {
		logger.setLevel(Level.INFO);
	}
	
	public static void setLevelFine() {
		logger.setLevel(Level.FINE);
	}
	
	

}

