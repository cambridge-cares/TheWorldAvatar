package com.cmclinnovations.ontokin.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.cmclinnovations.ontokin.model.exception.OntoException;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

/**
 * This is an abstract class provides the list of methods that needs 
 * to be implemented to convert CTML to OWL, and vice versa.
 * 
 * @author msff2
 *
 */
public abstract class OntoKin {
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the OntoKin class.
	private Logger logger = LoggerFactory.getLogger(OntoKin.class);
	
	public abstract void convert() throws OWLOntologyCreationException, OntoException;
	
	/**
	 * The default constructor of OntoKin.
	 */
	public OntoKin(){
	}
	
	/**
	 * A constructor of OntoKin class to log which code is going to run.
	 */
	public OntoKin(String codeStatus){
		logger.info(codeStatus);
	}

	/**
	 * Converts an OntoKin object into a string.
	 */
	@Override
	public String toString() {
		return "OntoKin [logger=" + logger + ", getClass()=" + getClass() + ", hashCode()=" + hashCode()
				+ ", toString()=" + super.toString() + "]";
	}
}
