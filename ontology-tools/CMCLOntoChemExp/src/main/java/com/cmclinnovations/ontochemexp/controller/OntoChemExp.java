package com.cmclinnovations.ontochemexp.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

/**
 * This is an abstract class provides the list of methods that need 
 * to be implemented to convert PrIMe Experiment data to OWL.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public abstract class OntoChemExp {
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the OntoChemExp class.
	private Logger logger = LoggerFactory.getLogger(OntoChemExp.class);
	
	public abstract void convert(long instanceSerialID) throws OWLOntologyCreationException, OntoChemExpException;
	
	/**
	 * The default constructor of OntoChemExp.
	 */
	public OntoChemExp(){
	}
	
	/**
	 * A constructor of OntoChemExp class to log which code is going to run.
	 */
	public OntoChemExp(String codeStatus){
		logger.info(codeStatus);
	}

	/**
	 * Converts an OntoChemExp object into a string.
	 */
	@Override
	public String toString() {
		return "OntoChemExp [logger=" + logger + ", getClass()=" + getClass() + ", hashCode()=" + hashCode()
				+ ", toString()=" + super.toString() + "]";
	}
}
