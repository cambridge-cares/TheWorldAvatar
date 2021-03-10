package org.cam.ceb.como.nist.converter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.cam.ceb.como.nist.model.exception.OntoSpeciesException;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

/**
 * This is an abstract class provides the list of methods that needs 
 * to be implemented to convert NIST Species to OWL.
 * 
 * @author msff2
 *
 */
public abstract class OntoSpecies {
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the OntoKin class.
	private Logger logger = LoggerFactory.getLogger(OntoSpecies.class);
	
	public abstract void convert() throws OntoSpeciesException, OWLOntologyCreationException;
	
	/**
	 * The default constructor of OntoSpecies.
	 */
	public OntoSpecies(){
	}
	
	/**
	 * A constructor of OntoSpecies class to log which code is going to run.
	 */
	public OntoSpecies(String codeStatus){
		logger.info(codeStatus);
	}

	/**
	 * Converts an OntoSpecies object into a string.
	 */
	@Override
	public String toString() {
		return "OntoSpecies [logger=" + logger + ", getClass()=" + getClass() + ", hashCode()=" + hashCode()
				+ ", toString()=" + super.toString() + "]";
	}
}
