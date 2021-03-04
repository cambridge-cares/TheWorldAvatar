package com.cmclinnovations.prime.species.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

public abstract class OntoPrimeSpecies {
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the OntoChemExp class.
	private Logger logger = LoggerFactory.getLogger(OntoPrimeSpecies.class);
	
	public abstract void convert(long instanceSerialID) throws OWLOntologyCreationException, OntoPrimeSpeciesException;
	
	public OntoPrimeSpecies() {	
	}
	
	public OntoPrimeSpecies(String codeStatus) {
		logger.info(codeStatus);
	}
	
	@Override
	public String toString() {
		return "OntoPrimeSpecies [logger=" + logger + ", getClass()=" + getClass() + ", hashCode()=" + hashCode()
				+ ", toString()=" + super.toString() + "]";
	}
}
