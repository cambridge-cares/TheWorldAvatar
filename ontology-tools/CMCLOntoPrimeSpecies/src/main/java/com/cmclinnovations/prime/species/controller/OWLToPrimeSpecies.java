package com.cmclinnovations.prime.species.controller;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.prime.species.model.converter.owl.IOwlConverter;
import com.cmclinnovations.prime.species.model.converter.owl.OwlConverter;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;

public class OWLToPrimeSpecies extends OntoPrimeSpecies {
	private static Logger logger = LoggerFactory.getLogger(OWLToPrimeSpecies.class);
	private ArrayList<String> sourceFiles;
	private String destinationFilePath;
	
	/**
	 * Parameterised constructor of the OwlToPrime class.
	 * 
	 * @param sourceFiles ArrayList<String> represent the path+file names
	 * of the OWL files which are being converted into PrIMeSpecies.
	 * </br>
	 * @param destinationFilePath String represents the path to the 
	 * PrIMe files, which will be generated from OWL.
	 */
	public OWLToPrimeSpecies(ArrayList<String> sourceFiles, String destinationFilePath) {
		super("OWL to PrIMeSpecies converter started running...");
		this.sourceFiles = sourceFiles;
		this.destinationFilePath = destinationFilePath;
	}
	
	/**
	 * This method calls the convert method which belongs to the interface
	 * called IOwlConverter.
	 */
	public void convert(long instanceSerialID) throws OWLOntologyCreationException, OntoPrimeSpeciesException {
		try {
			IOwlConverter iOwlConverter = new OwlConverter();
			iOwlConverter.convert(this.sourceFiles, this.destinationFilePath, instanceSerialID);
			logger.info("OWL to PrIMeSpecies conversion FINISHED.");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	@Override
	public String toString() {
		return "OWLToPrimeSpeices [sourceFiles=" + sourceFiles + ", destinationFilePath=" + destinationFilePath
				+ ", toString()=" + super.toString() + ", getClass()=" + getClass() + ", hashCode()" + hashCode()
				+ "]";
	}
}
