package com.cmclinnovations.prime.species.controller;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;

import com.cmclinnovations.prime.species.model.converter.species.IPrimeSpeciesConverter;
import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;

public class PrimeSpeciesToOWL extends OntoPrimeSpecies {
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the PrimeToOwl class.
	private static Logger logger = org.slf4j.LoggerFactory.getLogger(PrimeSpeciesToOWL.class);
	// Keeps the list of input PrIMe file(s), which are requested to be 
	// converted into OWL. Each item in the list consists of an absolute 
	// path to a PrIMe file including the file name.
	private ArrayList<String> sourceFiles;
	// Keeps the absolute path where the generated OWL file(s) will be stored.
	private String destinationFilePath;
	
	/**
	 * Parameterised Constructor of the PrimeToOwl class.
	 * 
	 * @param sourceFilePath String represents the path to PrIMe file(s), 
	 * which will be converted into OWL.
	 * @param destinationFilePath String represents the path to OWL file(s),
	 * which will be generated from PrIMe.  
	 */
	public PrimeSpeciesToOWL(ArrayList<String> sourceFiles, String destinationFilePath) {
		super("PrIMeSpecies to OWL converter started running...");
		this.sourceFiles = sourceFiles;
		this.destinationFilePath = destinationFilePath;
	}
	
	/**
	 * This method calls the convert method which belongs to the interface
	 * called IPrimeConverter.
	 */
	public void convert(long instanceSerialID) throws OWLOntologyCreationException, OntoPrimeSpeciesException {
		try {
			IPrimeSpeciesConverter iPrimeSpeciesConverter = new PrimeSpeciesConverter();
			iPrimeSpeciesConverter.convert(this.sourceFiles, this.destinationFilePath, instanceSerialID);
			logger.info("PrIMeSpecies to OWL conversion FINISHED.");
		}catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * @return String Converts an object of PrimeToOwl into a string. 
	 */
	@Override
	public String toString() {
		return "PrimeSpeciesToOwl [sourceFiles=" + sourceFiles + ", destinationFilePath=" + destinationFilePath
				+ ", toString()=" + super.toString() + ", getClass()=" + getClass() + ", hashCode()=" + hashCode()
				+ "]";
	}
}
