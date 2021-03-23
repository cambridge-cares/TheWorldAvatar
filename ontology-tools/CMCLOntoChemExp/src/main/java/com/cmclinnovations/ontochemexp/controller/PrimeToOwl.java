package com.cmclinnovations.ontochemexp.controller;

import java.util.ArrayList;
import java.util.List;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;

import com.cmclinnovations.ontochemexp.model.converter.prime.IPrimeConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

/**
 * This class contains methods that are needed to run the PrIMe to OWL
 * converter.
 * </br>
 * Based on the request of a user, the OntoChemExpFactory class may instantiate this class.
 * </br>
 * If the user requests to convert from PrIMe to OWL, this class gets instantiated.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class PrimeToOwl extends OntoChemExp{
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the PrimeToOwl class.
	private static Logger logger = org.slf4j.LoggerFactory.getLogger(PrimeToOwl.class);
	// Keeps the list of input PrIMe file(s), which are requested to be 
	// converted into OWL. Each item in the list consists of an absolute 
	// path to a PrIMe file including the file name.
	private ArrayList<String> sourceFiles;
	// Keeps the absolute path where the generated OWL file(s) will be stored.
	private String destinationFilePath;

	/**
	 * Parameterised Constructor of the PrimeToOwl class.
	 * 
	 * @param sourceFiles a list of string representing the paths to a set of PrIMe files, 
	 * which will be converted into OWL.
	 * @param destinationFilePath String represents the path to OWL file(s),
	 * which will be generated from PrIMe.  
	 */
	public PrimeToOwl(List<String> sourceFiles, String destinationFilePath) {
		super("PrIMe to OWL converter started running...");
		this.sourceFiles = (ArrayList<String>)sourceFiles;
		this.destinationFilePath = destinationFilePath;
	}
	
	/**
	 * This method calls the convert method which belongs to the interface
	 * called IPrimeConverter.
	 */
	public void convert(long instanceSerialID) throws OWLOntologyCreationException, OntoChemExpException{
		try{
		IPrimeConverter iPrimeConverter = new PrimeConverter();
		iPrimeConverter.convert(this.sourceFiles, this.destinationFilePath, instanceSerialID);
		logger.info("PrIMe to OWL conversion FINISHED.");
		}catch(Exception e){
			e.printStackTrace();
		}
	}

	/**
	 * @return String Converts an object of PrimeToOwl into a string. 
	 */
	@Override
	public String toString() {
		return "PrimeToOwl [sourceFiles=" + sourceFiles + ", destinationFilePath=" + destinationFilePath
				+ ", toString()=" + super.toString() + ", getClass()=" + getClass() + ", hashCode()=" + hashCode()
				+ "]";
	}
}
