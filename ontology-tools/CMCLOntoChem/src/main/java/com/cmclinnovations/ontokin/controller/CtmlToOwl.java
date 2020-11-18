package com.cmclinnovations.ontokin.controller;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontokin.model.converter.ctml.ICtmlConverter;
import com.cmclinnovations.ontokin.model.exception.OntoException;

/**
 * This class contains the methods that are needed to run the CTML to OWL
 * converter.
 * </br>
 * Based on the request of a user, OntoKinFactory class may instantiate this class.
 * </br>
 * If the user requests to convert from CTML to OWL, this class gets instantiated.
 * 
 * @author msff2
 *
 */
public class CtmlToOwl extends OntoKin{
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the CtmlToOwl class.
	private static Logger logger = org.slf4j.LoggerFactory.getLogger(CtmlToOwl.class);
	// Keeps the list of input CTML file(s), which are requested to be 
	// converted into OWL. Each item in the list consists of an absolute 
	// path to a CTML file including the file name.
	private ArrayList<String> sourceFiles;
	// Keeps the absolute path where the generated OWL file(s) will be stored.
	private String destinationFilePath;

	/**
	 * Parameterised Constructor of the CtmlToOwl class.
	 * 
	 * @param sourceFilePath String represents the path to CTML file(s), 
	 * which will be converted into OWL.
	 * @param destinationFilePath String represents the path to OWL file(s),
	 * which will be generated from CTML.  
	 */
	public CtmlToOwl(ArrayList<String> sourceFiles, String destinationFilePath) {
		super("CTML to OWL converter started running...");
		this.sourceFiles = sourceFiles;
		this.destinationFilePath = destinationFilePath;
	}
	
	/**
	 * This method calls the convert method which belongs to the interface
	 * called ICtmlConverter.
	 */
	public void convert() throws OWLOntologyCreationException, OntoException{
		try{
		ICtmlConverter iCtmlConverter = new CtmlConverter();
		iCtmlConverter.convert(this.sourceFiles, this.destinationFilePath);
		logger.info("CTML to OWL conversion FINISHED.");
		}catch(Exception e){
			e.printStackTrace();
		}
	}

	/**
	 * @return String Converts an object of CtmlToOwl into a string. 
	 */
	@Override
	public String toString() {
		return "CtmlToOwl [sourceFiles=" + sourceFiles + ", destinationFilePath=" + destinationFilePath
				+ ", getClass()=" + getClass() + ", hashCode()=" + hashCode() + ", toString()=" + super.toString()
				+ "]";
	}
}
