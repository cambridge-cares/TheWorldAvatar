package com.cmclinnovations.ontokin.controller;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;

import com.cmclinnovations.ontokin.model.converter.owl.IOwlConverter;
import com.cmclinnovations.ontokin.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontokin.model.exception.OntoException;

/**
 * This class contains the methods that are needed to run the OWL to CTML
 * converter.
 * </br>
 * Based on the request of a user, OntoKinFactory class may instantiate this class.
 * </br>
 * If the user requests to convert from OWL to CTML, this class gets instantiated.
 * 
 * @author msff2
 *
 */
public class OwlToCtml extends OntoKin{
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the OwlToCtml class.
	private static Logger logger = org.slf4j.LoggerFactory.getLogger(OwlToCtml.class);
	// Keeps the list of input OWL file(s), which are requested to be 
	// converted into CTML. Each item in the list consists of an absolute 
	// path to an OWL file including the file name.
	private ArrayList<String> sourceFiles;
	// Keeps the absolute path where the generated CTML file(s) will be stored.
	private String destinationFilePath;
	
	/**
	 * Parameterised constructor of the OwlToCtml class.
	 * 
	 * @param sourceFiles ArrayList<String> represent the path+file names
	 * of the OWL files which are being converted into CTML.
	 * </br>
	 * @param destinationFilePath String represents the path to the 
	 * CTML files, which will be generated from OWL.
	 */
	public OwlToCtml(ArrayList<String> sourceFiles, String destinationFilePath) {
		super("OWL to CTML converter started running...");
		this.sourceFiles = sourceFiles;
		this.destinationFilePath = destinationFilePath;
	}

	/**
	 * This method calls the convert method which belongs to the interface
	 * called IOwlConverter.
	 */
	public void convert() throws OWLOntologyCreationException, OntoException {
		try{
		IOwlConverter iOwlConverter = new OwlConverter();
		iOwlConverter.convert(this.sourceFiles, this.destinationFilePath);
		logger.info("OWL to CTML conversion FINISHED.");
		}catch(Exception e){
			e.printStackTrace();
		}
	}

	@Override
	public String toString() {
		return "OwlToCtml [sourceFiles=" + sourceFiles + ", destinationFilePath=" + destinationFilePath
				+ ", toString()=" + super.toString() + ", getClass()=" + getClass() + ", hashCode()=" + hashCode()
				+ "]";
	}
}