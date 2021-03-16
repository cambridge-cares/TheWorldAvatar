package com.cmclinnovations.ontochemexp.controller;

import java.util.ArrayList;
import java.util.List;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.ontochemexp.model.converter.owl.IOwlConverter;
import com.cmclinnovations.ontochemexp.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

/**
 * This class contains the methods that are needed to run the OWL to PrIMe
 * converter.
 * </br>
 * Based on the request of a user, OntoChemExpFactory class may instantiate this class.
 * </br>
 * If the user requests to convert from OWL to PrIMe, this class gets instantiated.
 * 
 * @author jb2197
 *
 */
public class OwlToPrime extends OntoChemExp {
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the OwlToPrime class.
	private static Logger logger = LoggerFactory.getLogger(OwlToPrime.class);
	// Keeps the list of input OWL file(s), which are requested to be 
	// converted into PrIMe. Each item in the list consists of an absolute 
	// path to an OWL file including the file name.
	private ArrayList<String> sourceFiles;
	// Keeps the absolute path where the generated PrIMe file(s) will be stored.
	private String destinationFilePath;
	
	/**
	 * Parameterised constructor of the OwlToPrime class.
	 * 
	 * @param sourceFiles ArrayList<String> represent the path+file names
	 * of the OWL files which are being converted into PrIMe.
	 * </br>
	 * @param destinationFilePath String represents the path to the 
	 * PrIMe files, which will be generated from OWL.
	 */
	public OwlToPrime(List<String> sourceFiles, String destinationFilePath) {
		super("OWL to PrIMe converter started running...");
		this.sourceFiles = (ArrayList<String>)sourceFiles;
		this.destinationFilePath = destinationFilePath;
	}
	
	/**
	 * This method calls the convert method which belongs to the interface
	 * called IOwlConverter.
	 */
	public void convert(long instanceSerialID) throws OWLOntologyCreationException, OntoChemExpException {
		try {
			IOwlConverter iOwlConverter = new OwlConverter();
			iOwlConverter.convert(this.sourceFiles, this.destinationFilePath, instanceSerialID);
			logger.info("OWL to PrIMe conversion FINISHED.");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	@Override
	public String toString() {
		return "OwlToPrime [sourceFiles=" + sourceFiles + ", destinationFilePath=" + destinationFilePath
				+ ", toString()=" + super.toString() + ", getClass()=" + getClass() + ", hashCode()" + hashCode()
				+ "]";
	}
}
