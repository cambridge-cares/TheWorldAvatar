package com.cmclinnovations.ontochem.model.converter.json;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import com.cmclinnovations.ontochem.controller.OntoKin;
import com.cmclinnovations.ontochem.model.exception.OntoException;

/**
 * This class contains the methods that are needed to run the CompChem (provided
 * in JSON) to OWL converter.
 * </br>
 * If the user requests to convert from CompChem to OWL, this class gets instantiated.
 * 
 * @author msff2
 *
 */

public class CompChemToOWL extends OntoKin{
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the CompChemToOWL class.
	private static Logger logger = org.slf4j.LoggerFactory.getLogger(CompChemToOWL.class);
	// Keeps the list of input CompChem file(s), which are requested to be 
	// converted into OWL. Each item in the list consists of an absolute 
	// path to a CompChem file including the file name.
	private ArrayList<String> sourceFiles;
	// Keeps the absolute path where the generated OWL file(s) will be stored.
	private String destinationFilePath;

	/**
	 * Parameterised Constructor of the CompChemToOWL class.
	 * 
	 * @param sourceFilePath String represents the path to CompChem file(s), 
	 * which will be converted into OWL.
	 * @param destinationFilePath String represents the path to OWL file(s),
	 * which will be generated from CompChem.  
	 */
	public CompChemToOWL(ArrayList<String> sourceFiles, String destinationFilePath) {
		super("CompChem to OWL converter started running...");
		this.sourceFiles = sourceFiles;
		this.destinationFilePath = destinationFilePath;
	}
	
	/**
	 * This method calls the convert method which belongs to the interface
	 * called ICompChemConverter.
	 */
	public void convert() throws OWLOntologyCreationException, OntoException{
		try{
		ICompChemConverter iCompChemConverter = new CompChemConverter();
		iCompChemConverter.convert(this.sourceFiles, this.destinationFilePath);
		logger.info("CompChem to OWL conversion FINISHED.");
		}catch(Exception e){
			e.printStackTrace();
		}
	}

	/**
	 * @return String Converts an object of CompChemToOwl into a string. 
	 */
	@Override
	public String toString() {
		return "CompChemToOWL [sourceFiles=" + sourceFiles + ", destinationFilePath=" + destinationFilePath
				+ ", toString()=" + super.toString() + ", getClass()=" + getClass() + ", hashCode()=" + hashCode()
				+ "]";
	}
}
