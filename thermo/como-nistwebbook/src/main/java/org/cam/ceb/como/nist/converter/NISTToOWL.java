package org.cam.ceb.como.nist.converter;

import org.cam.ceb.como.nist.model.exception.OntoSpeciesException;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;

/**
 * This class contains the methods that are needed to run the NIST to OWL</br>
 * converter. NIST data is made available to the converter as in-memory</br>
 * objects in Java. If the user requests to convert from NIST to OWL, this</br>
 * class gets instantiated.
 * 
 * @author msff2
 *
 */
public class NISTToOWL extends OntoSpecies{
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the CompChemToOWL class.
	private static Logger logger = org.slf4j.LoggerFactory.getLogger(NISTToOWL.class);
	// Keeps the source HTML file path where all NIST data are available</br>
	// as HTML files.
	private String sourceHTMLFilePath;
	// Keeps the source species structure file path where all SDF and MOL</br>
	// files are available</br>
	private String sourceStructureFilePath;
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
	public NISTToOWL(String sourceHTMLFilePath, String sourceStructureFilePath, String destinationFilePath) {
		super("NIST Species to OWL converter started running...");
		this.sourceHTMLFilePath = sourceHTMLFilePath;
		this.sourceStructureFilePath = sourceStructureFilePath;
		this.destinationFilePath = destinationFilePath;
	}
	
	/**
	 * This method calls the convert method which belongs to the interface
	 * called INISTConverter.
	 */
	public void convert() throws OntoSpeciesException, OWLOntologyCreationException{
		try{
		INISTConverter iNISTConverter = new NISTConverter();
		iNISTConverter.convert(this.sourceHTMLFilePath, this.sourceStructureFilePath, this.destinationFilePath);
		logger.info("NIST Species to OWL conversion FINISHED.");
		}catch(Exception e){
			e.printStackTrace();
		}
	}

	/**
	 * @return String Converts an object of NISTToOWL into a string. 
	 */
	@Override
	public String toString() {
		return "NISTToOWL [sourceHTMLFilePath=" + sourceHTMLFilePath + ", sourceStructureFilePath="
				+ sourceStructureFilePath + ", destinationFilePath=" + destinationFilePath + ", toString()="
				+ super.toString() + ", getClass()=" + getClass() + ", hashCode()=" + hashCode() + "]";
	}
}
