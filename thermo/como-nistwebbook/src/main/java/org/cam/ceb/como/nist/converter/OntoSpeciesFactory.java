package org.cam.ceb.como.nist.converter;

import java.io.File;

/**
 * A factory class that instantiates the NISTToOWL class.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class OntoSpeciesFactory {
	/**
	 * 
	 * @param sourceHTMLFilePath The path to the input HTML file(s).
	 * @param sourceStructureFilePath The path to the input structure file(s).
	 * @param destinationFilePath The path to the output OWL file(s).
	 * @return An instance of NISTToOWL. 
	 */
	public static OntoSpecies getOntoSpecies(String sourceHTMLFilePath, String sourceStructureFilePath, String destinationFilePath) {
		if (new File(sourceHTMLFilePath).exists() && new File(sourceStructureFilePath).exists() && new File(destinationFilePath).exists())
			return new NISTToOWL(sourceHTMLFilePath, sourceStructureFilePath, destinationFilePath);
		return null;
	}
}
