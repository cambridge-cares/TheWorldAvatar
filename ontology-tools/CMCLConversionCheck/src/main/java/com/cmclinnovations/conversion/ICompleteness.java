package com.cmclinnovations.conversion;
/**
 * Declares the method whose implementation:</br>
 * 1. Checks the difference between a source file and the one generated</br>
 * from this</br>
 * 2. Produces a report to describe the difference between them.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public interface ICompleteness {
	/**
	 * 
	 * @param reportFile
	 * @param sourceFile
	 * @param generatedFile
	 */
	public void reportDifference(String reportFile, String sourceFile, String generatedFile);
}
