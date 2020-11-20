package com.cmclinnovations.ontochem.model.reference;

import java.util.List;

import com.cmclinnovations.ontochem.model.exception.OntoException;

/**
 * Declares the methods whose implementation extracts content from a set of DOIs.
 * 
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public interface IDoi {
	public void extractContent() throws OntoException;
	public void add(List<String> doi);
}
