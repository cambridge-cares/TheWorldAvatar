package com.cmclinnovations.ontochem.model.reference;

import java.util.List;

/**
 * Declares the method whose implementation extracts DOIs from a piece of text.
 * 
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public interface IDoiParser {
	public List<String> extractDoi(String text);
}
