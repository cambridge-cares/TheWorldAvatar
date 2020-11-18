package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;

/**
 * Implements the method that forwards calls to the Experiment parser.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class ExperimentConverter extends PrimeConverter implements IExperimentConverter{
	public void parse(String qName, Attributes attributes){
		// Calls the method that forwards calls to
		// the methods that parse experiment data and metadata.
		iExperimentParser.parse(qName, attributes);
	}
}
