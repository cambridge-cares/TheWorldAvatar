package com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.species;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the comment about thermo(dynamic) 
 * properties of a species.
 * 
 * @author msff2
 *
 */
public class SpeciesThermoParser extends CtmlConverter implements ISpeciesThermoParser{
	/**
	 * Parses the thermo(dynamic) properties of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of thermo(dynamic)
		// properties e.g. NASA coefficients in a species.
		parseThermo(qName, attributes);
		// Calls the method that checks if the comment about 
		// thermodynamic property of a species is available 
		// in the CTML file being processed. 
		parseThermoComment(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the thermo tag within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseThermo(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesThermo())) {
				speciesThermoParseStatus.setSpeciesThermo(true);
			}
		}
	}

	/**
	 * Checks the appearance of the comment tag under the thermo tag
	 * within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseThermoComment(String qName, Attributes attributes) {
		if (speciesThermoParseStatus.isSpeciesThermo()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesThermo())) {
				speciesThermoParseStatus.setThermoComment(true);
			}
		}
	}
}
