package com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.species;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the size of a species and its units.
 * 
 * @author msff2
 *
 */
public class SizeParser extends CtmlConverter implements ISizeParser{
	/**
	 * Parses the size of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the size property in a species.
		parseSize(qName, attributes);
		// Calls the method that checks if the size units 
		// of a species is available in the CTML file which 
		// is being processed.
		parseSizeUnits(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the size tag within the scope of
	 * a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseSize(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesSize())) {
			speciesSizeParseStatus.setSpeciesSize(true);
		}
	}
	
	/**
	 * Reads the value of the size units of a species 
	 * which is being parsed.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseSizeUnits(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesSize())) {
			String units = attributes.getValue(appConfigCtml.getCtmlSpeciesSizeUnits());
			if (units != null) {
				speciesSize.setUnits(units);
				speciesSizeParseStatus.setSpeciesSizeUnits(true);
				speciesSizeParseStatus.setSpeciesSize(true);
			}
		}
	}

}
