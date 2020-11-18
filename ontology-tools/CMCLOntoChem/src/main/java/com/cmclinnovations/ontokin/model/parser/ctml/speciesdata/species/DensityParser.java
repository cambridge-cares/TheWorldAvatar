package com.cmclinnovations.ontokin.model.parser.ctml.speciesdata.species;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the density of a species and its units.
 * 
 * @author msff2
 *
 */
public class DensityParser extends CtmlConverter implements IDensityParser{
	/**
	 * Parses the density of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the density property in a species.
		parseDensity(qName, attributes);
		// Calls the method that checks if the density units 
		// of a species is available in the CTML file which 
		// is being processed.
		parseDensityUnits(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the density tag within the scope of
	 * a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseDensity(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDensity())) {
			siteDensityParseStatus.setSiteDensity(true);
		}
	}
	
	/**
	 * Reads the value of the site density units of a phase 
	 * which is being parsed.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseDensityUnits(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDensity())) {
			String units = attributes.getValue(appConfigCtml.getCtmlSpeciesDensityUnits());
			if (speciesDensity != null) {
				speciesDensity.setUnits(units);
				densityParseStatus.setSpeciesDensityUnits(true);
				densityParseStatus.setSpeciesDensity(true);
			}
		}
	}

}
