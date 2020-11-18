package com.cmclinnovations.ontokin.model.parser.ctml.phase.thermo;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the site density of a CTML thermo(dynamic 
 * property) element and its units.
 * 
 * @author msff2
 *
 */
public class SiteDensityParser extends CtmlConverter implements ISiteDensityParser{
	/**
	 * Parses the site density metadata of a phase.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the site density properties in a phase.
		parseSiteDensity(qName, attributes);
		// Calls the method that checks if the site density thermo property 
		// of a phase is available in the CTML file which is being processed.
		parseSiteDensityUnits(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the site_density tag.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseSiteDensity(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseSiteDensity())) {
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
	private void parseSiteDensityUnits(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseSiteDensity())) {
			String units = attributes.getValue(appConfigCtml.getCtmlPhaseSiteDensityUnits());
			if (units != null) {
				siteDensity.setUnits(units);
				siteDensityParseStatus.setUnits(true);
				siteDensityParseStatus.setSiteDensity(true);
			}
		}
	}

}
