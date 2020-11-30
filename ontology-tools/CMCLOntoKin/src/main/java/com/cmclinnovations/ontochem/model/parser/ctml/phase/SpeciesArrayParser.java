package com.cmclinnovations.ontochem.model.parser.ctml.phase;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * This class parses the species array metadata of a CTML phase element.
 * 
 * @author msff2
 *
 */
public class SpeciesArrayParser  extends CtmlConverter implements ISpeciesArrayParser{

	/**
	 * Parses the CTML species array metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the element array in a phase
		parseSpeciesArray(qName, attributes);
		// Calls the method that checks if the element array of a phase
		// is available in the CTML file which is being processed
		parseSpeciesArrayDataSrc(qName, attributes);
	}

	/**
	 * Checks the appearance of the species array in a phase.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseSpeciesArray(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseSpeciesArray())) {
			speciesArrayParseStatus.setSpeciesArray(true);
		}
	}

	/**
	 * Parses and extracts the data source of the species array from
	 * a phase if available.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseSpeciesArrayDataSrc(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseSpeciesArray())) {
			String dataSrc = attributes.getValue(appConfigCtml.getCtmlPhaseSpeciesArrayDataSrc());
			if (dataSrc != null) {
				speciesArray.setDatasrc(dataSrc);
				speciesArrayParseStatus.setSpeciesArray(true);
				speciesArrayParseStatus.setSpeciesDataSrc(true);
			}
		}
	}
}
