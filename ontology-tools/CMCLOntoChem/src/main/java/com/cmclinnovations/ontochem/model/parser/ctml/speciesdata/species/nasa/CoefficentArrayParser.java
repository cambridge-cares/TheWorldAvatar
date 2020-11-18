package com.cmclinnovations.ontochem.model.parser.ctml.speciesdata.species.nasa;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * This class parses the NASA Polynomial Coefficients of a species 
 * and their metadata.
 * 
 * @author msff2
 *
 */
public class CoefficentArrayParser extends CtmlConverter implements ICoefficentArrayParser{
	/**
	 * Calls the methods that parse NASA Polynomial Coefficients 
	 * of a species and their metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the presence of
		// the NASA Polynomial Coefficients Array for a species.
		parseCoefficientArray(qName, attributes);
		// Calls the method that parses the name of the NASA Polynomial 
		// Coefficients Array.
		parseCoeffArrayName(qName, attributes);
		// Calls the method that parses the size of the NASA Polynomial
		// Coefficients Array.
		parseCoeffArraySize(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the floatArray tag within the scope 
	 * of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseCoefficientArray(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlNasaPolCoeffArray())) {
			coeffArrayParseStatus.setFloatArray(true);
		}
	}
	
	/**
	 * Reads the name of the NASA Polynomial Coefficients' array of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseCoeffArrayName(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlNasaPolCoeffArray())) {
			String name = attributes.getValue(appConfigCtml.getCtmlNasaPolCoeffArrayName());
			if (coeffArray != null) {
				coeffArray.setName(name);
				coeffArrayParseStatus.setName(true);
				coeffArrayParseStatus.setFloatArray(true);
			}
		}
	}
	
	/**
	 * Reads the size of the NASA Polynomial Coefficients' array of a species. 
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseCoeffArraySize(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlNasaPolCoeffArray())) {
			String size = attributes.getValue(appConfigCtml.getCtmlNasaPolCoeffArraySize());
			if (coeffArray != null) {
				coeffArray.setSize(size);
				coeffArrayParseStatus.setSize(true);
				coeffArrayParseStatus.setFloatArray(true);
			}
		}
	}
}
