package com.cmclinnovations.ontochem.model.parser.ctml.speciesdata.species.nasa;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * This class parses the NASA Polynomial Coefficients metadata 
 * for a species. 
 * 
 * @author msff2
 *
 */
public class NASAPolynomialParser extends CtmlConverter implements INASAPolynomialParser{
	/**
	 * Parses the density of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the presence of
		// the NASA Polynomial Coefficients for a species.
		parseNASAPolynomial(qName, attributes);
		// Calls the method that parses the value of the maximum temperature
		// above which NASA Polynomial Coefficients are invalid.
		parseNASATmax(qName, attributes);
		// Calls the method that parses the value of the maximum temperature
		// below which NASA Polynomial Coefficients are invalid.
		parseNASATmin(qName, attributes);
		// Calls the method that parses the value of pressure
		// at which NASA Polynomial Coefficients were computed.
		parseNASAP0(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the NASA tag within the scope of a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseNASAPolynomial(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesThermoNasa())) {
			nasaPolyParseStatus.setNASA(true);
		}
	}
	
	/**
	 * Reads the value of the maximum temperature above which NASA 
	 * Polynomial Coefficients are not valid. 
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseNASATmax(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesThermoNasa())) {
			String Tmax = attributes.getValue(appConfigCtml.getCtmlNasaTMax());
			if (nasa != null) {
				nasa.setTmax(Tmax);
				nasaPolyParseStatus.setTmax(true);
				nasaPolyParseStatus.setNASA(true);
			}
		}
	}
	
	/**
	 * Reads the value of the minimum temperature below which NASA 
	 * Polynomial Coefficients are not valid. 
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseNASATmin(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesThermoNasa())) {
			String Tmin = attributes.getValue(appConfigCtml.getCtmlNasaTMin());
			if (nasa != null) {
				nasa.setTmin(Tmin);
				nasaPolyParseStatus.setTmin(true);
				nasaPolyParseStatus.setNASA(true);
			}
		}
	}
	
	/**
	 * Reads the value of the pressure at which NASA
	 * Polynomial Coefficients were computed. 
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseNASAP0(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesThermoNasa())) {
			String P0 = attributes.getValue(appConfigCtml.getCtmlNasaP0());
			if (nasa != null) {
				nasa.setP0(P0);
				nasaPolyParseStatus.setP0(true);
				nasaPolyParseStatus.setNASA(true);
			}
		}
	}

}
