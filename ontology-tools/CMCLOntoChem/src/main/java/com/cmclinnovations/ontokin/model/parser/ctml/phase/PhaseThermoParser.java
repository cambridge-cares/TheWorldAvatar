package com.cmclinnovations.ontokin.model.parser.ctml.phase;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the state of a CTML thermodynamic property element.
 * 
 * @author msff2
 *
 */
public class PhaseThermoParser extends CtmlConverter implements IPhaseThermoParser{
	/**
	 * Parses the CTML reaction array metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the thermo properties in a phase.
		parseThermo(qName, attributes);
		// Calls the method that checks if the thermo properties of a phase
		// is available in the CTML file which is being processed.
		parseThermoModel(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the thermo tag.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseThermo(String qName, Attributes attributes) {
//		if (phaseParseStatus.isPhase()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseThermo())) {
				thermoParseStatus.setThermo(true);
			}
//		}
	}
	
	/**
	 * Reads the value of the thermo property model.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseThermoModel(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseThermo())) {
			String model = attributes.getValue(appConfigCtml.getCtmlPhaseThermoModel());
			if (model != null) {
				thermoProperty.setModel(model);
				thermoParseStatus.setThermo(true);
				thermoParseStatus.setThermoModel(true);
			}
		}
	}

}
