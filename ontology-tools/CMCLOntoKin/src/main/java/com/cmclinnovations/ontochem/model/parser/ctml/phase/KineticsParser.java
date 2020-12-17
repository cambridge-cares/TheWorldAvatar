package com.cmclinnovations.ontochem.model.parser.ctml.phase;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * This class parses the kinetics model of a phase from a CTML file.
 * 
 * @author msff2
 *
 */
public class KineticsParser extends CtmlConverter implements IKineticsParser{
	/**
	 * Parses the CTML phase kinetics metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the kientics in a phase.
		parseKinetics(qName, attributes);
		// Calls the method that checks if the kinetics model of a phase
		// is available in the CTML file which is being processed.
		parseKineticsModel(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the kinetics tag.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseKinetics(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseKinetics())) {
			kineticsParseStatus.setKinetics(true);
		}
	}
	
	/**
	 * Reads the value of the kinetics model.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseKineticsModel(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseKinetics())) {
			String model = attributes.getValue(appConfigCtml.getCtmlPhaseKineticsModel());
			if (model != null) {
				kinetics.setModel(model);
				kineticsParseStatus.setKinetics(true);
				kineticsParseStatus.setKineticsModel(true);
			}
		}
	}

}
