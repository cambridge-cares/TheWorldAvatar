package com.cmclinnovations.ontokin.model.parser.ctml.phase;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the phase array of a phase from a CTML file if available.
 * 
 * @author msff2
 *
 */
public class PhaseArrayParser extends CtmlConverter implements IPhaseArrayParser{
	/**
	 * Parses the CTML phase array data.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// a phase array.
		parsePhaseArray(qName, attributes);
	}
	
	/**
	 * Checks the appearance of a phase array tag.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parsePhaseArray(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseArray())) {
			phaseArrayParseStatus.setPhaseArray(true);
		}
	}
}
