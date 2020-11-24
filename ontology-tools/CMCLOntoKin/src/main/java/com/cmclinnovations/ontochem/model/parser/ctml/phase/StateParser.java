package com.cmclinnovations.ontochem.model.parser.ctml.phase;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * This class parses the state of a CTML phase element.
 * 
 * @author msff2
 *
 */
public class StateParser extends CtmlConverter implements IStateParser{
	/**
	 * Parses the CTML state element of a phase.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the state in a phase
		parsePhaseState(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the state tag in a phase.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parsePhaseState(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseState())) {
			phaseStateParseStatus.setState(true);
		}
	}
}
