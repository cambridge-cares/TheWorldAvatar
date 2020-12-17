package com.cmclinnovations.ontochem.model.parser.ctml.phase;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * This class parses the reaction array metadata of a CTML phase element.
 * 
 * @author msff2
 *
 */
public class ReactionArrayParser extends CtmlConverter implements IReactionArrayParser{

	/**
	 * Parses the CTML reaction array metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the reaction array in a phase
		parseReactionArray(qName, attributes);
		// Calls the method that checks if the element array of a phase
		// is available in the CTML file which is being processed
		parseReactionArrayDataSource(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the speciesArray tag.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseReactionArray(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseReactionArray())) {
			reactionArrayParseStatus.setReactionArray(true);
		}
	}
	
	/**
	 * Reads the value of the reaction array data source property.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseReactionArrayDataSource(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseReactionArray())) {
			String dataSrc = attributes.getValue(appConfigCtml.getCtmlPhaseReactionArrayDataSrc());
			if (dataSrc != null) {
				reactionArray.setDatasrc(dataSrc);
				reactionArrayParseStatus.setReactionArray(true);
				reactionArrayParseStatus.setReactionDataSrc(true);
			}
		}
	}
	
}
