package com.cmclinnovations.ontochem.model.parser.ctml.phase;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * This class parses the element array metadata of a CTML phase element.
 * 
 * @author msff2
 *
 */
public class ElementArrayParser extends CtmlConverter implements IElementArrayParser{

	/**
	 * Parses the CTML phase element array metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the element array in a phase
		parseElementArray(qName, attributes);
		// Calls the method that checks if the element array of a phase
		// is available in the CTML file which is being processed
		parseElementArrayDataSrc(qName, attributes);
	}

	/**
	 * Checks the appearance of the CTML phase tag.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseElementArray(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseElementArray())) {
			elementArrayParseStatus.setElementArray(true);
		}
	}
	
	/**
	 * Parses and extracts the data source of the element array from
	 * a phase if available.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseElementArrayDataSrc(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseElementArray())) {
			String dataSrc = attributes.getValue(appConfigCtml.getCtmlPhaseElmentArrayDataSrc());
			if (dataSrc != null) {
				elementArray.setDatasrc(dataSrc);
				elementArrayParseStatus.setElementDataSrc(true);
				elementArrayParseStatus.setElementArray(true);
			}
		}
	}	
}
