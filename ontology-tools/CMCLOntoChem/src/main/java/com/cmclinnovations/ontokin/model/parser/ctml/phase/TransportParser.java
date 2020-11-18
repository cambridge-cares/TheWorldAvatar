package com.cmclinnovations.ontokin.model.parser.ctml.phase;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the transport model of a phase from a CTML file.
 * 
 * @author msff2
 *
 */
public class TransportParser extends CtmlConverter implements ITransportParser{
	/**
	 * Parses the CTML phase transport metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the transport property model in a phase.
		parseTransport(qName, attributes);
		// Calls the method that checks if the transport property model of a 
		// phase is available in the CTML file which is being processed.
		parseTransportModel(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the transport tag.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseTransport(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseTransport())) {
			transportParseStatus.setTransport(true);
		}
	}
	
	/**
	 * Reads the value of the transport property model.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseTransportModel(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseTransport())) {
			String model = attributes.getValue(appConfigCtml.getCtmlPhaseTransportModel());
			if (model != null) {
				transportProperty.setModel(model);
				transportParseStatus.setTransport(true);
				transportParseStatus.setTransportModel(true);
			}
		}
	}

}
