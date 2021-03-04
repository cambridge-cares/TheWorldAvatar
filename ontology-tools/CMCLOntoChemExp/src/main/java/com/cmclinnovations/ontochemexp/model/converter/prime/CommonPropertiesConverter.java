package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;

/**
 * Implements the method that forwards calls to the CommonProperties parser.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class CommonPropertiesConverter extends PrimeConverter implements ICommonPropertiesConverter {
	public void parse(String qName, Attributes attributes) {
		// System.out.println("#######qName############:");
		// Calls the method that forwards calls to
		// the methods that parse commonProperties data and metadata.
		iCommonPropertiesParser.parse(qName, attributes);
		iCommonPropertiesPropertyParser.parse(qName, attributes);
		iCommonPropertiesPropertyValueParser.parse(qName, attributes);
		iCommonPropertiesPropertyUncertaintyParser.parse(qName, attributes);
		iCommonPropertiesPropertyComponentParser.parse(qName, attributes);
		iCommonPropertiesPropertyComponentSpeciesLinkParser.parse(qName, attributes);
		iCommonPropertiesPropertyComponentAmountParser.parse(qName, attributes);
		iCommonPropertiesPropertyComponentUncertaintyParser.parse(qName, attributes);
	}
}
