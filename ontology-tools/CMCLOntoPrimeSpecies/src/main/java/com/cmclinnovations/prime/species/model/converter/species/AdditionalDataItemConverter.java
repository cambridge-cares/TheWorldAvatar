package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public class AdditionalDataItemConverter extends PrimeSpeciesConverter implements IAdditionalDataItemConverter {
	public void parse(String qName, Attributes attributes) {
		iAdditionalDataItemParser.parse(qName, attributes);
	}
}
