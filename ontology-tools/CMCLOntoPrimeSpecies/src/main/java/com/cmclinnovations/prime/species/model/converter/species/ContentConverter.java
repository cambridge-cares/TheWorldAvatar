package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public class ContentConverter extends PrimeSpeciesConverter implements IContentConverter {
	public void parse(String qName, Attributes attributes) {
		iContentParser.parse(qName, attributes);
	}
}
