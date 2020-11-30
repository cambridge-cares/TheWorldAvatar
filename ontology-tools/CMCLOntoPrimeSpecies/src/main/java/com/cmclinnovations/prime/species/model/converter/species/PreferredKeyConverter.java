package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public class PreferredKeyConverter extends PrimeSpeciesConverter implements IPreferredKeyConverter {
	public void parse(String qName, Attributes attributes) {
		iPreferredKeyParser.parse(qName, attributes);
	}
}
