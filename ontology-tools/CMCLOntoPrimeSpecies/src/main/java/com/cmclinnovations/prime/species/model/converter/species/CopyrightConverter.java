package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public class CopyrightConverter extends PrimeSpeciesConverter implements ICopyrightConverter {
	public void parse(String qName, Attributes attributes) {
		iCopyrightParser.parse(qName, attributes);
	}
}
