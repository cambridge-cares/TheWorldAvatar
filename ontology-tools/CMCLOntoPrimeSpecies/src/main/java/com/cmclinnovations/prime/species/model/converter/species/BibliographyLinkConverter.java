package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public class BibliographyLinkConverter extends PrimeSpeciesConverter implements IBibliographyLinkConverter {
	public void parse(String qName, Attributes attributes) {
		iBibliographyLinkParser.parse(qName, attributes);
	}
}
