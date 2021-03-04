package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public interface IPreferredKeyConverter {
	public void parse(String qName, Attributes attributes);
}
