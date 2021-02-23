package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public interface IAdditionalDataItemConverter {
	public void parse(String qName, Attributes attributes);
}
