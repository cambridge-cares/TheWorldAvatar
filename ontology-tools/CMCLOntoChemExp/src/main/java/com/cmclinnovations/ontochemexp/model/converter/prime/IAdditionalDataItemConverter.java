package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;

public interface IAdditionalDataItemConverter {
	public void parse(String qName, Attributes attributes);
}
