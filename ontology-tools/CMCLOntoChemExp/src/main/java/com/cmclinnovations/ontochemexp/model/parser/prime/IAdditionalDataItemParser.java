package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;

public interface IAdditionalDataItemParser {
	public void parse(String qName, Attributes attributes);
}
