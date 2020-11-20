package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

public interface IDataGroupPropertyValueParser {
	public void parse(String qName, Attributes attributes);
}
