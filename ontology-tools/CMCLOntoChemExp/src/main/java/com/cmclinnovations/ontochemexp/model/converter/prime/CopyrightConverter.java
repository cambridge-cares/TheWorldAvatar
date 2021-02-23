package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;


public class CopyrightConverter extends PrimeConverter implements ICopyrightConverter{
	public void parse(String qName, Attributes attributes){
		// Calls the method that forwards calls to
		// the methods that parse Copyright   data and metadata.
		iCopyrightParser.parse(qName, attributes);
	}
}
