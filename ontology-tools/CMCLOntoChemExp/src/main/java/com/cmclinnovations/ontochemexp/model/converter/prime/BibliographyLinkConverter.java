package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;


public class BibliographyLinkConverter extends PrimeConverter implements IBibliographyLinkConverter{
	public void parse(String qName, Attributes attributes){
		//System.out.println("#######qName############:");
		// Calls the method that forwards calls to
		// the methods that parse BibliographyLink   data and metadata.
		iBibliographyLinkParser.parse(qName, attributes);
	}
}
