package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;
/**
 * Declares the method whose implementation parses PrIMe commonProperties data and metadata. 
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public interface ICommonPropertiesConverter{
	public void parse(String qName, Attributes attributes);
}
