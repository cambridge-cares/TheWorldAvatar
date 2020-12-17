package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

/*
 * @author Songyi Deng (sd626@cam.ac.uk)
 */
public interface IDataGroupPropertyParser {
	public void parse(String qName, Attributes attributes);
}

