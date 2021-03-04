package com.cmclinnovations.ontochem.model.converter.ctml;

import org.xml.sax.Attributes;
/**
 * Implements the method that forwards calls to the following parsers:
 * 1. CTML Metadata (such as CMCL ctml version, commit and comment) parser.
 * 2. Validation requirement info parser.
 * 
 * @author msff2
 *
 */
public class MetadataConverter extends CtmlConverter implements IMetadataConverter{
	public void parse(String qName, Attributes attributes){
		// Calls the method that forwards the call to
		// the methods that parse the CTML metadata
		iCtmlMDParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the CTML root comments
		iCtmlCommentParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the CTML mechanism
		// validation metadata
		iValidateParser.parse(qName, attributes);
	}
}
