package com.cmclinnovations.ontochem.model.owl;

import org.slf4j.Logger;
import org.xml.sax.SAXException;

/**
 * Declares the method whose implementation calls the method that reads 
 * the CTML metadata from an in-memory temporary storage to pass them
 * to the corresponding CTML to OWL conversion methods.
 * 
 * @author msff2
 *
 */
public interface IMetadataWriter {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(IMetadataWriter.class);
	public void writer(char ch[], int start, int length) throws SAXException;
}
