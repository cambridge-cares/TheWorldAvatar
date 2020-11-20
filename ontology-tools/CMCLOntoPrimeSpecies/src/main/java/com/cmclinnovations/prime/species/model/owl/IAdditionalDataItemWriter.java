package com.cmclinnovations.prime.species.model.owl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;

public interface IAdditionalDataItemWriter {
	static Logger logger = LoggerFactory.getLogger(IAdditionalDataItemWriter.class);
	public void writer(char ch[], int start, int length) throws SAXException;
}
