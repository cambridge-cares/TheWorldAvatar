package com.cmclinnovations.ontochemexp.model.owl;

import org.slf4j.Logger;
import org.xml.sax.SAXException;

public interface IAdditionalDataItemWriter {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(IAdditionalDataItemWriter.class);
//	public void writer(char ch[], int start, int length) throws SAXException;
	public void writer(String qName) throws SAXException;
	public void writeValue();
	public void setUP();
}
