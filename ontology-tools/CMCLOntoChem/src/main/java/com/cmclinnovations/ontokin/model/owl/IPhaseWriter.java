package com.cmclinnovations.ontokin.model.owl;

import org.slf4j.Logger;
import org.xml.sax.SAXException;

/**
 * Declares the method whose implementation reads phase data and
 * and attributes from an in-memory temporary storage to pass them
 * to the corresponding CTML to OWL conversion methods.
 * 
 * @author msff2
 *
 */
public interface IPhaseWriter {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(IPhaseWriter.class);
	public void writer(char ch[], int start, int length) throws SAXException;
	public void readElementArray(String elementArrayData);
	public void readSiteDensity(String siteDensityValue);
	public void readPhaseArray(String phaseArrayData);
}
