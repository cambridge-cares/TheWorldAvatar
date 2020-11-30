package com.cmclinnovations.ontochem.model.owl;

import org.slf4j.Logger;
import org.xml.sax.SAXException;

/**
 * Declares the method whose implementation reads species data and
 * and attributes from an in-memory temporary storage to pass them
 * to the corresponding CTML to OWL conversion methods.
 * 
 * @author msff2
 *
 */
public interface ISpeciesWriter {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ISpeciesWriter.class);
	public void writer(char ch[], int start, int length) throws SAXException;
	public void writeNote(String note);
	public void writeSizeInfo(String size);
	public void writeDensityInfo(String density);
	public void writeStringInfo(String string);
	public void writeLJWellDepthInfo(String wellDepth);
	public void writeLJDiameterInfo(String diameter);
	public void writeDipoleMomentInfo(String dipoleMoment);
	public void writePolarizabilityInfo(String polarizability);
	public void writeRotRelaxInfo(String rotRelax);
}
