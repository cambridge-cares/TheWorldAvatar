package com.cmclinnovations.ontochemexp.model.owl;

import org.slf4j.Logger;
import org.xml.sax.SAXException;

/**
 * Declares the method whose implementation calls the method that reads 
 * a PrIMe experiment's dataGroup data and metadata from an in-memory temporary 
 * storage to pass them to the corresponding PrIMe to OWL conversion methods.
 *  * @author Songyi Deng  (sd626@cam.ac.uk)
 * 
 */
public interface IDataGroupWriter {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(IDataGroupWriter.class);
	public void writer(char ch[], int start, int length) throws SAXException;
	public void writeUncertaintyValue();
	public void setUPUncertainty();
	public void readDataGroupDataPointXUncertainty(String qName) throws SAXException;
	public void writeX1ToOwl();
	public void writeX2ToOwl();
	public void writeX3ToOwl();
	public void writeX4ToOwl();
	public void writeX5ToOwl();
	public void writeX6ToOwl();
	public void writeX7ToOwl();
	public void writeX8ToOwl();
	public void writeX9ToOwl();
	public void writeX10ToOwl();
	public void writeX11ToOwl();
}
