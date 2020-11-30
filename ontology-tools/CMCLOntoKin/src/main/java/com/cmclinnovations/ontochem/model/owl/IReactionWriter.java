package com.cmclinnovations.ontochem.model.owl;

import org.slf4j.Logger;
import org.xml.sax.SAXException;

/**
 * Declares the method whose implementation reads reaction data and
 * and attributes from an in-memory temporary storage to pass them
 * to the corresponding CTML to OWL conversion methods.
 * 
 * @author msff2
 *
 */
public interface IReactionWriter {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(IReactionWriter.class);
	public void writer(char ch[], int start, int length) throws SAXException;
	public void writeReactionOrder(String order);
	public void decideArrheniusOrSticking();
	public void writeArrheniusProperties(String coefficientType, Long instanceId);
	public void writeCoefficientA(String coeffA, String coefficientType, Long instanceId);
	public void writeCoefficientb(String coeffb, String coefficientType, Long instanceId);
	public void writeCoefficientE(String coeffE, String coefficientType, Long instanceId);
	public void writeCoefficientP(String coeffP, String coefficientType, Long instanceId);
	public void writeCoverageDependecies();
	public void writeCoefficienta(String coeffa);
	public void writeCoefficientm(String coeffm);
	public void writeCoefficiente(String coeffe);
	public void writeLandauTellerCoeffB(String coeffB);
	public void writeLandauTellerCoeffC(String coeffC);
	public void writeFallOffModelData(String fallOffModeldata);
	public void writeChebInstance();
	public void writeTminProperty(String tmin);
	public void writeTmaxProperty(String tmax);
	public void writePminProperty(String pmin);
	public void writePmaxProperty(String pmax);
}
