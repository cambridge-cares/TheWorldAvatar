package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

/**
 * This class parses a PrIMe experiment's apparatus data and stores them 
 * in the experiment data structure.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class ExperimentParser extends PrimeConverter implements IExperimentParser {
	/**
	 * Parses PrIMe experiment data and metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the PrIMe root tag, which is experiment
		parseExperiment(qName, attributes);
		parseExperimentPrimeID(qName, attributes);
		parseExperimentXmlns(qName, attributes);
		parseExperimentXmlnsXsi(qName, attributes);
		parseExperimentXsiSchemaLocation(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the experiment tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseExperiment(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemExperiment())) {
			experimentParseStatus.setExperiment(true);
		}
	}
	
	private void parseExperimentPrimeID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemExperiment())) {
			String primeID = attributes.getValue(primeVocabulary.getAttribPrimeID());
			if (primeID != null) {
				experiment.setPrimeID(primeID);
				experimentParseStatus.setPrimeID(true);
				experimentParseStatus.setExperiment(true);
			}
		}
	}
	
	private void parseExperimentXmlns(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemExperiment())) {
			String xmlns = attributes.getValue("xmlns");
			if (xmlns != null) {
				experiment.setXmlns(xmlns);
				experimentParseStatus.setXmlns(true);
				experimentParseStatus.setExperiment(true);
			}
		}
	}
	
	private void parseExperimentXmlnsXsi(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemExperiment())) {
//			String xmlnsXsi = attributes.getValue(primeVocabulary.getAttribXmlnsXsi());
			String xmlnsXsi = "http://www.w3.org/2001/XMLSchema-instance";
			if (xmlnsXsi != null) {
				experiment.setXmlnsXsi(xmlnsXsi);
				experimentParseStatus.setXmlnsXsi(true);
				experimentParseStatus.setExperiment(true);
			}
		}
	}
	
	private void parseExperimentXsiSchemaLocation(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemExperiment())) {
			String xsiSchemaLocation = attributes.getValue(primeVocabulary.getAttribXsiSchemaLocation());
			if (xsiSchemaLocation != null) {
				experiment.setXsiSchemaLocation(xsiSchemaLocation);
				experimentParseStatus.setXsiSchemaLocation(true);
				experimentParseStatus.setExperiment(true);
			}
		}
	}
}
