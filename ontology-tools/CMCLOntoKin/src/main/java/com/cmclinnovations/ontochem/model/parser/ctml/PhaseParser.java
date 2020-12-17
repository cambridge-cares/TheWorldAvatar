package com.cmclinnovations.ontochem.model.parser.ctml;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * This class parses the CTML phase metadata and stores them in the 
 * phase data structure.
 * 
 * @author msff2
 *
 */
public class PhaseParser extends CtmlConverter implements IPhaseParser{
	/**
	 * Parses the CTML phase related metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes){
	// Calls the method that checks the appearance of
	// a CTML phase
	parsePhase(qName, attributes);
	// Calls the method that checks if the dimension of
	// a phase is available in the CTML file which is
	// being processed
	parseDimension(qName, attributes);
	// Calls the method that checks if the id of
	// a phase is available in the CTML file which is
	// being processed
	parseId(qName, attributes);
	// Calls the method that checks if the id of
	// a phase is available in the CTML file which is
	// being processed
	parseMaterial(qName, attributes);
	// Calls the method that checks if the comment about
	// a phase is available in the CTML file which is
	// being processed
	parseComment(qName, attributes);
	}
	
	/**
	 * Checks the appearance of the CTML phase tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parsePhase(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhase())) {
			// Initialises all the objects maintaining the structure of a phase
			initCtmlConverter.initPhase();
			if(lastComment!=null){
				phaseMD.setSourceComment(lastComment);
				lastComment = null;
			}
			phaseParseStatus.setPhase(true);
		}
	}
	
	/**
	 * Parses and extracts the values of the phase dimension if available.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseDimension(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhase())) {
			String phaseDimension = attributes.getValue(appConfigCtml.getCtmlPhaseDimension());
			if (phaseDimension != null) {
				phaseMD.setDimension(phaseDimension);
				phaseParseStatus.setPhaseDimension(true);
				phaseParseStatus.setPhase(true);
			}
		}
	}

	/**
	 * Parses and extracts the values of the phase id if available.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseId(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhase())) {
			String phaseId = attributes.getValue(appConfigCtml.getCtmlPhaseId());
			if (phaseId != null) {
				phaseMD.setId(phaseId);
				phaseParseStatus.setPhaseId(true);
				phaseParseStatus.setPhase(true);
			}
		}
	}
	
	/**
	 * Reads the value of phase material.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseMaterial(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhase())) {
			String material = attributes.getValue(appConfigCtml.getCtmlPhaseMaterial());
			if (material != null) {
				phaseMD.setMaterial(material);
				phaseParseStatus.setPhaseMaterial(true);
				phaseParseStatus.setPhase(true);
			}
		}
	}
	
	/**
	 * Parses and reads the comment about a phase.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseComment(String qName, Attributes attributes) {
		if (phaseParseStatus.isPhase()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getCtmlComment())) {
				phaseParseStatus.setPhaseComment(true);
			}
		}
	}
}
