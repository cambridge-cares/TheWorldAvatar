package com.cmclinnovations.ontokin.model.parser.ctml;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the CTML Validate metadata and stores them in the 
 * Validate data structure.
 * 
 * @author msff2
 *
 */
public class ValidateParser  extends CtmlConverter implements IValidateParser{
	/**
	 * Parses the CTML species and reaction validation related metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes){
	// Calls the method that checks the appearance of
	// the CTML validate
	parseValidate(qName, attributes);
	// Calls the method that checks if the species of
	// the mechanism appeared in the CTML file which is
	// being processed require a validation
	parseSpeciesValidation(qName, attributes);
	// Calls the method that checks if the reactions of
	// the mechanism appeared in the CTML file which is
	// being processed require a validation
	parseReactionValidation(qName, attributes);
}
	
	/**
	 * Checks the appearance of the CTML validate tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseValidate(String qName, Attributes attributes) {
	if (qName.equalsIgnoreCase(appConfigCtml.getCtmlValidate())) {
		validateParseStatus.setValidate(true);
	}
	}
	/**
	 * Parses and confirms if the reactions of the mechanism 
	 * appearing the CTML file need to be validated
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseReactionValidation(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlValidate())) {
			String reactionsValidated = attributes.getValue(appConfigCtml.getCtmlValidateReactions());
			if (reactionsValidated != null) {
				validateMD.setReactions(reactionsValidated);
				validateParseStatus.setReactionsToBeValidated(true);
				validateParseStatus.setValidate(true);
			}
		}
	}

	/**
	 * Parses and confirms if the species of the mechanism 
	 * appearing the CTML file need to be validated
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseSpeciesValidation(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlValidate())) {
			String speciesValidated = attributes.getValue(appConfigCtml.getCtmlValidateSpecies());
			if (speciesValidated != null) {
				validateMD.setSpecies(speciesValidated);
				validateParseStatus.setSpeciesToBeValidated(true);
				validateParseStatus.setValidate(true);
			}
		}
	}
}
