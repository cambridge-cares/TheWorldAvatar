package com.cmclinnovations.ontochem.model.parser.ctml;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

public class CtmlCommentParser extends CtmlConverter implements ICtmlCommentParser {
	/**
	 * Parses the CTML species and reaction validation related metadata.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the CTML comment and extracts its value
		parseCtmlComment(qName, attributes);
		// Calls the method that checks the appearance of
		// a material name within a CTML comment and extracts its value
		parseCtmlMaterial(qName, attributes);
	}

	/**
	 * Parses and reads the value of the CTML comment
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseCtmlComment(String qName, Attributes attributes) {
		if (ctmlMDParseStatus.isCtml()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getCtmlComment())) {
				ctmlCommentParseStatus.setComment(true);
			}
		}
	}

	/**
	 * Parses and reads the value of a material within a CTML comment
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseCtmlMaterial(String qName, Attributes attributes) {
		if (ctmlMDParseStatus.isCtml()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getCtmlComment())) {
				String material = attributes.getValue(appConfigCtml.getCtmlMaterial());
				if (material != null) {
					ctmlComment.setMaterial(material);
					ctmlCommentParseStatus.setMaterial(true);
					ctmlCommentParseStatus.setComment(true);
				}
			}
		}
	}
}
