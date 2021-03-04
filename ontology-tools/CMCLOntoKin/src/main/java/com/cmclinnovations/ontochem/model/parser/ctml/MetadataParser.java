package com.cmclinnovations.ontochem.model.parser.ctml;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * This class parses the CTML metadata and stores them in the CTML data
 * structure.
 * 
 * @author msff2
 *
 */
public class MetadataParser extends CtmlConverter implements IMetadataParser {

	/**
	 * Parses the CTML metadata attributes.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the CTML root tag
		parseCtmlRootTag(qName, attributes);
		// Calls the method that checks the appearance of
		// the CTML version and extracts its value
		parseCtmlVersion(qName, attributes);
		// Calls the method that checks the appearance of
		// the CTML commit info and extracts its value
		parseCtmlCommit(qName, attributes);
	}

	/**
	 * Checks the appearance of the CTML root tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseCtmlRootTag(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtml())) {
			ctmlMDParseStatus.setCtml(true);
		}
	}

	/**
	 * Parses and reads the value of the CTML version
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseCtmlVersion(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtml())) {
			String version = attributes.getValue(appConfigCtml.getCtmlVersion());
			if (version != null) {
				ctmlMD.setCmclVersion(version);
				ctmlMDParseStatus.setCtmlCmclVersion(true);
			}
		}
	}

	/**
	 * Parses and reads the value of the CTML commit
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseCtmlCommit(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtml())) {
			String commit = attributes.getValue(appConfigCtml.getCtmlCommit());
			if (commit != null) {
				ctmlMD.setCommit(commit);
				ctmlMDParseStatus.setCtmlCommit(true);
			}
		}
	}
}
