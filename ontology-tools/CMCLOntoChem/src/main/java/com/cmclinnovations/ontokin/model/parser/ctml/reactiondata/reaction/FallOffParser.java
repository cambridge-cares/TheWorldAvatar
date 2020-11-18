package com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the type of a fall-off reaction and the named third body 
 * required by a reaction.
 * 
 * @author msff2
 *
 */
public class FallOffParser extends CtmlConverter implements IFallOffParser{

	public void parse(String qName, Attributes attributes) {
		parseFallOff(qName, attributes);
		parseType(qName, attributes);
		parseNamedThirdBody(qName, attributes);
	}

	private void parseFallOff(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffFallOff())) {
				fallOffParseStatus.setFallOff(true);
			}
		}
	}

	private void parseType(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffFallOff())) {
				String type = attributes.getValue(appConfigCtml.getRateCoeffFallOffType());
				if (type != null) {
					fallOff.setType(type);
					fallOffParseStatus.setType(true);
					fallOffParseStatus.setFallOff(true);
				}
			}
		}
	}
	
	private void parseNamedThirdBody(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffFallOff())) {
				String namedThirdBody = attributes.getValue(appConfigCtml.getRateCoeffFallOffThirdBody());
				if (namedThirdBody != null) {
					fallOff.setNamedThirdBody(namedThirdBody);
					fallOffParseStatus.setNamedThirdBody(true);
					fallOffParseStatus.setFallOff(true);
				}
			}
		}
	}
}