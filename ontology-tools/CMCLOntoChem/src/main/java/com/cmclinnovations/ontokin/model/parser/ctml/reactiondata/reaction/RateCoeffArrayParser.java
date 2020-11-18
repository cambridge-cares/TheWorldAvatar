package com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses rate coefficients of a Chebyshev reaction from a CTML file.
 * 
 * @author msff2
 *
 */
public class RateCoeffArrayParser extends CtmlConverter implements IRateCoeffArrayParser{

	public void parse(String qName, Attributes attributes) {
		parseRateCoeffArray(qName, attributes);
		parseName(qName, attributes);
		parseUnits(qName, attributes);
		parseDegreeT(qName, attributes);
		parseDegreeP(qName, attributes);
	}

	private void parseRateCoeffArray(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffFloatArray())) {
				rateCoeffArrayParseStatus.setFloatArray(true);
			}
		}
	}

	private void parseName(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffFloatArray())) {
				String name = attributes.getValue(appConfigCtml.getRateCoeffFloatArrayName());
				if (name != null) {
					rateCoeffArray.setName(name);
					rateCoeffArrayParseStatus.setName(true);
					rateCoeffArrayParseStatus.setFloatArray(true);
				}
			}
		}
	}
	
	private void parseUnits(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffFloatArray())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffFloatArrayUnits());
				if (units != null) {
					rateCoeffArray.setUnits(units);
					rateCoeffArrayParseStatus.setUnits(true);
					rateCoeffArrayParseStatus.setFloatArray(true);
				}
			}
		}
	}
	
	private void parseDegreeT(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffFloatArray())) {
				String degreeT = attributes.getValue(appConfigCtml.getRateCoeffFloatArrayDegreeT());
				if (degreeT != null) {
					rateCoeffArray.setDegreeT(degreeT);
					rateCoeffArrayParseStatus.setDegreeT(true);
					rateCoeffArrayParseStatus.setFloatArray(true);
				}
			}
		}
	}
	
	private void parseDegreeP(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffFloatArray())) {
				String degreeP = attributes.getValue(appConfigCtml.getRateCoeffFloatArrayDegreeP());
				if (degreeP != null) {
					rateCoeffArray.setDegreeP(degreeP);
					rateCoeffArrayParseStatus.setDegreeP(true);
					rateCoeffArrayParseStatus.setFloatArray(true);
				}
			}
		}
	}
}