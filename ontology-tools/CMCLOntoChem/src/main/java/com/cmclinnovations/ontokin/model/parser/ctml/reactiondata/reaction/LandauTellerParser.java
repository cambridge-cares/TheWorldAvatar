package com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the Landau-Teller Coefficients of a reaction 
 * from a CTML file.
 * 
 * @author msff2
 *
 */
public class LandauTellerParser extends CtmlConverter implements ILandauTellerParser{
	public void parse(String qName, Attributes attributes) {
		parseLandauTeller(qName, attributes);
		parseLandauTellerB(qName, attributes);
		parseLandauTellerBUnits(qName, attributes);
		parseLandauTellerC(qName, attributes);
		parseLandauTellerCUnits(qName, attributes);

	}
	
	private void parseLandauTeller(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffLandauTeller())) {
				landauTellerParseStatus.setLandauTeller(true);
			}
		}
	}
	
	private void parseLandauTellerB(String qName, Attributes attributes) {
		if (landauTellerParseStatus.isLandauTeller()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffLandauTellerB())) {
				landauTellerParseStatus.setB(true);
			}
		}
	}
	
	private void parseLandauTellerBUnits(String qName, Attributes attributes) {
		if (landauTellerParseStatus.isLandauTeller()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffLandauTellerB())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffLandauTellerBUnits());
				if (units != null) {
					landauTellerB.setUnits(units);
					landauTellerParseStatus.setBUnits(true);
					landauTellerParseStatus.setB(true);
					landauTellerParseStatus.setLandauTeller(true);
				}
			}
		}
	}
	
	private void parseLandauTellerC(String qName, Attributes attributes) {
		if (landauTellerParseStatus.isLandauTeller()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffLandauTellerC())) {
				landauTellerParseStatus.setC(true);
			}
		}
	}
	
	private void parseLandauTellerCUnits(String qName, Attributes attributes) {
		if (landauTellerParseStatus.isLandauTeller()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffLandauTellerC())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffLandauTellerCUnits());
				if (units != null) {
					landauTellerC.setUnits(units);
					landauTellerParseStatus.setCUnits(true);
					landauTellerParseStatus.setC(true);
					landauTellerParseStatus.setLandauTeller(true);
				}
			}
		}
	}
}
