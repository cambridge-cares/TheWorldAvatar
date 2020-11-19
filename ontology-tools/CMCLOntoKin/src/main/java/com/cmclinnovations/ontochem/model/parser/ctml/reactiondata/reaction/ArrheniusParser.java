package com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Arrhenius;

/**
 * This class parses Arrhenius metadata (e.g. name and type) and coefficients
 * from a CTML file.
 * 
 * @author msff2
 *
 */
public class ArrheniusParser extends CtmlConverter implements IArrheniusParser{

	public void parse(String qName, Attributes attributes) {
		parseArrhenius(qName, attributes);
		parseName(qName, attributes);
		parseType(qName, attributes);
		parseMotzWise(qName, attributes);
		parseSpecies(qName, attributes);
		parseCoeffA(qName, attributes);
		parseCoeffAUnits(qName, attributes);
		parseCoeffB(qName, attributes);
		parseCoeffBUnits(qName, attributes);
		parseCoeffE(qName, attributes);
		parseCoeffEUnits(qName, attributes);
		parseCoeffP(qName, attributes);
		parseCoeffPUnits(qName, attributes);
	}

	private void parseArrhenius(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionRateCoeffArrhenius())) {
				arrhenius = new Arrhenius();
				arrheniusParseStatus.setArrhenius(true);
			}
		}
	}

	private void parseName(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionRateCoeffArrhenius())) {
				String name = attributes.getValue(appConfigCtml.getReactionRateCoeffArrheniusName());
				if (name != null) {
					arrhenius.setName(name);
					arrheniusParseStatus.setName(true);
					arrheniusParseStatus.setArrhenius(true);
				}
			}
		}
	}
	
	private void parseType(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionRateCoeffArrhenius())) {
				String type = attributes.getValue(appConfigCtml.getReactionRateCoeffArrheniusType());
				if (type != null) {
					arrhenius.setType(type);
					arrheniusParseStatus.setType(true);
					arrheniusParseStatus.setArrhenius(true);
				}
			}
		}
	}
	
	private void parseMotzWise(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionRateCoeffArrhenius())) {
				String motzWise = attributes.getValue(appConfigCtml.getReactionRateCoeffArrheniusMotzWise());
				if (motzWise != null) {
					arrhenius.setMotzWise(motzWise);
					arrheniusParseStatus.setMotzWise(true);
					arrheniusParseStatus.setArrhenius(true);
				}
			}
		}
	}

	private void parseSpecies(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionRateCoeffArrhenius())) {
				String species = attributes.getValue(appConfigCtml.getReactionRateCoeffArrheniusSpecies());
				if (species != null) {
					arrhenius.setSpecies(species);
					arrheniusParseStatus.setSpecies(true);
					arrheniusParseStatus.setArrhenius(true);
				}
			}
		}
	}
	
	private void parseCoeffA(String qName, Attributes attributes) {
		if (arrheniusParseStatus.isArrhenius()) {			
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusA())) {
				arrheniusParseStatus.setA(true);
			}
		}
	}
	
	private void parseCoeffAUnits(String qName, Attributes attributes) {
		if (arrheniusParseStatus.isArrhenius()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusA())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffArrheniusAUnits());
				if (units != null) {
					arrheniusCoeffA.setUnits(units);
					arrheniusParseStatus.setAUnits(true);
					arrheniusParseStatus.setA(true);
					arrheniusParseStatus.setArrhenius(true);
				}
			}
		}
	}
	
	private void parseCoeffB(String qName, Attributes attributes) {
		if (arrheniusParseStatus.isArrhenius()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusB())) {
				arrheniusParseStatus.setB(true);
			}
		}
	}
	
	private void parseCoeffBUnits(String qName, Attributes attributes) {
		if (arrheniusParseStatus.isArrhenius()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusB())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffArrheniusBUnits());
				if (units != null) {
					arrheniusCoeffB.setUnits(units);
					arrheniusParseStatus.setBUnits(true);
					arrheniusParseStatus.setB(true);
					arrheniusParseStatus.setArrhenius(true);
				}
			}
		}
	}

	private void parseCoeffE(String qName, Attributes attributes) {
		if (arrheniusParseStatus.isArrhenius()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusE())) {
				arrheniusParseStatus.setE(true);
			}
		}
	}
	
	private void parseCoeffEUnits(String qName, Attributes attributes) {
		if (arrheniusParseStatus.isArrhenius()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusE())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffArrheniusEUnits());
				if (units != null) {
					arrheniusCoeffE.setUnits(units);
					arrheniusParseStatus.setEUnits(true);
					arrheniusParseStatus.setE(true);
					arrheniusParseStatus.setArrhenius(true);
				}
			}
		}
	}
	
	private void parseCoeffP(String qName, Attributes attributes) {
		if (arrheniusParseStatus.isArrhenius()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusP())) {
				arrheniusParseStatus.setP(true);
			}
		}
	}
	
	private void parseCoeffPUnits(String qName, Attributes attributes) {
		if (arrheniusParseStatus.isArrhenius()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusP())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffArrheniusPUnits());
				if (units != null) {
					arrheniusCoeffP.setUnits(units);
					arrheniusParseStatus.setPUnits(true);
					arrheniusParseStatus.setArrhenius(true);
				}
			}
		}
	}
}