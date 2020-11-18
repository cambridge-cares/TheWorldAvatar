package com.cmclinnovations.ontokin.model.parser.ctml.reactiondata;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses reaction metadata from a CTML file.
 * 
 * @author msff2
 *
 */
public class ReactionParser extends CtmlConverter implements IReactionParser{

	public void parse(String qName, Attributes attributes) {
		parseReaction(qName, attributes);
		parseDuplicate(qName, attributes);
		parseReversible(qName, attributes);
		parseLandauTeller(qName, attributes);
		parseType(qName, attributes);
		parseMayNonCon(qName, attributes);
		parsePartialPressure(qName, attributes);
		parseSiteFrac(qName, attributes);
		parseId(qName, attributes);
		parseComment(qName, attributes);
		parseEquation(qName, attributes);
		parseRateCoefficient(qName, attributes);
		parseReactants(qName, attributes);
		parseProducts(qName, attributes);
	}

	private void parseReaction(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionDataReaction())) {
			if(lastComment!=null){
				reaction.setSourceComment(lastComment);
				lastComment = null;
			}
			reactionParseStatus.setReaction(true);
		}
	}

	private void parseDuplicate(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionDataReaction())) {
			String duplicate = attributes.getValue(appConfigCtml.getReactionDuplicate());
			if (duplicate != null) {
				reaction.setDuplicate(duplicate);
				reactionParseStatus.setDuplicate(true);
				reactionParseStatus.setReaction(true);
			}
		}
	}
	
	private void parseReversible(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionDataReaction())) {
			String reversible = attributes.getValue(appConfigCtml.getReactionReversible());
			if (reversible != null) {
				reaction.setReversible(reversible);
				reactionParseStatus.setReversible(true);
				reactionParseStatus.setReaction(true);
			}
		}
	}

	private void parseLandauTeller(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionDataReaction())) {
			String landauTeller = attributes.getValue(appConfigCtml.getReactionLandauTeller());
			if (landauTeller != null) {
				reaction.setLandauTeller(landauTeller);
				reactionParseStatus.setLandauTeller(true);
				reactionParseStatus.setReaction(true);
			}
		}
	}

	private void parseType(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionDataReaction())) {
			String type = attributes.getValue(appConfigCtml.getReactionType());
			if (type != null) {
				reaction.setType(type);
				reactionParseStatus.setType(true);
				reactionParseStatus.setReaction(true);
			}
		}
	}

	private void parseMayNonCon(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionDataReaction())) {
			String mayNonCon = attributes.getValue(appConfigCtml.getReactionMayNonCon());
			if (mayNonCon != null) {
				reaction.setNoncon(mayNonCon);
				reactionParseStatus.setNonCon(true);
				reactionParseStatus.setReaction(true);
			}
		}
	}

	private void parsePartialPressure(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionDataReaction())) {
			String partialPressure = attributes.getValue(appConfigCtml.getReactionPartialPressure());
			if (partialPressure != null) {
				reaction.setPartialpressure(partialPressure);
				reactionParseStatus.setPartialPressure(true);
				reactionParseStatus.setReaction(true);
			}
		}
	}

	private void parseSiteFrac(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionDataReaction())) {
			String siteFrac = attributes.getValue(appConfigCtml.getReactionSiteFrac());
			if (siteFrac != null) {
				reaction.setSitefrac(siteFrac);
				reactionParseStatus.setSiteFrac(true);
				reactionParseStatus.setReaction(true);
			}
		}
	}

	private void parseId(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionDataReaction())) {
			String id = attributes.getValue(appConfigCtml.getReactionId());
			if (id != null) {
				reaction.setId(id);
				reactionParseStatus.setId(true);
				reactionParseStatus.setReaction(true);
			}
		}
	}

	/**
	 * Parses and reads the comment about a reaction.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseComment(String qName, Attributes attributes) {
		if (reactionParseStatus.isReaction()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionComment())) {
				reactionParseStatus.setComment(true);
			}
		}
	}

	/**
	 * Parses and reads the equation of a reaction.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseEquation(String qName, Attributes attributes) {
		if (reactionParseStatus.isReaction()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionEquation())) {
				reactionParseStatus.setEquation(true);
			}
		}
	}
	
	/**
	 * Parses and reads the reactants of a reaction.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseReactants(String qName, Attributes attributes) {
		if (reactionParseStatus.isReaction()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionReactants())) {
				reactionParseStatus.setReactants(true);
			}
		}
	}
	
	/**
	 * Parses and reads the products of a reaction.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseProducts(String qName, Attributes attributes) {
		if (reactionParseStatus.isReaction()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionProducts())) {
				reactionParseStatus.setProducts(true);
			}
		}
	}
	
	/**
	 * Parses the rate coefficient of a reaction.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseRateCoefficient(String qName, Attributes attributes) {
		if (reactionParseStatus.isReaction()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionRateCoeff())) {
				rateCoeffParseStatus.setRateCoeff(true);
			}
		}
	}
}
