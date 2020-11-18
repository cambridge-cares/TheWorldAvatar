package com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the reaction order data and metadata from a CTML file.
 * 
 * @author msff2
 *
 */public class ReactionOrderParser extends CtmlConverter implements IReactionOrderParser{

	public void parse(String qName, Attributes attributes) {
		parseOrder(qName, attributes);
		parseOrderDirection(qName, attributes);
		parseOrderSpecies(qName, attributes);
	}

	private void parseOrder(String qName, Attributes attributes) {
		if(reactionParseStatus.isReaction()){
			if (qName.equalsIgnoreCase(appConfigCtml.getReactionOrder())) {
				reactionOrderParseStatus.setOrder(true);
			}
		}
	}

	private void parseOrderDirection(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionOrder())) {
			String direction = attributes.getValue(appConfigCtml.getReactionDirection());
			if (direction != null) {
				reactionOrder.setDirection(direction);
				reactionOrderParseStatus.setDirection(true);
				reactionOrderParseStatus.setOrder(true);
			}
		}
	}
	
	private void parseOrderSpecies(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionOrder())) {
			String species = attributes.getValue(appConfigCtml.getReactionOrderSpecies());
			if (species != null) {
				reactionOrder.setSpecies(species);
				reactionOrderParseStatus.setSpecies(true);
				reactionOrderParseStatus.setOrder(true);
			}
		}
	}
}
