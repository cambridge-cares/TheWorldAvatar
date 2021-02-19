package com.cmclinnovations.ontochem.model.parser.ctml;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

public class ReactionDataParser extends CtmlConverter implements IReactionDataParser{
	public void parse(String qName, Attributes attributes){
	parseReactionData(qName, attributes);
	parseId(qName, attributes);
	parseCaseSensitive(qName, attributes);
	}
	
	private void parseReactionData(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionData())) {
			// Initialises all the objects maintaining the structure of 
			// a reaction
			initCtmlConverter.initReaction();
			if(lastComment!=null){
				reactionData.setSourceComment(lastComment);
				lastComment = null;
			}
			reactionDataParseStatus.setReactionData(true);
		}
	}
	
	private void parseId(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionData())) {
			String id = attributes.getValue(appConfigCtml.getReactionDataId());
			if (id != null) {
				reactionData.setId(id);
				reactionDataParseStatus.setId(true);
				reactionDataParseStatus.setReactionData(true);
			}
		}
	}

	private void parseCaseSensitive(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionData())) {
			String caseSensitive = attributes.getValue(appConfigCtml.getReactionDataCaseSensitive());
			if (caseSensitive != null) {
				reactionData.setCaseSensitive(caseSensitive);
				reactionDataParseStatus.setCaseSensitive(true);
				reactionDataParseStatus.setReactionData(true);
			}
		}
	}

}
