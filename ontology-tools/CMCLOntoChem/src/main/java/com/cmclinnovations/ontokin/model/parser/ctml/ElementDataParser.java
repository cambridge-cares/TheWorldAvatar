package com.cmclinnovations.ontokin.model.parser.ctml;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

public class ElementDataParser extends CtmlConverter implements IElementDataParser{

	public void parse(String qName, Attributes attributes){
	parseElementData(qName, attributes);
	parseId(qName, attributes);
	parseCaseSensitive(qName, attributes);
	}
	
	private void parseElementData(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlElementData())) {
			// Initialises all the objects maintaining the structure of 
			// an element
			initCtmlConverter.initElement();
			if(lastComment!=null){
				elementData.setSourceComment(lastComment);
				lastComment = null;
			}
			elementDataParseStatus.setElementData(true);
		}
	}
	
	private void parseId(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlElementData())) {
			String id = attributes.getValue(appConfigCtml.getCtmlElementDataId());
			if (id != null) {
				elementData.setId(id);
				elementDataParseStatus.setElementDataId(true);
				elementDataParseStatus.setElementData(true);
			}
		}
	}

	private void parseCaseSensitive(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlElementData())) {
			String caseSensitive = attributes.getValue(appConfigCtml.getCtmlElementDataCaseSensitive());
			if (caseSensitive != null) {
				elementData.setCaseSensitive(caseSensitive);
				elementDataParseStatus.setElementDataCaseSensitive(true);
				elementDataParseStatus.setElementData(true);
			}
		}
	}

}
