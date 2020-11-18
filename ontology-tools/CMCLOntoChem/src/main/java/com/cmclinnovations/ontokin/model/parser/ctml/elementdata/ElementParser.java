package com.cmclinnovations.ontokin.model.parser.ctml.elementdata;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

public class ElementParser extends CtmlConverter implements IElementParser{

	public void parse(String qName, Attributes attributes){
	parseElement(qName, attributes);
	parseElementName(qName, attributes);
	parseElementAtomicWt(qName, attributes);
	parseElementAtomicWtUnits(qName, attributes);
	parseElementComment(qName, attributes);
	}
	
	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlElementDataElement())) {
			if(lastComment!=null){
				elementDataElement.setSourceComment(lastComment);
				lastComment = null;
			}
			elementParseStatus.setElementDataElement(true);
		}
	}
	
	private void parseElementName(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlElementDataElement())) {
			String name = attributes.getValue(appConfigCtml.getCtmlElementDataElementName());
			if (name != null) {
				elementDataElement.setName(name);
				elementParseStatus.setElementName(true);
				elementParseStatus.setElementDataElement(true);
			}
		}
	}

	private void parseElementAtomicWt(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlElementDataElement())) {
			String atomicWt = attributes.getValue(appConfigCtml.getCtmlElementDataElementAtomicWt());
			if (atomicWt != null) {
				elementDataElement.setAtomicWt(atomicWt);
				elementParseStatus.setElementAtomicWt(true);
				elementParseStatus.setElementDataElement(true);
			}
		}
	}

	private void parseElementAtomicWtUnits(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlElementDataElement())) {
			String units = attributes.getValue(appConfigCtml.getCtmlElementDataElementAtomicWtUnits());
			if (units != null) {
				elementDataElement.setUnits(units);
				elementParseStatus.setElementAtomicWtUnits(true);
				elementParseStatus.setElementDataElement(true);
			}
		}
	}
	
	/**
	 * Parses and reads the comment about an element.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseElementComment(String qName, Attributes attributes) {
		if (elementParseStatus.isElementDataElement()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getCtmlElementComment())) {
				elementParseStatus.setElementComment(true);
			}
		}
	}
}
