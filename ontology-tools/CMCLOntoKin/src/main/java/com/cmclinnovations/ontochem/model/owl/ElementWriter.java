package com.cmclinnovations.ontochem.model.owl;

import org.xml.sax.SAXException;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontochem.model.exception.OntoException;

/**
 * Implements the method that forwards a call to those methods that
 * read element metadata and attributes from an in-memory temporary storage 
 * to pass them to the corresponding CTML to OWL conversion methods.
 * 
 * @author msff2
 *
 */
public class ElementWriter extends CtmlConverter implements IElementWriter {
	public void writer(char ch[], int start, int length) throws SAXException{
		readCtmlElementData(ch, start, length);
	}
	
	private void readCtmlElementData(char ch[], int start, int length) throws SAXException {
		if (elementDataParseStatus.isElementData()) {
			// Calls this method to read the element data
			// property values e.g. id
			readElementDataProperties();
			if (elementParseStatus.isElementDataElement()) {
				// Calls this method to read the element
				// properties of a mechanism from CTML 
				// e.g. name and atomic mass 
				readElementProperties(ch, start, length);
			}
		}
	}
	
	private void readElementDataProperties() {
		if (elementDataParseStatus.isElementDataId()) {
			readElementDataId();
			elementDataParseStatus.setElementDataId(false);
		}
		if (elementDataParseStatus.isElementDataCaseSensitive()) {
			readElementDataCaseSensitive();
			elementDataParseStatus.setElementDataCaseSensitive(false);
		}
	}


	private void readElementProperties(char ch[], int start, int length) {
		if (elementParseStatus.isElementName()) {
			readElementDataElementName();
			elementParseStatus.setElementName(false);
		}
		if (elementParseStatus.isElementAtomicWt()) {
			readElementDataElementAtomicWt();
			elementParseStatus.setElementAtomicWt(false);
		}
		if (elementParseStatus.isElementAtomicWtUnits()) {
			readElementAtomicWtUnits();
			elementParseStatus.setElementAtomicWtUnits(false);
		}else{
			readElementAtomicWtDefaultUnits();
		}
	}

	/**
	 * Reads the id of the group of elements in the mechanism being parsed
	 */
	private void readElementDataId() {
		try {
			iOwlConstructWriter.addElementId(basePath, elementData.getId());
		} catch (OntoException e) {
			logger.error("The id of the group of elements could not be created.");
		}
	}
	
	/**
	 * Reads if the element data is case sensitive or not
	 */
	private void readElementDataCaseSensitive() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH,
					appConfigOntokin.getElementDataCaseSensitivity(), elementData.getCaseSensitive(),
					appConfigOntokin.getElementMetadata(), elementMetaDataInstanceId);
		} catch (OntoException e) {
			logger.error("The case sensitivity infomration of the element data could not be created.");
		}
	}
	
	/**
	 * Reads the name of an element
	 */
	private void readElementDataElementName() {
		try {
			iOwlConstructWriter.addElementName(basePath, elementDataElement.getName());
		} catch (OntoException e) {
			logger.error("The name of an element could not be created.");
		}
	}

	/**
	 * Reads the atomic mass of an element
	 */
	private void readElementDataElementAtomicWt() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getElementAtomicWt(),
					elementDataElement.getAtomicWt(),
					appConfigOntokin.getOntokinElement().concat(UNDERSCORE).concat(elementDataElement.getName()));
		} catch (OntoException e) {
			logger.error("The atomic mass of an element could not be created.");
		}
	}
	
	/**
	 * Writes the units of atomic weight for an atom.
	 */
	private void readElementAtomicWtUnits() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyAtomicWtUnits(),
					elementDataElement.getUnits(),
					appConfigOntokin.getOntokinElement().concat(UNDERSCORE).concat(elementDataElement.getName()));
		} catch (OntoException e) {
			logger.error("The units of atomic weight of an element could " + "not be created");
		}
	}
	
	/**
	 * Writes the default units of atomic weight for an atom.
	 */
	private void readElementAtomicWtDefaultUnits() {
		try {
			iOwlConstructWriter.addDataProperty(basePath, HASH, appConfigOntokin.getDataPropertyAtomicWtUnits(),
					appConfigCtml.getCtmlElementAtomicWtDefaultUnits(),
					appConfigOntokin.getOntokinElement().concat(UNDERSCORE).concat(elementDataElement.getName()));
		} catch (OntoException e) {
			logger.error("The units of atomic weight of an element could " + "not be created");
		}
	}
}
