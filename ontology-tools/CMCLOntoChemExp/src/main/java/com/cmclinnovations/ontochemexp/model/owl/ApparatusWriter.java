
package com.cmclinnovations.ontochemexp.model.owl;

import java.util.ArrayList;

import org.apache.commons.lang3.text.WordUtils;
import org.semanticweb.owlapi.model.IRI;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Apparatus;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.ApparatusProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Kind;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Mode;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Uncertainty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Value;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

/**
 * Implements the method that forwards a call to those methods that
 * read Apparatus data from an in-memory temporary storage 
 * to pass them to the corresponding PrIMe to OWL conversion methods.
 * 
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class ApparatusWriter extends PrimeConverter implements IApparatusWriter {
	public void writer(char ch[], int start, int length) throws SAXException {
		readApparatus(ch, start, length);
		readApparatusKind(ch, start, length);
		readApparatusMode(ch, start, length);
		readApparatusProperty(ch, start, length);
		readApparatusPropertyValue(ch, start, length);
//		readApparatusPropertyUncertainty(ch, start, length);
	}
	
	/**
	 * Forwards the call to the methods that first read and then write 
	 * apparatus data and metadata.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	private void readApparatus(char ch[], int start, int length) throws SAXException {
		if (apparatusParseStatus.isApparatus()) {
			createEquipment();
			linkEquipmentToExperiment();
			apparatusParseStatus.setApparatus(false);
			experiment.setApparatus(apparatus);	
			apparatus = new Apparatus();
		}
	}
	
	/**
	 * Reads the kind of apparatus and writes it using OWL.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void readApparatusKind(char ch[], int start, int length) throws SAXException {
		if (kindParseStatus.isKind()) {
			String value = new String(ch, start, length);
			kind.setValue(value);
			createKind();
			linkKindToEquipment();
			kindParseStatus.setKind(false);
			apparatus.setKind(kind);
			kind = new Kind();
		}
	}
	
	/**
	 * Reads the mode of apparatus and writes it using OWL.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 */
	private void readApparatusMode(char ch[], int start, int length) throws SAXException {
		if (modeParseStatus.isMode()) {
			String value = new String(ch, start, length);
			mode.setValue(value);
			createMode();
			linkModeToEquipment();
			modeParseStatus.setMode(false);
			modeList.add(mode);
			mode = new Mode();
			apparatus.setMode(modeList);
			modeList = new ArrayList<Mode>();
		}
	}
	
	private void readApparatusProperty(char ch[], int start, int length) throws SAXException {
		if (apparatusPropertyParseStatus.isProperty()) {
//			createProperty();
//			linkPropertyToEquipment();
			DimensionalQuantityWriter dQ = new DimensionalQuantityWriter(apparatusID, 
					apparatusPropertyCount, 
					"Apparatus"+UNDERSCORE+apparatusID, 
					apparatusProperty);
			try {
				dQ.createDimensionalQuantityInOWL();
			} catch (OntoChemExpException e) {
				e.printStackTrace();
			}
			apparatusPropertyParseStatus.setProperty(false);
			apparatusPropertyList.add(apparatusProperty);
			apparatusProperty = new ApparatusProperty();
			apparatus.setProperty(apparatusPropertyList);
			apparatusPropertyList = new ArrayList<ApparatusProperty>();
		}
	}

	private void readApparatusPropertyValue(char ch[], int start, int length) throws SAXException {
		if (apparatusPropertyValueParseStatus.isValue()) {
			String value = new String(ch, start, length);
			apparatusPropertyValue.setValueValue(value);
			createPropertyValue();
//			linkPropertyValueToProperty();
			apparatusPropertyValueParseStatus.setValue(false);
			apparatusProperty.setPropertyValue(apparatusPropertyValue);
			apparatusPropertyValue = new Value();
		}
	}
	
	private void readApparatusPropertyUncertainty(char ch[], int start, int length) throws SAXException {
		if (apparatusPropertyUncertaintyParseStatus.isUncertainty()) {
			String value = new String(ch, start, length);
			apparatusPropertyUncertainty.setUncertaintyValue(value);
			createPropertyUncertainty();
			linkPropertyUncertaintyToProperty();
			apparatusPropertyUncertaintyParseStatus.setUncertainty(false);
			apparatusProperty.setPropertyUncertainty(apparatusPropertyUncertainty);
			apparatusPropertyUncertainty = new Uncertainty();
		}
	}
	
	/**
	 * Following the detection of the kind element, it creates 
	 * an instance of Equipment in OWL.
	 * 
	 */
	private void createEquipment() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassApparatus(), 
					"Apparatus"+UNDERSCORE+apparatusID);
			} catch (ABoxManagementException e) {
			logger.error(
					"An individual of equipment could not be created.");
		}
	}
	
	/**
	 * Links the equipment to the experiment.
	 * 
	 */
	private void linkEquipmentToExperiment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasApparatus(), 
					"Experiment"+UNDERSCORE+experimentInstanceId, "Apparatus"+UNDERSCORE+apparatusID);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between an equipment and an experiment conducted using it.");
		}
	}
	
	private void createKind() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassKind(), 
					"Kind"+UNDERSCORE+apparatusID);
			
			if (kind.getValue() != null && !kind.getValue().trim().isEmpty()) {
				iABoxManagement.addProperty("Kind"+UNDERSCORE+apparatusID, 
	         			ontoChemExpVocabulary.getDataPropertyhasValue(), 
	         			kind.getValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Kind"+UNDERSCORE+apparatusID, dataPropertyIRI, kind.getValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of kind could not be created.");
		}
	}

	private void linkKindToEquipment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasKind(), 
					"Apparatus"+UNDERSCORE+apparatusID, "Kind"+UNDERSCORE+apparatusID);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between a kind and an equipment conducted using it.");
		}
	}
	
	/**
	 * Following the detection of a mode element, it creates 
	 * an instance of Mode in OWL.
	 * 
	 */
	private void createMode() {
		modeCount += 1;
		
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassMode(), 
					"Mode"+UNDERSCORE+apparatusID+UNDERSCORE+modeCount);
			if (mode.getValue() != null && !mode.getValue().trim().isEmpty()) {
				iABoxManagement.addProperty("Mode"+UNDERSCORE+apparatusID+UNDERSCORE+modeCount, 
	         			ontoChemExpVocabulary.getDataPropertyhasValue(), 
	         			mode.getValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Mode"+UNDERSCORE+apparatusID+UNDERSCORE+modeCount, dataPropertyIRI, mode.getValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of Mode could not be created.");
		}
	}
	
	/**
	 * Links a mode to the equipment.
	 * 
	 */
	private void linkModeToEquipment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasMode(), 
					"Apparatus"+UNDERSCORE+apparatusID, "Mode"+UNDERSCORE+apparatusID+UNDERSCORE+modeCount);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between an equipment and its mode.");
		}
	}
	
	private void createProperty() {
		apparatusPropertyCount += 1;
		
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassProperty(), 
					"Property"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount);
			
			if (apparatusProperty.getPropertyName() != null && !apparatusProperty.getPropertyName().trim().isEmpty()) {
				iABoxManagement.addProperty("Property"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasName(), 
						apparatusProperty.getPropertyName(), STRING);
			}
			
			if (apparatusProperty.getPropertyId() != null && !apparatusProperty.getPropertyId().trim().isEmpty()) {
				iABoxManagement.addProperty("Property"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasID(), 
						apparatusProperty.getPropertyId(), STRING);
			}
			
			if (apparatusProperty.getPropertyLabel() != null && !apparatusProperty.getPropertyLabel().trim().isEmpty()) {
				iABoxManagement.addProperty("Property"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasLabel(), 
						apparatusProperty.getPropertyLabel(), STRING);
			}
			
			if (apparatusProperty.getPropertyUnits() != null && !apparatusProperty.getPropertyUnits().trim().isEmpty()) {
				iABoxManagement.addProperty("Property"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasUnits(), 
						apparatusProperty.getPropertyUnits(), STRING);
			}
			
			if (apparatusProperty.getPropertyDescription() != null && !apparatusProperty.getPropertyDescription().trim().isEmpty()) {
				iABoxManagement.addProperty("Property"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasDescription(), 
						apparatusProperty.getPropertyDescription(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of Property could not be created.");
		}
	}
	
	private void linkPropertyToEquipment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasProperty(), 
					"Apparatus"+UNDERSCORE+apparatusID, "Property"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between an equipment and its apparatus property.");
		}
	}
	
	
	private void createPropertyValue() {
		try {
//			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassValue(), 
//					"Value"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount);
			
			if (apparatusPropertyValue.getValueValue() != null && !apparatusPropertyValue.getValueValue().trim().isEmpty()) {
				iABoxManagement.addProperty(currentDQInstance, 
	         			ontoChemExpVocabulary.getDataPropertyhasValue(), 
	         			apparatusPropertyValue.getValueValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Value"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
//						dataPropertyIRI, apparatusPropertyValue.getValueValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of PropertyValue could not be created.");
		}
	}
	
//	private void linkPropertyValueToProperty() {
//		try {
//			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasValue(), 
//					"Property"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, "Value"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount);
//		} catch (ABoxManagementException e) {
//			logger.error(
//					"A link could not be established between the apparatus property and its value.");
//		}
//	}
	
	private void createPropertyUncertainty() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassUncertainty(), 
					"Uncertainty"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount);
			
			if (apparatusPropertyUncertainty.getUncertaintyBound() != null && !apparatusPropertyUncertainty.getUncertaintyBound().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasBound(), 
						apparatusPropertyUncertainty.getUncertaintyBound(), STRING);
			}
			
			if (apparatusPropertyUncertainty.getUncertaintyKind() != null && !apparatusPropertyUncertainty.getUncertaintyKind().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasKind(), 
						apparatusPropertyUncertainty.getUncertaintyKind(), STRING);
			}
			
			if (apparatusPropertyUncertainty.getUncertaintyTransformation() != null && !apparatusPropertyUncertainty.getUncertaintyTransformation().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasTransformation(), 
						apparatusPropertyUncertainty.getUncertaintyTransformation(), STRING);
			}
			
			if (apparatusPropertyUncertainty.getUncertaintyType() != null && !apparatusPropertyUncertainty.getUncertaintyType().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasType(), 
						apparatusPropertyUncertainty.getUncertaintyType(), STRING);
			}
			
			if (apparatusPropertyUncertainty.getUncertaintyValue() != null && !apparatusPropertyUncertainty.getUncertaintyValue().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
	         			ontoChemExpVocabulary.getDataPropertyhasValue(), 
	         			apparatusPropertyUncertainty.getUncertaintyValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, 
//						dataPropertyIRI, apparatusPropertyUncertainty.getUncertaintyValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of PropertyUncertainty could not be created.");
		}
	}
	
	private void linkPropertyUncertaintyToProperty() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(), 
					"Property"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount, "Uncertainty"+UNDERSCORE+apparatusID+UNDERSCORE+apparatusPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between the apparatus property and its uncertainty.");
		}
	}
}
