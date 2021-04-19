
package com.cmclinnovations.ontochemexp.model.owl;

import static com.cmclinnovations.ontochemexp.model.owl.IApparatusWriter.logger;
import static com.cmclinnovations.ontochemexp.model.owl.IBibliographyLinkWriter.logger;
import static com.cmclinnovations.ontochemexp.model.owl.ICopyrightWriter.logger;

import java.util.ArrayList;

import org.apache.commons.lang3.text.WordUtils;
import org.semanticweb.owlapi.model.IRI;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.ApparatusProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonProperties;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonPropertiesProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Amount;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Component;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.SpeciesLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Uncertainty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Value;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.CommonPropertiesParseStatus;
import com.cmclinnovations.ontochemexp.model.utils.PrimeConverterUtils;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

/**
 * Implements the method that forwards a call to those methods that
 * read CommonProperties data from an in-memory temporary storage 
 * to pass them to the corresponding PrIMe to OWL conversion methods.
 * 
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class CommonPropertiesWriter extends PrimeConverter implements ICommonPropertiesWriter {
	public void writer(char ch[], int start, int length) throws SAXException {
		readCommonProperties(ch, start, length);
		readCommonPropertiesProperty(ch, start, length);
		readCommonPropertiesPropertyValue(ch, start, length);
//		readCommonPropertiesPropertyUncertainty(ch, start, length);
		readCommonPropertiesPropertyComponent(ch, start, length);
		readCommonPropertiesPropertyComponentSpeciesLink(ch, start, length);
//		readCommonPropertiesPropertyComponentAmount(ch, start, length);
//		readCommonPropertiesPropertyComponentUncertainty(ch, start, length);
	}
	
	
	/**
	 * Forwards the call to the methods that first read and then write 
	 * commonProperties data and metadata.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	private void readCommonProperties(char ch[], int start, int length) throws SAXException {
		if (commonPropertiesParseStatus.isCommonProperties()) {
			createCommonProperties();
			linkCommonPropertiesToExperiment();
			commonPropertiesParseStatus.setCommonProperties(false);
			experiment.setCommonProperties(commonProperties);
			commonProperties = new CommonProperties();
		}
	}
	
	/**
	 * Forwards the call to the methods that first read and then write 
	 * commonProperties data and metadata.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	private void readCommonPropertiesProperty(char ch[], int start, int length) throws SAXException {
		if (commonPropertiesPropertyParseStatus.isProperty()) {
//			createProperty();
//			linkPropertyToEquipment();
			DimensionalQuantityWriter dQ = new DimensionalQuantityWriter(commonPropertiesID, commonPropertiesPropertyCount, "CommonProperties"+UNDERSCORE+commonPropertiesID, 
					commonPropertiesProperty);
			try {
				dQ.createDimensionalQuantityInOWL();
			} catch (OntoChemExpException e) {
				e.printStackTrace();
			}
			commonPropertiesPropertyParseStatus.setProperty(false);
			commonPropertiesPropertyList.add(commonPropertiesProperty);
			commonPropertiesProperty = new CommonPropertiesProperty();
			commonProperties.setProperty(commonPropertiesPropertyList);
			commonPropertiesPropertyList = new ArrayList<CommonPropertiesProperty>();
		}
	}
	
	private void readCommonPropertiesPropertyValue(char ch[], int start, int length) throws SAXException {
		if (commonPropertiesPropertyValueParseStatus.isValue()) {
			String value = new String(ch, start, length);
			commonPropertiesPropertyValue.setValueValue(value);
			createPropertyValue();
//			linkPropertyValueToProperty();
			commonPropertiesPropertyValueParseStatus.setValue(false);
			commonPropertiesProperty.setPropertyValue(commonPropertiesPropertyValue);
			commonPropertiesPropertyValue = new Value();
		}
	}
	
	private void readCommonPropertiesPropertyUncertainty(char ch[], int start, int length) throws SAXException {
		if (commonPropertiesPropertyUncertaintyParseStatus.isUncertainty() && !inCommonPropertiesPropertyComponent) {
			String value = new String(ch, start, length);
			commonPropertiesPropertyUncertainty.setUncertaintyValue(value);
			
			createPropertyUncertainty();
			linkPropertyUncertaintyToProperty();
			commonPropertiesPropertyUncertaintyParseStatus.setUncertainty(false);
			commonPropertiesProperty.setPropertyUncertainty(commonPropertiesPropertyUncertainty);
			commonPropertiesPropertyUncertainty = new Uncertainty();
		}
	}
	
	private void readCommonPropertiesPropertyComponent(char ch[], int start, int length) throws SAXException {
		if (commonPropertiesPropertyComponentParseStatus.isComponent() && inCommonProperties) {
			// Component is only allowed to be linked with InitialComposition
			if (currentDQInstance.startsWith(ontoChemExpVocabulary.getClassInitialComposition())) {
				createPropertyComponent();
				linkPropertyComponentToProperty();
			}
			commonPropertiesPropertyComponentParseStatus.setComponent(false);
			commonPropertiesPropertyComponentList.add(commonPropertiesPropertyComponent);
			commonPropertiesPropertyComponent = new Component();
			commonPropertiesProperty.setComponent(commonPropertiesPropertyComponentList);
			commonPropertiesPropertyComponentList = new ArrayList<Component>();
		}
	}
	
	private void readCommonPropertiesPropertyComponentSpeciesLink(char ch[], int start, int length) throws SAXException {
		if (commonPropertiesPropertyComponentSpeciesLinkParseStatus.isSpeciesLink()) {
			String value = new String(ch, start, length);
			commonPropertiesPropertyComponentSpeciesLink.setSpeciesLinkValue(value);
			createPropertyComponentSpeciesLink();
			linkPropertyComponentSpeciesLinkToPropertyComponent();
			commonPropertiesPropertyComponentSpeciesLinkParseStatus.setSpeciesLink(false);
			commonPropertiesPropertyComponent.setComponentSpeciesLink(commonPropertiesPropertyComponentSpeciesLink);
			commonPropertiesPropertyComponentSpeciesLink = new SpeciesLink();
		}
	}
	
	
	
	private void readCommonPropertiesPropertyComponentAmount(char ch[], int start, int length) throws SAXException {
		if (commonPropertiesPropertyComponentAmountParseStatus.isAmount()) {
			String value = new String(ch, start, length);
			commonPropertiesPropertyComponentAmount.setAmountValue(value);
			createPropertyComponentAmount();
			linkPropertyComponentAmountToPropertyComponent();
			commonPropertiesPropertyComponentAmountParseStatus.setAmount(false);
			commonPropertiesPropertyComponent.setComponentAmount(commonPropertiesPropertyComponentAmount);
			commonPropertiesPropertyComponentAmount = new Amount();
		}
	}
	
	private void readCommonPropertiesPropertyComponentUncertainty(char ch[], int start, int length) throws SAXException {
		if (commonPropertiesPropertyComponentUncertaintyParseStatus.isUncertainty() && inCommonPropertiesPropertyComponent) {
			String value = new String(ch, start, length);
			commonPropertiesPropertyComponentUncertainty.setUncertaintyValue(value);
			createPropertyComponentUncertainty();
			linkPropertyComponentUncertaintyToPropertyComponent();
			commonPropertiesPropertyComponentUncertaintyParseStatus.setUncertainty(false);
			commonPropertiesPropertyComponent.setComponentUncertainty(commonPropertiesPropertyComponentUncertainty);
			commonPropertiesPropertyComponentUncertainty = new Uncertainty();
		}
	}
	
	private void createCommonProperties() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassCommonProperties(), 
					"CommonProperties"+UNDERSCORE+commonPropertiesID);
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of commonProperties could not be created.");
		}
	}
	
	private void linkCommonPropertiesToExperiment() {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasCommonProperties(), 
						"Experiment"+UNDERSCORE+experimentInstanceId,"CommonProperties"+UNDERSCORE+commonPropertiesID);
			} catch (ABoxManagementException e) {
				logger.error(
						"A link could not be established between a commonProperties and an experiment.");
			}
	}
	
	private void createProperty() {
		commonPropertiesPropertyCount += 1;
		
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassProperty(), 
					"Property"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount);
			
			if (commonPropertiesProperty.getPropertyName() != null && !commonPropertiesProperty.getPropertyName().trim().isEmpty()) {
				iABoxManagement.addProperty("Property"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasName(), 
						commonPropertiesProperty.getPropertyName(), STRING);
			}
			
			if (commonPropertiesProperty.getPropertyId() != null && !commonPropertiesProperty.getPropertyId().trim().isEmpty()) {
				iABoxManagement.addProperty("Property"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasID(), 
						commonPropertiesProperty.getPropertyId(), STRING);
			}
			
			if (commonPropertiesProperty.getPropertyLabel() != null && !commonPropertiesProperty.getPropertyLabel().trim().isEmpty()) {
				iABoxManagement.addProperty("Property"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasLabel(), 
						commonPropertiesProperty.getPropertyLabel(), STRING);
			}
			
			if (commonPropertiesProperty.getPropertyUnits() != null && !commonPropertiesProperty.getPropertyUnits().trim().isEmpty()) {
				iABoxManagement.addProperty("Property"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasUnits(), 
						commonPropertiesProperty.getPropertyUnits(), STRING);
			}
			
			if (commonPropertiesProperty.getPropertyDescription() != null && !commonPropertiesProperty.getPropertyDescription().trim().isEmpty()) {
				iABoxManagement.addProperty("Property"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount, 
						ontoChemExpVocabulary.getDataPropertyhasDescription(), 
						commonPropertiesProperty.getPropertyDescription(), STRING);
			}
			
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of Property could not be created.");
		}
	}
	
	private void linkPropertyToEquipment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasProperty(), 
					"CommonProperties"+UNDERSCORE+commonPropertiesID, 
					"Property"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between an equipment and its commonProperties property.");
		}
	}
	
	private void createPropertyValue() {
		try {
//			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassValue(), 
//					"Value"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount);
//			
			if (commonPropertiesPropertyValue.getValueValue() != null && !commonPropertiesPropertyValue.getValueValue().trim().isEmpty()) {
				iABoxManagement.addProperty(currentDQInstance, 
						ontoChemExpVocabulary.getDataPropertyhasValue(), 
						commonPropertiesPropertyValue.getValueValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Value"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount, 
//						dataPropertyIRI, commonPropertiesPropertyValue.getValueValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of PropertyValue could not be created.");
		}
	}
	
//	private void linkPropertyValueToProperty() {
//		try {
//			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasValue(), 
//					"Property"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount, 
//					"Value"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount);
//		} catch (ABoxManagementException e) {
//			logger.error(
//					"A link could not be established between the apparatus property and its value.");
//		}
//	}
	
	private void createPropertyUncertainty() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassUncertainty(), 
					"Uncertainty"+UNDERSCORE+(commonPropertiesID+commonPropertiesPropertyCount));
			
			if (commonPropertiesPropertyUncertainty.getUncertaintyBound() != null && !commonPropertiesPropertyUncertainty.getUncertaintyBound().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+(commonPropertiesID+commonPropertiesPropertyCount), 
						ontoChemExpVocabulary.getDataPropertyhasBound(), 
						commonPropertiesPropertyUncertainty.getUncertaintyBound(), STRING);
			}
			
			if (commonPropertiesPropertyUncertainty.getUncertaintyKind() != null && !commonPropertiesPropertyUncertainty.getUncertaintyKind().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+(commonPropertiesID+commonPropertiesPropertyCount), 
						ontoChemExpVocabulary.getDataPropertyhasKind(), 
						commonPropertiesPropertyUncertainty.getUncertaintyKind(), STRING);
			}
			
			if (commonPropertiesPropertyUncertainty.getUncertaintyTransformation() != null && !commonPropertiesPropertyUncertainty.getUncertaintyTransformation().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+(commonPropertiesID+commonPropertiesPropertyCount), 
						ontoChemExpVocabulary.getDataPropertyhasTransformation(), 
						commonPropertiesPropertyUncertainty.getUncertaintyTransformation(), STRING);
			}
			
			if (commonPropertiesPropertyUncertainty.getUncertaintyType() != null && !commonPropertiesPropertyUncertainty.getUncertaintyType().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+(commonPropertiesID+commonPropertiesPropertyCount), 
						ontoChemExpVocabulary.getDataPropertyhasType(), 
						commonPropertiesPropertyUncertainty.getUncertaintyType(), STRING);
			}
			
			if (commonPropertiesPropertyUncertainty.getUncertaintyValue() != null && !commonPropertiesPropertyUncertainty.getUncertaintyValue().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+(commonPropertiesID+commonPropertiesPropertyCount), 
						ontoChemExpVocabulary.getDataPropertyhasValue(), 
						commonPropertiesPropertyUncertainty.getUncertaintyValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+(commonPropertiesID+commonPropertiesPropertyCount), 
//						dataPropertyIRI, commonPropertiesPropertyUncertainty.getUncertaintyValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of PropertyUncertainty could not be created.");
		}
	}
	
	private void linkPropertyUncertaintyToProperty() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(), 
					"Property"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount, "Uncertainty"+UNDERSCORE+(commonPropertiesID+commonPropertiesPropertyCount));
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between the apparatus property and its uncertainty.");
		}
	}
	
	private void createPropertyComponent() {
		componentCount += 1;
		
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassComponent(), 
					"Component"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount);
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of PropertyComponent could not be created.");
		}
	}
	
	private void linkPropertyComponentToProperty() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasComponent(), 
					currentDQInstance, 
					"Component"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between the apparatus property and its component.");
		}
	}
	
	private void createPropertyComponentSpeciesLink() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassSpeciesLink(), 
					"SpeciesLink"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount);
			
			if (commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkPreferredKey() != null 
					&& !commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkPreferredKey().trim().isEmpty()) {
				iABoxManagement.addProperty("SpeciesLink"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasPreferredKey(), 
						commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkPreferredKey(), STRING);
			}
			
			if (commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkPrimeID() != null 
					&& !commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkPrimeID().trim().isEmpty()) {
//				iABoxManagement.addProperty("SpeciesLink"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
//						ontoChemExpVocabulary.getDataPropertyhasPrimeID(), 
//						commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkPrimeID(), STRING);
				
				String uniqueSpeciesIRI;
				try {
					uniqueSpeciesIRI = PrimeConverterUtils.retrieveSpeciesIRI(ontoChemExpKB.getOntoSpeciesUniqueSpeciesIRIKBAboxIRI()
							.concat(commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkPrimeID()));
					if (uniqueSpeciesIRI.trim() != null && !uniqueSpeciesIRI.trim().isEmpty()) {
						iABoxManagement.addProperty("SpeciesLink"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
							ontoChemExpVocabulary.getDataPropertyhasUniqueSpeciesIRI(), 
							uniqueSpeciesIRI, STRING);
					}
				} catch (OntoChemExpException e) {
					logger.error("The uniqueSpeciesIRI could not be retrieved.");
					e.printStackTrace();
				}
			}
			
			if (commonPropertiesPropertyComponentSpeciesLink.getCas() != null 
					&& !commonPropertiesPropertyComponentSpeciesLink.getCas().trim().isEmpty()) {
				iABoxManagement.addProperty("SpeciesLink"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasCAS(), 
						commonPropertiesPropertyComponentSpeciesLink.getCas(), STRING);
			}
			
			if (commonPropertiesPropertyComponentSpeciesLink.getInchi() != null 
					&& !commonPropertiesPropertyComponentSpeciesLink.getInchi().trim().isEmpty()) {
				iABoxManagement.addProperty("SpeciesLink"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasInChI(), 
						commonPropertiesPropertyComponentSpeciesLink.getInchi(), STRING);
			}
			
			if (commonPropertiesPropertyComponentSpeciesLink.getSmiles() != null 
					&& !commonPropertiesPropertyComponentSpeciesLink.getSmiles().trim().isEmpty()) {
				iABoxManagement.addProperty("SpeciesLink"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasSMILES(), 
						commonPropertiesPropertyComponentSpeciesLink.getSmiles(), STRING);
			}
			
			if (commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkValue() != null 
					&& !commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkValue().trim().isEmpty()) {
				iABoxManagement.addProperty("SpeciesLink"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasValue(), commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("SpeciesLink"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
//						dataPropertyIRI, commonPropertiesPropertyComponentSpeciesLink.getSpeciesLinkValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of PropertyComponentSpeciesLink could not be created.");
		}
	}
	
	private void linkPropertyComponentSpeciesLinkToPropertyComponent() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasSpeciesLink(), 
					"Component"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
					"SpeciesLink"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between the component and its speciesLink.");
		}
	}
	
	private void createPropertyComponentAmount() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassAmount(), 
					"Amount"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount);
			
			if (commonPropertiesPropertyComponentAmount.getAmountUnits() != null 
					&& !commonPropertiesPropertyComponentAmount.getAmountUnits().trim().isEmpty()) {
				iABoxManagement.addProperty("Amount"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasUnits(), 
						commonPropertiesPropertyComponentAmount.getAmountUnits(), STRING);
			}
			
			if (commonPropertiesPropertyComponentAmount.getAmountValue() != null 
					&& !commonPropertiesPropertyComponentAmount.getAmountValue().trim().isEmpty()) {
				iABoxManagement.addProperty("Amount"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasValue(), commonPropertiesPropertyComponentAmount.getAmountValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Amount"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
//						dataPropertyIRI, commonPropertiesPropertyComponentAmount.getAmountValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of PropertyComponentAmount could not be created.");
		}
	}
	
	private void linkPropertyComponentAmountToPropertyComponent() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasAmount(), 
					"Component"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
					"Amount"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between the component and its amount.");
		}
	}
	
	private void createPropertyComponentUncertainty() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassUncertainty(), 
					"Uncertainty"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount);
			
			if (commonPropertiesPropertyComponentUncertainty.getUncertaintyBound() != null && !commonPropertiesPropertyComponentUncertainty.getUncertaintyBound().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasBound(), 
						commonPropertiesPropertyComponentUncertainty.getUncertaintyBound(), STRING);
			}
			
			if (commonPropertiesPropertyComponentUncertainty.getUncertaintyKind() != null && !commonPropertiesPropertyComponentUncertainty.getUncertaintyKind().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasKind(), 
						commonPropertiesPropertyComponentUncertainty.getUncertaintyKind(), STRING);
			}
			
			if (commonPropertiesPropertyComponentUncertainty.getUncertaintyTransformation() != null && !commonPropertiesPropertyComponentUncertainty.getUncertaintyTransformation().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasTransformation(), 
						commonPropertiesPropertyComponentUncertainty.getUncertaintyTransformation(), STRING);
			}
			
			if (commonPropertiesPropertyComponentUncertainty.getUncertaintyType() != null && !commonPropertiesPropertyComponentUncertainty.getUncertaintyType().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasType(), 
						commonPropertiesPropertyComponentUncertainty.getUncertaintyType(), STRING);
			}
			
			if (commonPropertiesPropertyComponentUncertainty.getUncertaintyValue() != null && !commonPropertiesPropertyComponentUncertainty.getUncertaintyValue().trim().isEmpty()) {
				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
						ontoChemExpVocabulary.getDataPropertyhasValue(), 
						commonPropertiesPropertyComponentUncertainty.getUncertaintyValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+(commonPropertiesID+commonPropertiesPropertyCount), 
//						dataPropertyIRI, commonPropertiesPropertyUncertainty.getUncertaintyValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of PropertyComponentUncertainty could not be created.");
		}
	}
	
	private void linkPropertyComponentUncertaintyToPropertyComponent() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(), 
					"Component"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount, 
					"Uncertainty"+UNDERSCORE+commonPropertiesID+UNDERSCORE+commonPropertiesPropertyCount+UNDERSCORE+componentCount);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between the component and its uncertainty.");
		}
	}
}
