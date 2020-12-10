
package com.cmclinnovations.ontochemexp.model.owl;

import static com.cmclinnovations.ontochemexp.model.owl.IApparatusWriter.logger;
import static com.cmclinnovations.ontochemexp.model.owl.ICommonPropertiesWriter.logger;
import static com.cmclinnovations.ontochemexp.model.owl.IDataPointWriter.logger;

import java.util.ArrayList;
import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroup;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupDataGroupLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupDataPoint;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupPropertyComponent;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.DataGroupDataPointX;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X1;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X10;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X11;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X2;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X3;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X4;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X5;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X6;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X7;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X8;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X9;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Component;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.DataAttributeLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.DerivedProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Feature;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Indicator;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Observable;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.PropertyLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.SpeciesLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Uncertainty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Value;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

/**
 * Implements the method that forwards a call to those methods that read
 * DataGroup data from an in-memory temporary storage to pass them to the
 * corresponding PrIMe to OWL conversion methods.
 * 
 * 
 * @author Songyi Deng (sd626@cam.ac.uk)
 *
 */

public class DataGroupWriter extends PrimeConverter implements IDataGroupWriter {
	public void writer(char ch[], int start, int length) throws SAXException {
		readDataGroup(ch, start, length);

		readDataGroupDataGroupLink(ch, start, length);

		readDataGroupProperty(ch, start, length);
		readDataGroupPropertyValue(ch, start, length);
		readDataGroupPropertyUncertainty(ch, start, length);
		readDataGroupPropertyComponent(ch, start, length);
		readDataGroupPropertyComponentSpeciesLink(ch, start, length);
		readDataGroupPropertySpeciesLink(ch, start, length);
		readDataGroupPropertyDerivedProperty(ch, start, length);
		readDataGroupPropertyDerivedPropertyFeature(ch, start, length);
		readDataGroupPropertyDerivedPropertyFeatureIndicator(ch, start, length);
		readDataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink(ch, start, length);
		readDataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink(ch, start, length);
		readDataGroupPropertyDerivedPropertyFeatureObservable(ch, start, length);

		readDataGroupDataPoint(ch, start, length);
		readDataGroupDataPointX(ch, start, length);
	}

	/**
	 * Forwards the call to the methods that first read and then write DataGroup
	 * data and metadata.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	private void readDataGroup(char ch[], int start, int length) throws SAXException {
		if (dataGroupParseStatus.isDataGroup()) {
			createDataGroup();
			linkDataGroupToExperiment();
			dataGroupParseStatus.setDataGroup(false);
			dataGroupList.add(dataGroup);
			dataGroup = new DataGroup();
			experiment.setDataGroup(dataGroupList);
			dataGroupList = new ArrayList<DataGroup>();
		}
	}

	private void readDataGroupDataGroupLink(char ch[], int start, int length) throws SAXException {
		if (dataGroupDataGroupLinkParseStatus.isDataGroupLink()) {
			String value = new String(ch, start, length);
			dataGroupDataGroupLink.setValue(value);
			createDataGroupDataGroupLink();
			linkDataGroupLinkToDataGroup();
			dataGroupDataGroupLinkParseStatus.setDataGroupLink(false);
			dataGroup.setDataGroupLink(dataGroupDataGroupLink);
			dataGroupDataGroupLink = new DataGroupDataGroupLink();
		}
	}

	private void readDataGroupProperty(char ch[], int start, int length) throws SAXException {
		if (dataGroupPropertyParseStatus.isProperty()) {
			createDataGroupProperty();
			linkDataGroupPropertyToDataGroup();
			dataGroupPropertyParseStatus.setProperty(false);
			dataGroupPropertyList.add(dataGroupProperty);
			dataGroupProperty = new DataGroupProperty();
			dataGroup.setProperty(dataGroupPropertyList);
			dataGroupPropertyList = new ArrayList<DataGroupProperty>();
		}
	}

	private void readDataGroupPropertyValue(char ch[], int start, int length) throws SAXException {
		if (dataGroupPropertyValueParseStatus.isValue()) {
			String value = new String(ch, start, length);
			dataGroupPropertyValue.setValueValue(value);
			createPropertyValue();
			linkPropertyValueToProperty();
			dataGroupPropertyValueParseStatus.setValue(false);
			dataGroupProperty.setPropertyValue(dataGroupPropertyValue);
			dataGroupPropertyValue = new Value();
		}
	}

	private void readDataGroupPropertyUncertainty(char ch[], int start, int length) throws SAXException {
		if (dataGroupPropertyUncertaintyParseStatus.isUncertainty()) {
			createPropertyUncertainty();
			linkPropertyUncertaintyToProperty();
		}
	}

	public void writeUncertaintyValue() {
		if (dataGroupPropertyUncertainty.getUncertaintyValue() != null
				&& !dataGroupPropertyUncertainty.getUncertaintyValue().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(),
						dataGroupPropertyUncertainty.getUncertaintyValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Uncertainty"+UNDERSCORE+(dataGroupID+dataGroupCount)+UNDERSCORE+dataGroupPropertyCount, 
//						dataPropertyIRI, dataGroupPropertyUncertainty.getUncertaintyValue(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
	}

	public void setUPUncertainty() {
		dataGroupPropertyUncertaintyParseStatus.setUncertainty(false);
		dataGroupProperty.setPropertyUncertainty(dataGroupPropertyUncertainty);
		dataGroupPropertyUncertainty = new Uncertainty();
	}

	private void readDataGroupPropertyComponent(char ch[], int start, int length) throws SAXException {
		if (dataGroupPropertyComponentParseStatus.isComponent() && inDataGroup) {
			createPropertyComponent();
			linkPropertyComponentToProperty();
		}
	}
	
	private void readDataGroupPropertyComponentSpeciesLink(char ch[], int start, int length) throws SAXException {
		if (dataGroupPropertyComponentParseStatus.isComponentSpeciesLink() ) {
			checkComponentValue();
			createPropertyComponentSpeciesLink();
			linkPropertyComponentSpeciesLinkToComponent();
		}
	}
	
	private void readDataGroupPropertySpeciesLink(char ch[], int start, int length) throws SAXException {
		if (dataGroupPropertySpeciesLinkParseStatus.isSpeciesLink()) {
			String value = new String(ch, start, length);
			dataGroupPropertySpeciesLink.setSpeciesLinkValue(value);
			createPropertySpeciesLink();
			linkPropertySpeciesLinkToProperty();
			dataGroupPropertySpeciesLinkParseStatus.setSpeciesLink(false);
			dataGroupProperty.setPropertySpeciesLink(dataGroupPropertySpeciesLink);
			dataGroupPropertySpeciesLink = new SpeciesLink();
		}
	}

	private void readDataGroupPropertyDerivedProperty(char ch[], int start, int length) throws SAXException {
		if (dataGroupPropertyDerivedPropertyParseStatus.isDerivedProperty()) {
			createPropertyDerivedProperty();
			linkPropertyDerivedPropertyToProperty();
			dataGroupPropertyDerivedPropertyParseStatus.setDerivedProperty(false);
			dataGroupProperty.setPropertyDerivedProperty(dataGroupPropertyDerivedProperty);
			dataGroupPropertyDerivedProperty = new DerivedProperty();
		}
	}

	private void readDataGroupPropertyDerivedPropertyFeature(char ch[], int start, int length) throws SAXException {
		if (dataGroupPropertyDerivedPropertyFeatureParseStatus.isFeature()) {
			createPropertyDerivedPropertyFeature();
			linkFeatureToPropertyDerivedProperty();
			dataGroupPropertyDerivedPropertyFeatureParseStatus.setFeature(false);
			dataGroupPropertyDerivedProperty.setDerivedPropertyFeature(dataGroupPropertyDerivedPropertyFeature);
			dataGroupPropertyDerivedPropertyFeature = new Feature();
		}
	}

	private void readDataGroupPropertyDerivedPropertyFeatureIndicator(char ch[], int start, int length)
			throws SAXException {
		if (dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus.isIndicator()) {
			createPropertyDerivedPropertyFeatureIndicator();
			linkIndicatorToPropertyDerivedPropertyFeature();
			dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus.setIndicator(false);
			dataGroupPropertyDerivedPropertyFeatureIndicatorList.add(dataGroupPropertyDerivedPropertyFeatureIndicator);
			dataGroupPropertyDerivedPropertyFeatureIndicator = new Indicator();
			dataGroupPropertyDerivedPropertyFeature
					.setFeatureIndicator(dataGroupPropertyDerivedPropertyFeatureIndicatorList);
			dataGroupPropertyDerivedPropertyFeatureIndicatorList = new ArrayList<Indicator>();
		}
	}

	private void readDataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink(char ch[], int start, int length)
			throws SAXException {
		if (dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkParseStatus.isPropertyLink()) {
			String value = new String(ch, start, length);
			dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.setPropertyLinkValue(value);
			createPropertyDerivedPropertyFeatureIndicatorPropertyLink();
			linkPropertyLinkToPropertyDerivedPropertyFeatureIndicator();
			dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkParseStatus.setPropertyLink(false);
			dataGroupPropertyDerivedPropertyFeatureIndicator
					.setIndicatorPropertyLink(dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink);
			dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink = new PropertyLink();
		}
	}

	private void readDataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink(char ch[], int start, int length)
			throws SAXException {
		if (dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkParseStatus.isDataAttributeLink()) {
			String value = new String(ch, start, length);
			dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.setDataAttributeLinkValue(value);
			createPropertyDerivedPropertyFeatureIndicatorDataAttributeLink();
			linkDataAttributeLinkToPropertyDerivedPropertyFeatureIndicator();
			dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkParseStatus.setDataAttributeLink(false);
			dataGroupPropertyDerivedPropertyFeatureIndicator
					.setIndicatorDataAttriuteLink(dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink);
			dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink = new DataAttributeLink();
		}
	}

	private void readDataGroupPropertyDerivedPropertyFeatureObservable(char ch[], int start, int length)
			throws SAXException {
		if (dataGroupPropertyDerivedPropertyFeatureObservableParseStatus.isObservable()) {
			String value = new String(ch, start, length);
			dataGroupPropertyDerivedPropertyFeatureObservable.setObservableValue(value);
			createPropertyDerivedPropertyFeatureObservable();
			linkObservableToPropertyDerivedPropertyFeature();
			dataGroupPropertyDerivedPropertyFeatureObservableParseStatus.setObservable(false);
			dataGroupPropertyDerivedPropertyFeature
					.setFeatureObservable(dataGroupPropertyDerivedPropertyFeatureObservable);
			dataGroupPropertyDerivedPropertyFeatureObservable = new Observable();
		}
	}

	private void readDataGroupDataPoint(char ch[], int start, int length) throws SAXException {
		if (dataGroupDataPointParseStatus.isDataPoint()) {
			createDataGroupDataPoint();
			linkDataPointToDataGroup();
			dataGroupDataPointParseStatus.setDataPoint(false);
			dataGroupDataPointList.add(dataGroupDataPoint);
			dataGroupDataPoint = new DataGroupDataPoint();
			dataGroup.setDataPoint(dataGroupDataPointList);
			dataGroupDataPointList = new ArrayList<DataGroupDataPoint>();
		}
	}

	private void readDataGroupDataPointX(char ch[], int start, int length) throws SAXException {
		if (x1ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x1.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX1();
				linkDataPointX1ToDataPoint();
			}
		}
		if (x2ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x2.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX2();
				linkDataPointX2ToDataPoint();
			}
		}
		if (x3ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x3.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX3();
				linkDataPointX3ToDataPoint();
			}
		}
		if (x4ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x4.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX4();
				linkDataPointX4ToDataPoint();
			}
		}
		if (x5ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x5.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX5();
				linkDataPointX5ToDataPoint();
			}
		}
		if (x6ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x6.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX6();
				linkDataPointX6ToDataPoint();
			}
		}
		if (x7ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x7.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX7();
				linkDataPointX7ToDataPoint();
			}
		}
		if (x8ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x8.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX8();
				linkDataPointX8ToDataPoint();
			}
		}
		if (x9ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x9.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX9();
				linkDataPointX9ToDataPoint();
			}
		}
		if (x10ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x10.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX10();
				linkDataPointX10ToDataPoint();
			}
		}
		if (x11ParseStatus.isX()) {
			String value = multiLineValue.toString();
			List<Object> items = new ArrayList<Object>();
			items.add(value);
			
			x11.setItems(items);
			if (value != null && !value.trim().isEmpty()) {
				createDataGroupDataPointX11();
				linkDataPointX11ToDataPoint();
			}
		}
	}

	public void readDataGroupDataPointXUncertainty(String qName) throws SAXException {
		if (xUncertaintyParseStatus.isUncertainty()
				&& (x1ParseStatus.isX() || x2ParseStatus.isX() || x3ParseStatus.isX() || x4ParseStatus.isX()
						|| x5ParseStatus.isX() || x6ParseStatus.isX() || x7ParseStatus.isX() || x8ParseStatus.isX()
						|| x9ParseStatus.isX() || x10ParseStatus.isX() || x11ParseStatus.isX())) {
			createXUncertainty();
			linkXUncertaintyToX();
		}
	}

	private void createDataGroup() {
		dataGroupCount += 1;

		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataGroup(),
					"DataGroup" + UNDERSCORE + (dataGroupID + dataGroupCount));

			if (dataGroup.getId() != null && !dataGroup.getId().trim().isEmpty()) {
				iABoxManagement.addProperty("DataGroup" + UNDERSCORE + (dataGroupID + dataGroupCount),
						ontoChemExpVocabulary.getDataPropertyhasID(), dataGroup.getId(), STRING);
			}

			if (dataGroup.getLabel() != null && !dataGroup.getLabel().trim().isEmpty()) {
				iABoxManagement.addProperty("DataGroup" + UNDERSCORE + (dataGroupID + dataGroupCount),
						ontoChemExpVocabulary.getDataPropertyhasLabel(), dataGroup.getLabel(), STRING);
			}

			if (dataGroup.getDataPointForm() != null && !dataGroup.getDataPointForm().trim().isEmpty()) {
				iABoxManagement.addProperty("DataGroup" + UNDERSCORE + (dataGroupID + dataGroupCount),
						ontoChemExpVocabulary.getDataPropertyhasDataPointForm(), dataGroup.getDataPointForm(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroup could not be created.");
		}
	}

	/**
	 * Links the result to the experiment.
	 * 
	 */
	private void linkDataGroupToExperiment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataGroup(),
					"Experiment" + UNDERSCORE + experimentInstanceId,
					"DataGroup" + UNDERSCORE + (dataGroupID + dataGroupCount));
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroup and an experiment.");
		}
	}

	private void createDataGroupDataGroupLink() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataGroupLink(),
					"DataGroupLink" + UNDERSCORE + (dataGroupID + dataGroupCount));

			if (dataGroupDataGroupLink.getDataGroupID() != null
					&& !dataGroupDataGroupLink.getDataGroupID().trim().isEmpty()) {
				iABoxManagement.addProperty("DataGroupLink" + UNDERSCORE + (dataGroupID + dataGroupCount),
						ontoChemExpVocabulary.getDataPropertyhasDataGroupID(), dataGroupDataGroupLink.getDataGroupID(),
						STRING);
			}

			if (dataGroupDataGroupLink.getDataPointID() != null
					&& !dataGroupDataGroupLink.getDataPointID().trim().isEmpty()) {
				iABoxManagement.addProperty("DataGroupLink" + UNDERSCORE + (dataGroupID + dataGroupCount),
						ontoChemExpVocabulary.getDataPropertyhasDataPointID(), dataGroupDataGroupLink.getDataPointID(),
						STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupLink could not be created.");
		}
	}

	private void linkDataGroupLinkToDataGroup() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataGroupLink(),
					"DataGroup" + UNDERSCORE + (dataGroupID + dataGroupCount),
					"DataGroupLink" + UNDERSCORE + (dataGroupID + dataGroupCount));
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupLink and a dataGroup.");
		}
	}

	private void createDataGroupProperty() {
		dataGroupPropertyCount += 1;

		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassProperty(),
					"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);

			if (dataGroupProperty.getPropertyName() != null && !dataGroupProperty.getPropertyName().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasName(), dataGroupProperty.getPropertyName(), STRING);
			}

			if (dataGroupProperty.getPropertyId() != null && !dataGroupProperty.getPropertyId().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasID(), dataGroupProperty.getPropertyId(), STRING);
			}

			if (dataGroupProperty.getPropertyLabel() != null
					&& !dataGroupProperty.getPropertyLabel().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasLabel(), dataGroupProperty.getPropertyLabel(), STRING);
			}

			if (dataGroupProperty.getPropertyUnits() != null
					&& !dataGroupProperty.getPropertyUnits().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasUnits(), dataGroupProperty.getPropertyUnits(), STRING);
			}

			if (dataGroupProperty.getPropertyDescription() != null
					&& !dataGroupProperty.getPropertyDescription().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasDescription(),
						dataGroupProperty.getPropertyDescription().replace("\r", " ").replace("\n", ""), STRING);
			}

			if (dataGroupProperty.getPropertyDerivedPropertyExists() != null
					&& !dataGroupProperty.getPropertyDerivedPropertyExists().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasDerivedPropertyExists(),
						dataGroupProperty.getPropertyDerivedPropertyExists(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of Property could not be created.");
		}
	}

	private void linkDataGroupPropertyToDataGroup() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasProperty(),
					"DataGroup" + UNDERSCORE + (dataGroupID + dataGroupCount),
					"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between an equipment and its dataGroup property.");
		}
	}

	private void createPropertyValue() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassValue(),
					"Value" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);

			if (dataGroupPropertyValue.getValueValue() != null
					&& !dataGroupPropertyValue.getValueValue().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Value" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), dataGroupPropertyValue.getValueValue(),
						STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Value"+UNDERSCORE+(dataGroupID+dataGroupCount)+UNDERSCORE+dataGroupPropertyCount, 
//						dataPropertyIRI, dataGroupPropertyValue.getValueValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyValue could not be created.");
		}
	}

	private void linkPropertyValueToProperty() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasValue(),
					"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
					"Value" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the apparatus property and its value.");
		}
	}

	private void createPropertyUncertainty() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassUncertainty(),
					"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);

			if (dataGroupPropertyUncertainty.getUncertaintyBound() != null
					&& !dataGroupPropertyUncertainty.getUncertaintyBound().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasBound(),
						dataGroupPropertyUncertainty.getUncertaintyBound(), STRING);
			}

			if (dataGroupPropertyUncertainty.getUncertaintyKind() != null
					&& !dataGroupPropertyUncertainty.getUncertaintyKind().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasKind(),
						dataGroupPropertyUncertainty.getUncertaintyKind(), STRING);
			}

			if (dataGroupPropertyUncertainty.getUncertaintyTransformation() != null
					&& !dataGroupPropertyUncertainty.getUncertaintyTransformation().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasTransformation(),
						dataGroupPropertyUncertainty.getUncertaintyTransformation(), STRING);
			}

			if (dataGroupPropertyUncertainty.getUncertaintyType() != null
					&& !dataGroupPropertyUncertainty.getUncertaintyType().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasType(),
						dataGroupPropertyUncertainty.getUncertaintyType(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyUncertainty could not be created.");
		}
	}

	private void linkPropertyUncertaintyToProperty() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
					"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
					"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the dataGroup property and its uncertainty.");
		}
	}

	private void createPropertyComponent() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassComponent(),
					"Component" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyComponent could not be created.");
		}
	}

	private void linkPropertyComponentToProperty() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasComponent(),
					"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
					"Component" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the dataGroup property and its component.");
		}
	}
	
	private void checkComponentValue() {
		String value = multiLineValue.toString().trim();
		if ((value == null) || (value.isEmpty())) {
			dataGroupPropertyComponentParseStatus.setComponentValue(false);
		}
	}
	
	private void createPropertyComponentSpeciesLink() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassSpeciesLink(),
					"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount);
			
			if (dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPreferredKey() != null 
					&& !dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPreferredKey().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasPreferredKey(),
						dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPreferredKey(), STRING);
			}
			
//			if (dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPrimeID() != null 
//					&& !dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPrimeID().trim().isEmpty()) {
//				iABoxManagement.addProperty(
//						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount,
//						ontoChemExpVocabulary.getDataPropertyhasPrimeID(),
//						dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPrimeID(), STRING);
//			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyComponentSpeciesLink could not be created.");
		}
	}

	private void linkPropertyComponentSpeciesLinkToComponent() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasSpeciesLink(),
					"Component" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
					"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the dataGroup propertyComponent and its speciesLink.");
		}
	}

	private void createPropertySpeciesLink() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassSpeciesLink(),
					"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);

			if (dataGroupPropertySpeciesLink.getSpeciesLinkPreferredKey() != null
					&& !dataGroupPropertySpeciesLink.getSpeciesLinkPreferredKey().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasPreferredKey(),
						dataGroupPropertySpeciesLink.getSpeciesLinkPreferredKey(), STRING);
			}

//			if (dataGroupPropertySpeciesLink.getSpeciesLinkPrimeID() != null
//					&& !dataGroupPropertySpeciesLink.getSpeciesLinkPrimeID().trim().isEmpty()) {
//				iABoxManagement.addProperty(
//						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
//								+ dataGroupPropertyCount,
//						ontoChemExpVocabulary.getDataPropertyhasPrimeID(),
//						dataGroupPropertySpeciesLink.getSpeciesLinkPrimeID(), STRING);
//			}

			if (dataGroupPropertySpeciesLink.getSpeciesLinkValue() != null
					&& !dataGroupPropertySpeciesLink.getSpeciesLinkValue().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(),
						dataGroupPropertySpeciesLink.getSpeciesLinkValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("SpeciesLink"+UNDERSCORE+(dataGroupID+dataGroupCount)+UNDERSCORE+dataGroupPropertyCount, 
//						dataPropertyIRI, dataGroupPropertySpeciesLink.getSpeciesLinkValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyComponentSpeciesLink could not be created.");
		}
	}

	private void linkPropertySpeciesLinkToProperty() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasSpeciesLink(),
					"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
					"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the dataGroup property and its speciesLink.");
		}
	}

	private void createPropertyDerivedProperty() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDerivedProperty(), "DerivedProperty"
					+ UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);

			if (dataGroupPropertyDerivedProperty.getDerivedPropertyName() != null
					&& !dataGroupPropertyDerivedProperty.getDerivedPropertyName().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"DerivedProperty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasName(),
						dataGroupPropertyDerivedProperty.getDerivedPropertyName(), STRING);
			}

			if (dataGroupPropertyDerivedProperty.getDerivedPropertyId() != null
					&& !dataGroupPropertyDerivedProperty.getDerivedPropertyId().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"DerivedProperty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasID(),
						dataGroupPropertyDerivedProperty.getDerivedPropertyId(), STRING);
			}

			if (dataGroupPropertyDerivedProperty.getDerivedPropertyLabel() != null
					&& !dataGroupPropertyDerivedProperty.getDerivedPropertyLabel().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"DerivedProperty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasLabel(),
						dataGroupPropertyDerivedProperty.getDerivedPropertyLabel(), STRING);
			}

			if (dataGroupPropertyDerivedProperty.getDerivedPropertyUnits() != null
					&& !dataGroupPropertyDerivedProperty.getDerivedPropertyUnits().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"DerivedProperty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasUnits(),
						dataGroupPropertyDerivedProperty.getDerivedPropertyUnits(), STRING);
			}

			if (dataGroupPropertyDerivedProperty.getDerivedPropertyDescription() != null
					&& !dataGroupPropertyDerivedProperty.getDerivedPropertyDescription().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"DerivedProperty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasDescription(),
						dataGroupPropertyDerivedProperty.getDerivedPropertyDescription(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyComponentSpeciesLink could not be created.");
		}
	}

	private void linkPropertyDerivedPropertyToProperty() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDerivedProperty(),
					"Property" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
					"DerivedProperty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
							+ dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the dataGroup property and its derivedProperty.");
		}
	}

	private void createPropertyDerivedPropertyFeature() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassFeature(),
					"Feature" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);

			if (dataGroupPropertyDerivedPropertyFeature.getFeatureId() != null
					&& !dataGroupPropertyDerivedPropertyFeature.getFeatureId().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Feature" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasID(),
						dataGroupPropertyDerivedPropertyFeature.getFeatureId(), STRING);
			}

//			if (dataGroupPropertyDerivedPropertyFeature.getFeaturePrimeID() != null
//					&& !dataGroupPropertyDerivedPropertyFeature.getFeaturePrimeID().trim().isEmpty()) {
//				iABoxManagement.addProperty(
//						"Feature" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
//						ontoChemExpVocabulary.getDataPropertyhasPrimeID(),
//						dataGroupPropertyDerivedPropertyFeature.getFeaturePrimeID(), STRING);
//			}

			if (dataGroupPropertyDerivedPropertyFeature.getFeatureType() != null
					&& !dataGroupPropertyDerivedPropertyFeature.getFeatureType().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Feature" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasType(),
						dataGroupPropertyDerivedPropertyFeature.getFeatureType(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyComponentSpeciesLink could not be created.");
		}
	}

	private void linkFeatureToPropertyDerivedProperty() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasFeature(),
					"DerivedProperty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
							+ dataGroupPropertyCount,
					"Feature" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the derivedProperty and its feature.");
		}
	}

	private void createPropertyDerivedPropertyFeatureIndicator() {
		indicatorCount += 1;

		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassIndicator(),
					"Indicator" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount
							+ UNDERSCORE + indicatorCount);

			if (dataGroupPropertyDerivedPropertyFeatureIndicator.getIndicatorId() != null
					&& !dataGroupPropertyDerivedPropertyFeatureIndicator.getIndicatorId().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Indicator" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount
								+ UNDERSCORE + indicatorCount,
						ontoChemExpVocabulary.getDataPropertyhasID(),
						dataGroupPropertyDerivedPropertyFeatureIndicator.getIndicatorId(), STRING);
			}

			if (dataGroupPropertyDerivedPropertyFeatureIndicator.getIndicatorTransformation() != null
					&& !dataGroupPropertyDerivedPropertyFeatureIndicator.getIndicatorTransformation().trim()
							.isEmpty()) {
				iABoxManagement.addProperty(
						"Indicator" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount
								+ UNDERSCORE + indicatorCount,
						ontoChemExpVocabulary.getDataPropertyhasTransformation(),
						dataGroupPropertyDerivedPropertyFeatureIndicator.getIndicatorTransformation(), STRING);
			}

			if (dataGroupPropertyDerivedPropertyFeatureIndicator.getIndicatorVariableID() != null
					&& !dataGroupPropertyDerivedPropertyFeatureIndicator.getIndicatorVariableID().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Indicator" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount
								+ UNDERSCORE + indicatorCount,
						ontoChemExpVocabulary.getDataPropertyhasVariableID(),
						dataGroupPropertyDerivedPropertyFeatureIndicator.getIndicatorVariableID(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyComponentSpeciesLink could not be created.");
		}
	}

	private void linkIndicatorToPropertyDerivedPropertyFeature() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasIndicator(),
					"Feature" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
					"Indicator" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount
							+ UNDERSCORE + indicatorCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the feature and its indicator.");
		}
	}

	private void createPropertyDerivedPropertyFeatureIndicatorPropertyLink() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassPropertyLink(),
					"PropertyLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount
							+ UNDERSCORE + indicatorCount);

			if (dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.getPropertyId() != null
					&& !dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.getPropertyId().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"PropertyLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount + UNDERSCORE + indicatorCount,
						ontoChemExpVocabulary.getDataPropertyhasPropertyID(),
						dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.getPropertyId(), STRING);
			}

			if (dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.getDataGroupID() != null
					&& !dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.getDataGroupID().trim()
							.isEmpty()) {
				iABoxManagement.addProperty(
						"PropertyLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount + UNDERSCORE + indicatorCount,
						ontoChemExpVocabulary.getDataPropertyhasDataGroupID(),
						dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.getDataGroupID(), STRING);
			}

			if (dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.getPropertyLinkValue() != null
					&& !dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.getPropertyLinkValue().trim()
							.isEmpty()) {
				iABoxManagement.addProperty(
						"PropertyLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount + UNDERSCORE + indicatorCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(),
						dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.getPropertyLinkValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("PropertyLink"+UNDERSCORE+(dataGroupID+dataGroupCount)+UNDERSCORE+dataGroupPropertyCount+UNDERSCORE+indicatorCount, 
//						dataPropertyIRI, dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.getPropertyLinkValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyComponentSpeciesLink could not be created.");
		}
	}

	private void linkPropertyLinkToPropertyDerivedPropertyFeatureIndicator() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasPropertyLink(),
					"Indicator" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount
							+ UNDERSCORE + indicatorCount,
					"PropertyLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount
							+ UNDERSCORE + indicatorCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the indicator and its propertyLink.");
		}
	}

	private void createPropertyDerivedPropertyFeatureIndicatorDataAttributeLink() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataAttributeLink(),
					"DataAttributeLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
							+ dataGroupPropertyCount + UNDERSCORE + indicatorCount);

			if (dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.getDataAttributeLinkId() != null
					&& !dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.getDataAttributeLinkId()
							.trim().isEmpty()) {
				iABoxManagement.addProperty(
						"DataAttributeLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount + UNDERSCORE + indicatorCount,
						ontoChemExpVocabulary.getDataPropertyhasID(),
						dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.getDataAttributeLinkId(),
						STRING);
			}

//			if (dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.getDataAttributeLinkPrimeID() != null
//					&& !dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.getDataAttributeLinkPrimeID()
//							.trim().isEmpty()) {
//				iABoxManagement.addProperty(
//						"DataAttributeLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
//								+ dataGroupPropertyCount + UNDERSCORE + indicatorCount,
//						ontoChemExpVocabulary.getDataPropertyhasPrimeID(),
//						dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.getDataAttributeLinkPrimeID(),
//						STRING);
//			}

			if (dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.getDataAttributeLinkValue() != null
					&& !dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.getDataAttributeLinkValue()
							.trim().isEmpty()) {
				iABoxManagement.addProperty(
						"DataAttributeLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount + UNDERSCORE + indicatorCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(),
						dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.getDataAttributeLinkValue(),
						STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("DataAttributeLink"+UNDERSCORE+(dataGroupID+dataGroupCount)+UNDERSCORE+dataGroupPropertyCount+UNDERSCORE+indicatorCount, 
//						dataPropertyIRI, dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.getDataAttributeLinkValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyComponentSpeciesLink could not be created.");
		}
	}

	private void linkDataAttributeLinkToPropertyDerivedPropertyFeatureIndicator() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataAttributeLink(),
					"Indicator" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount
							+ UNDERSCORE + indicatorCount,
					"DataAttributeLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
							+ dataGroupPropertyCount + UNDERSCORE + indicatorCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the indicator and its dataAttributeLink.");
		}
	}

	private void createPropertyDerivedPropertyFeatureObservable() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassObservable(),
					"Observable" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);

			if (dataGroupPropertyDerivedPropertyFeatureObservable.getObservableId() != null
					&& !dataGroupPropertyDerivedPropertyFeatureObservable.getObservableId().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Observable" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasID(),
						dataGroupPropertyDerivedPropertyFeatureObservable.getObservableId(), STRING);
			}

			if (dataGroupPropertyDerivedPropertyFeatureObservable.getObservableVariableID() != null
					&& !dataGroupPropertyDerivedPropertyFeatureObservable.getObservableVariableID().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Observable" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasVariableID(),
						dataGroupPropertyDerivedPropertyFeatureObservable.getObservableVariableID(), STRING);
			}

			if (dataGroupPropertyDerivedPropertyFeatureObservable.getObservableValue() != null
					&& !dataGroupPropertyDerivedPropertyFeatureObservable.getObservableValue().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Observable" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(),
						dataGroupPropertyDerivedPropertyFeatureObservable.getObservableValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Observable"+UNDERSCORE+(dataGroupID+dataGroupCount)+UNDERSCORE+dataGroupPropertyCount, 
//						dataPropertyIRI, dataGroupPropertyDerivedPropertyFeatureObservable.getObservableValue(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyComponentSpeciesLink could not be created.");
		}
	}

	private void linkObservableToPropertyDerivedPropertyFeature() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasObservable(),
					"Feature" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
					"Observable" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the feature and its observable.");
		}
	}

	private void createDataGroupDataPoint() {
		dataGroupDataPointCount += 1;

		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPoint(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);

			if (dataGroupDataPoint.getId() != null && !dataGroupDataPoint.getId().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasID(), dataGroupDataPoint.getId(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPoint could not be created.");
		}
	}

	private void linkDataPointToDataGroup() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPoint(),
					"DataGroup" + UNDERSCORE + (dataGroupID + dataGroupCount),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPoint and a dataGroup.");
		}
	}

	private void createDataGroupDataPointX1() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX1(),
					"X001" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX1ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X001" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void createDataGroupDataPointX2() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX2(),
					"X002" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX2ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X002" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void createDataGroupDataPointX3() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX3(),
					"X003" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX3ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X003" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void createDataGroupDataPointX4() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX4(),
					"X004" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX4ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X004" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void createDataGroupDataPointX5() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX5(),
					"X005" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX5ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X005" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void createDataGroupDataPointX6() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX6(),
					"X006" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX6ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X006" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void createDataGroupDataPointX7() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX7(),
					"X007" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX7ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X007" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void createDataGroupDataPointX8() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX8(),
					"X008" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX8ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X008" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void createDataGroupDataPointX9() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX9(),
					"X009" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX9ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X009" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void createDataGroupDataPointX10() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX10(),
					"X010" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX10ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X010" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void createDataGroupDataPointX11() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassDataPointX11(),
					"X011" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroupDataPointX could not be created.");
		}
	}

	private void linkDataPointX11ToDataPoint() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasDataPointX(),
					"DataPoint" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
					"X011" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a dataGroupDataPointX and a dataGroupDataPoint.");
		}
	}

	private void writeX1() {
		if (x1.getItems().get(0).toString() != null && !x1.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X001" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x1.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void writeX2() {
		if (x2.getItems().get(0).toString() != null && !x2.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X002" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x2.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void writeX3() {
		if (x3.getItems().get(0).toString() != null && !x3.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X003" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x3.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void writeX4() {
		if (x4.getItems().get(0).toString() != null && !x4.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X004" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x4.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void writeX5() {
		if (x5.getItems().get(0).toString() != null && !x5.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X005" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x5.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void writeX6() {
		if (x6.getItems().get(0).toString() != null && !x6.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X006" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x6.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void writeX7() {
		if (x7.getItems().get(0).toString() != null && !x7.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X007" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x7.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void writeX8() {
		if (x8.getItems().get(0).toString() != null && !x8.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X008" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x8.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void writeX9() {
		if (x9.getItems().get(0).toString() != null && !x9.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X009" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x9.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void writeX10() {
		if (x10.getItems().get(0).toString() != null && !x10.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X010" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x10.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void writeX11() {
		if (x11.getItems().get(0).toString() != null && !x11.getItems().get(0).toString().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"X011" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), x11.getItems().get(0).toString(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void createXUncertainty() {
		xUncertaintyCount += 1;
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassUncertainty(),
					"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount
							+ UNDERSCORE + xUncertaintyCount);

			if (xUncertainty.getUncertaintyBound() != null && !xUncertainty.getUncertaintyBound().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount,
						ontoChemExpVocabulary.getDataPropertyhasBound(), xUncertainty.getUncertaintyBound(), STRING);
			}

			if (xUncertainty.getUncertaintyKind() != null && !xUncertainty.getUncertaintyKind().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount,
						ontoChemExpVocabulary.getDataPropertyhasKind(), xUncertainty.getUncertaintyKind(), STRING);
			}

			if (xUncertainty.getUncertaintyTransformation() != null
					&& !xUncertainty.getUncertaintyTransformation().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount,
						ontoChemExpVocabulary.getDataPropertyhasTransformation(),
						xUncertainty.getUncertaintyTransformation(), STRING);
			}

			if (xUncertainty.getUncertaintyType() != null && !xUncertainty.getUncertaintyType().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount,
						ontoChemExpVocabulary.getDataPropertyhasType(), xUncertainty.getUncertaintyType(), STRING);
			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of xUncertainty could not be created.");
		}
	}

	private void linkXUncertaintyToX() {
		if (x1ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X001" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		} else if (x2ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X002" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		} else if (x3ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X003" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		} else if (x4ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X004" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		} else if (x5ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X005" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		} else if (x6ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X006" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		} else if (x7ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X007" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		} else if (x8ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X008" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		} else if (x9ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X009" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		} else if (x10ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X010" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		} else if (x11ParseStatus.isX()) {
			try {
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUncertainty(),
						"X011" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount,
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE
								+ dataGroupDataPointCount + UNDERSCORE + xUncertaintyCount);
			} catch (ABoxManagementException e) {
				logger.error("A link could not be established between a xUncertainty and a dataGroupDataPointX.");
			}
		}

	}

	public void writeX1ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x1.setItems(items);
		writeX1();
		dataGroupDataPoint.setX1(x1);
		x1 = new X1();
		x1ParseStatus.setParsed(true);
	}

	public void writeX2ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x2.setItems(items);
		writeX2();
		dataGroupDataPoint.setX2(x2);
		x2 = new X2();
		x2ParseStatus.setParsed(true);
	}

	public void writeX3ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x3.setItems(items);
		writeX3();
		dataGroupDataPoint.setX3(x3);
		x3 = new X3();
		x3ParseStatus.setParsed(true);
	}

	public void writeX4ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x4.setItems(items);
		writeX4();
		dataGroupDataPoint.setX4(x4);
		x4 = new X4();
		x4ParseStatus.setParsed(true);
	}

	public void writeX5ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x5.setItems(items);
		writeX5();
		dataGroupDataPoint.setX5(x5);
		x5 = new X5();
		x5ParseStatus.setParsed(true);
	}

	public void writeX6ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x6.setItems(items);
		writeX6();
		dataGroupDataPoint.setX6(x6);
		x6 = new X6();
		x6ParseStatus.setParsed(true);
	}

	public void writeX7ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x7.setItems(items);
		writeX7();
		dataGroupDataPoint.setX7(x7);
		x7 = new X7();
		x7ParseStatus.setParsed(true);
	}

	public void writeX8ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x8.setItems(items);
		writeX8();
		dataGroupDataPoint.setX8(x8);
		x8 = new X8();
		x8ParseStatus.setParsed(true);
	}

	public void writeX9ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x9.setItems(items);
		writeX9();
		dataGroupDataPoint.setX9(x9);
		x9 = new X9();
		x9ParseStatus.setParsed(true);
	}

	public void writeX10ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x10.setItems(items);
		writeX10();
		dataGroupDataPoint.setX10(x10);
		x10 = new X10();
		x10ParseStatus.setParsed(true);
	}

	public void writeX11ToOwl() {
		String value = multiLineValue.toString().trim();
		List<Object> items = new ArrayList<Object>();
		items.add(value);
		
		x11.setItems(items);
		writeX11();
		dataGroupDataPoint.setX11(x11);
		x11 = new X11();
		x11ParseStatus.setParsed(true);
	}
}
