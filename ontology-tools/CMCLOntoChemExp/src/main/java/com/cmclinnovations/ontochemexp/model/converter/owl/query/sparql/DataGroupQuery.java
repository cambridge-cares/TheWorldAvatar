package com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;

import com.cmclinnovations.ontochemexp.model.converter.owl.OwlConverter;
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
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.DataAttributeLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.DerivedProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Feature;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Indicator;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Observable;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.PropertyLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.SpeciesLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Uncertainty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Value;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

public class DataGroupQuery extends OwlConverter implements IDataGroupQuery {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ExperimentQuery.class);

	public void query() throws OntoChemExpException {
		queryInstance(ontoChemExpKB.getOntoChemNamespace().concat(COLON), ontoChemExpKB.getOntoChemExpKbTBoxIri(),
				ontoChemExpVocabulary.getClassDataGroup());
		Collections.sort(queryResult);
		ArrayList<String> dataGroupInstances = queryResult;
		queryResult = new ArrayList<>();

		for (String dataGroupInstance : dataGroupInstances) {
			queryObjProperty(dataGroupInstance);
			queryDataProperty(dataGroupInstance);
			dataGroupList.add(dataGroup);
			dataGroup = new DataGroup();
		}

		experiment.setDataGroup(dataGroupList);
		dataGroupList = new ArrayList<DataGroup>();
	}

	private void queryObjProperty(String dataGroupInstance) throws OntoChemExpException {
		queryDataGroupLink(dataGroupInstance);
		queryDataGroupProperty(dataGroupInstance);
		queryDataGroupDataPoint(dataGroupInstance);
	}

	private void queryDataProperty(String dataGroupInstance) throws OntoChemExpException {
		String label = readDataPropertyLabel(dataGroupInstance);
		if (label != null && !label.isEmpty()) {
			dataGroup.setLabel(label);
		}

		String id = readDataPropertyID(dataGroupInstance);
		if (id != null && !id.isEmpty()) {
			dataGroup.setId(id);
		}

		String dataPointForm = readDataPropertyDataPointForm(dataGroupInstance);
		if (dataPointForm != null && !dataPointForm.isEmpty()) {
			dataGroup.setDataPointForm(dataPointForm);
		}
	}

	private void queryDataGroupLink(String dataGroupInstance) throws OntoChemExpException {
		ArrayList<String> dataGroupLinkInstance = readObjPropertyDataGroupLink(dataGroupInstance);

		if (dataGroupLinkInstance.size() > 0 && dataGroupLinkInstance.size() < 2) {
			String dataGroupID = readDataPropertyDataGroupID(dataGroupLinkInstance.get(0));
			if (dataGroupID != null && !dataGroupID.isEmpty()) {
				dataGroupDataGroupLink.setDataGroupID(dataGroupID);
			}

			String dataPointID = readDataPropertyDataPointID(dataGroupLinkInstance.get(0));
			if (dataPointID != null && !dataPointID.isEmpty()) {
				dataGroupDataGroupLink.setDataPointID(dataPointID);
			}

			String value = readDataPropertyValue(dataGroupLinkInstance.get(0));
			if (value != null && !value.isEmpty()) {
				dataGroupDataGroupLink.setValue(value);
			}

			dataGroup.setDataGroupLink(dataGroupDataGroupLink);
			dataGroupDataGroupLink = new DataGroupDataGroupLink();
		}
	}

	private void queryDataGroupProperty(String dataGroupInstance) throws OntoChemExpException {
		ArrayList<String> propertyInstances = readObjPropertyProperty(dataGroupInstance);

		for (String propertyInstance : propertyInstances) {
			queryPropertyDataProperty(propertyInstance);
			queryPropertyObjProperty(propertyInstance);
			dataGroupPropertyList.add(dataGroupProperty);
			dataGroupProperty = new DataGroupProperty();
		}

		dataGroup.setProperty(dataGroupPropertyList);
		dataGroupPropertyList = new ArrayList<DataGroupProperty>();
	}

	private void queryPropertyDataProperty(String propertyInstance) throws OntoChemExpException {
		String derivedPropertyExists = readDataPropertyDerivedPropertyExists(propertyInstance);
		if (derivedPropertyExists != null && !derivedPropertyExists.isEmpty()) {
			dataGroupProperty.setPropertyDerivedPropertyExists(derivedPropertyExists);
		}
		
		String name = readDataPropertyName(propertyInstance);
		if (name != null && !name.isEmpty()) {
			dataGroupProperty.setPropertyName(name);
		}

		String id = readDataPropertyID(propertyInstance);
		if (id != null && !id.isEmpty()) {
			dataGroupProperty.setPropertyId(id);
		}

		String label = readDataPropertyLabel(propertyInstance);
		if (label != null && !label.isEmpty()) {
			dataGroupProperty.setPropertyLabel(label);
		}

		String units = readDataPropertyUnits(propertyInstance);
		if (units != null && !units.isEmpty()) {
			dataGroupProperty.setPropertyUnits(units);
		}

		String description = readDataPropertyDescription(propertyInstance);
		if (description != null && !description.isEmpty()) {
			dataGroupProperty.setPropertyDescription(description);
		}
	}

	private void queryPropertyObjProperty(String propertyInstance) throws OntoChemExpException {
		queryPropertyValue(propertyInstance);
		queryPropertyUncertainty(propertyInstance);
		queryPropertyComponent(propertyInstance);
		queryPropertySpeciesLink(propertyInstance);
		queryPropertyDerivedProperty(propertyInstance);
	}

	private void queryPropertyValue(String propertyInstance) throws OntoChemExpException {
		String propertyValue = readDataPropertyValue(propertyInstance);
		if (propertyValue != null && !propertyValue.isEmpty()) {
			dataGroupPropertyValue.setValueValue(propertyValue);
			dataGroupProperty.setPropertyValue(dataGroupPropertyValue);
			dataGroupPropertyValue = new Value();
		}
	}

	private void queryPropertyUncertainty(String propertyInstance) throws OntoChemExpException {
		ArrayList<String> uncertaintyInstance = readObjPropertyUncertainty(propertyInstance);
		if (uncertaintyInstance.size() > 0 && uncertaintyInstance.size() < 2) {
			queryPropertyUncertaintyAllAttributes(uncertaintyInstance.get(0));
			quaryPropertyUncertaintyValue(uncertaintyInstance.get(0));

			dataGroupProperty.setPropertyUncertainty(dataGroupPropertyUncertainty);
			dataGroupPropertyUncertainty = new Uncertainty();
		} else if (uncertaintyInstance.size() > 1) {
			logger.error(
					"There should not be more than one instance of property uncertainty of commonPropertiesProperty.");
		}
	}

	private void queryPropertyUncertaintyAllAttributes(String uncertaintyInstance) {
		String bound = readDataPropertyBound(uncertaintyInstance);
		if (bound != null && !bound.isEmpty()) {
			dataGroupPropertyUncertainty.setUncertaintyBound(bound);
		}
		String kind = readDataPropertyKind(uncertaintyInstance);
		if (kind != null && !kind.isEmpty()) {
			dataGroupPropertyUncertainty.setUncertaintyKind(kind);
		}
		String transformation = readDataPropertyTransformation(uncertaintyInstance);
		if (transformation != null && !transformation.isEmpty()) {
			dataGroupPropertyUncertainty.setUncertaintyTransformation(transformation);
		}
		String type = readDataPropertyType(uncertaintyInstance);
		if (type != null && !type.isEmpty()) {
			dataGroupPropertyUncertainty.setUncertaintyType(type);
		}
	}

	private void quaryPropertyUncertaintyValue(String uncertaintyInstance) {
		String value = readDataPropertyValue(uncertaintyInstance);
		if (value != null && !value.isEmpty()) {
			dataGroupPropertyUncertainty.setUncertaintyValue(value);
		}
	}

	private void queryPropertyComponent(String propertyInstance) throws OntoChemExpException {
		ArrayList<String> componentInstance = readObjPropertyComponent(propertyInstance);

		if (componentInstance.size() > 0 && componentInstance.size() < 2) {
			List<Object> items = new ArrayList<Object>();
			String componentValue = readDataPropertyValue(componentInstance.get(0));
			if (componentValue != null && !componentValue.isEmpty()) {
				items.add(componentValue);
			}
			
			ArrayList<String> componentSpeciesLinks = readObjPropertySpeciesLink(componentInstance.get(0));
			for (String componentSpeciesLink : componentSpeciesLinks) {
				String preferredKey = readDataPropertyPreferredKey(componentSpeciesLink);
				if (preferredKey != null && !preferredKey.isEmpty()) {
					dataGroupPropertyComponentSpeciesLink.setSpeciesLinkPreferredKey(preferredKey);
				}
				
				String primeID = readDataPropertyPrimeID(componentSpeciesLink);
				if (primeID != null && !primeID.isEmpty()) {
					dataGroupPropertyComponentSpeciesLink.setSpeciesLinkPrimeID(primeID);
				}
				
				String cas = readDataPropertyCAS(componentSpeciesLink);
				if (cas != null && !cas.isEmpty()) {
					dataGroupPropertyComponentSpeciesLink.setCas(cas);
				}
				
				String inchi = readDataPropertyInChI(componentSpeciesLink);
				if (inchi != null && !inchi.isEmpty()) {
					dataGroupPropertyComponentSpeciesLink.setInchi(inchi);
				}
				
				String smiles = readDataPropertySMILES(componentSpeciesLink);
				if (smiles != null && !smiles.isEmpty()) {
					dataGroupPropertyComponentSpeciesLink.setSmiles(smiles);
				}
				
				String sLValue = readDataPropertyValue(componentSpeciesLink);
				if (sLValue != null && !sLValue.isEmpty()) {
					dataGroupPropertyComponentSpeciesLink.setSpeciesLinkValue(sLValue);
				}
				
				items.add(dataGroupPropertyComponentSpeciesLink);
				dataGroupPropertyComponentSpeciesLink = new SpeciesLink();
			}
			
			if (items != null) {
				dataGroupPropertyComponent.setItems(items);
				dataGroupProperty.setPropertyComponent(dataGroupPropertyComponent);
				dataGroupPropertyComponent = new DataGroupPropertyComponent();
			}
		} else if (componentInstance.size() > 1) {
			logger.error("There should not be more than one instance of property component of dataGroupProperty.");
		}
	}

	private void queryPropertySpeciesLink(String propertyInstance) throws OntoChemExpException {
		ArrayList<String> speciesLinkInstance = readObjPropertySpeciesLink(propertyInstance);

		if (speciesLinkInstance.size() > 0 && speciesLinkInstance.size() < 2) {
			String preferredKey = readDataPropertyPreferredKey(speciesLinkInstance.get(0));
			if (preferredKey != null && !preferredKey.isEmpty()) {
				dataGroupPropertySpeciesLink.setSpeciesLinkPreferredKey(preferredKey);
			}

			String primeID = readDataPropertyPrimeID(speciesLinkInstance.get(0));
			if (primeID != null && !primeID.isEmpty()) {
				dataGroupPropertySpeciesLink.setSpeciesLinkPrimeID(primeID);
			}
			
			String cas = readDataPropertyCAS(speciesLinkInstance.get(0));
			if (cas != null && !cas.isEmpty()) {
				dataGroupPropertySpeciesLink.setCas(cas);
			}
			
			String inchi = readDataPropertyInChI(speciesLinkInstance.get(0));
			if (inchi != null && !inchi.isEmpty()) {
				dataGroupPropertySpeciesLink.setInchi(inchi);
			}
			
			String smiles = readDataPropertySMILES(speciesLinkInstance.get(0));
			if (smiles != null && !smiles.isEmpty()) {
				dataGroupPropertySpeciesLink.setSmiles(smiles);
			}

			String value = readDataPropertyValue(speciesLinkInstance.get(0));
			if (value != null && !value.isEmpty()) {
				dataGroupPropertySpeciesLink.setSpeciesLinkValue(value);
			}

			dataGroupProperty.setPropertySpeciesLink(dataGroupPropertySpeciesLink);
			dataGroupPropertySpeciesLink = new SpeciesLink();
		} else if (speciesLinkInstance.size() > 1) {
			logger.error("There should not be more than one instance of speciesLink of dataGroupProperty.");
		}
	}

	private void queryPropertyDerivedProperty(String propertyInstance) throws OntoChemExpException {
		ArrayList<String> derivedPropertyInstance = readObjPropertyDerivedProperty(propertyInstance);

		if (derivedPropertyInstance.size() > 0 && derivedPropertyInstance.size() < 2) {
			queryPropertyDerivedPropertyFeature(derivedPropertyInstance.get(0));

			String description = readDataPropertyDescription(derivedPropertyInstance.get(0));
			if (description != null && !description.isEmpty()) {
				dataGroupPropertyDerivedProperty.setDerivedPropertyDescription(description);
			}

			String id = readDataPropertyID(derivedPropertyInstance.get(0));
			if (id != null && !id.isEmpty()) {
				dataGroupPropertyDerivedProperty.setDerivedPropertyId(id);
			}

			String label = readDataPropertyLabel(derivedPropertyInstance.get(0));
			if (label != null && !label.isEmpty()) {
				dataGroupPropertyDerivedProperty.setDerivedPropertyLabel(label);
			}

			String name = readDataPropertyName(derivedPropertyInstance.get(0));
			if (name != null && !name.isEmpty()) {
				dataGroupPropertyDerivedProperty.setDerivedPropertyName(name);
			}

			String units = readDataPropertyUnits(derivedPropertyInstance.get(0));
			if (units != null && !units.isEmpty()) {
				dataGroupPropertyDerivedProperty.setDerivedPropertyUnits(units);
			}

			dataGroupProperty.setPropertyDerivedProperty(dataGroupPropertyDerivedProperty);
			dataGroupPropertyDerivedProperty = new DerivedProperty();
		} else if (derivedPropertyInstance.size() > 1) {
			logger.error("There should not be more than one instance of derivedProperty of dataGroupProperty.");
		}
	}

	private void queryPropertyDerivedPropertyFeature(String derivedPropertyInstance) throws OntoChemExpException {
		ArrayList<String> featureInstance = readObjPropertyFeature(derivedPropertyInstance);

		if (featureInstance.size() > 0 && featureInstance.size() < 2) {
			queryFeatureIndicator(featureInstance.get(0));
			queryFeatureObservable(featureInstance.get(0));

			String id = readDataPropertyID(featureInstance.get(0));
			if (id != null && !id.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeature.setFeatureId(id);
			}

			String primeID = readDataPropertyPrimeID(featureInstance.get(0));
			if (primeID != null && !primeID.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeature.setFeaturePrimeID(primeID);
			}

			String type = readDataPropertyType(featureInstance.get(0));
			if (type != null && !type.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeature.setFeatureType(type);
			}

			dataGroupPropertyDerivedProperty.setDerivedPropertyFeature(dataGroupPropertyDerivedPropertyFeature);
			dataGroupPropertyDerivedPropertyFeature = new Feature();
		} else if (featureInstance.size() > 1) {
			logger.error("There should not be more than one instance of feature of derivedProperty.");
		}
	}

	private void queryFeatureIndicator(String featureInstance) throws OntoChemExpException {
		ArrayList<String> indicatorInstances = readObjPropertyIndicator(featureInstance);

		for (String indicatorInstance : indicatorInstances) {
			queryPropertyLink(indicatorInstance);
			queryDataAttributeLink(indicatorInstance);

			String id = readDataPropertyID(indicatorInstance);
			if (id != null && !id.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureIndicator.setIndicatorId(id);
			}

			String variableID = readDataPropertyVariableID(indicatorInstance);
			if (variableID != null && !variableID.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureIndicator.setIndicatorVariableID(variableID);
			}

			String transformation = readDataPropertyTransformation(indicatorInstance);
			if (transformation != null && !transformation.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureIndicator.setIndicatorTransformation(transformation);
			}

			dataGroupPropertyDerivedPropertyFeatureIndicatorList.add(dataGroupPropertyDerivedPropertyFeatureIndicator);
			dataGroupPropertyDerivedPropertyFeatureIndicator = new Indicator();
		}

		dataGroupPropertyDerivedPropertyFeature
				.setFeatureIndicator(dataGroupPropertyDerivedPropertyFeatureIndicatorList);
		dataGroupPropertyDerivedPropertyFeatureIndicatorList = new ArrayList<Indicator>();
	}

	private void queryFeatureObservable(String featureInstance) throws OntoChemExpException {
		ArrayList<String> observableInstance = readObjPropertyObservable(featureInstance);

		if (observableInstance.size() > 0 && observableInstance.size() < 2) {
			String id = readDataPropertyID(observableInstance.get(0));
			if (id != null && !id.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureObservable.setObservableId(id);
			}

			String variableID = readDataPropertyVariableID(observableInstance.get(0));
			if (variableID != null && !variableID.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureObservable.setObservableVariableID(variableID);
			}

			String value = readDataPropertyValue(observableInstance.get(0));
			if (value != null && !value.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureObservable.setObservableValue(value);
			}

			dataGroupPropertyDerivedPropertyFeature
					.setFeatureObservable(dataGroupPropertyDerivedPropertyFeatureObservable);
			dataGroupPropertyDerivedPropertyFeatureObservable = new Observable();
		} else if (observableInstance.size() > 1) {
			logger.error("There should not be more than one instance of observable of derivedProperty feature.");
		}
	}

	private void queryPropertyLink(String indicatorInstance) throws OntoChemExpException {
		ArrayList<String> propertyLinkInstance = readObjPropertyPropertyLink(indicatorInstance);

		if (propertyLinkInstance.size() > 0 && propertyLinkInstance.size() < 2) {
			String propertyID = readDataPropertyPropertyID(propertyLinkInstance.get(0));
			if (propertyID != null && !propertyID.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.setPropertyId(propertyID);
			}

			String dataGroupID = readDataPropertyDataGroupID(propertyLinkInstance.get(0));
			if (dataGroupID != null && !dataGroupID.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.setDataGroupID(dataGroupID);
			}

			String value = readDataPropertyValue(propertyLinkInstance.get(0));
			if (value != null && !value.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.setPropertyLinkValue(value);
			}

			dataGroupPropertyDerivedPropertyFeatureIndicator
					.setIndicatorPropertyLink(dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink);
			dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink = new PropertyLink();
		} else if (propertyLinkInstance.size() > 1) {
			logger.error("There should not be more than one instance of propertyLink of feature indicator.");
		}
	}

	private void queryDataAttributeLink(String indicatorInstance) throws OntoChemExpException {
		ArrayList<String> dataAttributeLinkInstance = readObjPropertyDataAttributeLink(indicatorInstance);

		if (dataAttributeLinkInstance.size() > 0 && dataAttributeLinkInstance.size() < 2) {
			String id = readDataPropertyID(dataAttributeLinkInstance.get(0));
			if (id != null && !id.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.setDataAttributeLinkId(id);
			}

			String primeID = readDataPropertyPrimeID(dataAttributeLinkInstance.get(0));
			if (primeID != null && !primeID.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.setDataAttributeLinkPrimeID(primeID);
			}

			String value = readDataPropertyValue(dataAttributeLinkInstance.get(0));
			if (value != null && !value.isEmpty()) {
				dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.setDataAttributeLinkValue(value);
			}	

			dataGroupPropertyDerivedPropertyFeatureIndicator
					.setIndicatorDataAttriuteLink(dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink);
			dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink = new DataAttributeLink();
		} else if (dataAttributeLinkInstance.size() > 1) {
			logger.error(
					"There should not be more than one instance of dataAttributeLinkInstance of feature indicator.");
		}
	}

	private void queryDataGroupDataPoint(String dataGroupInstance) throws OntoChemExpException {
		ArrayList<String> dataPointInstances = readObjPropertyDataPoint(dataGroupInstance);
		
		for (String dataPointInstance : dataPointInstances) {
			queryDataPointX(dataPointInstance);

			String id = readDataPropertyID(dataPointInstance);
			if (id != null && !id.isEmpty()) {
				dataGroupDataPoint.setId(id);
			}

			dataGroupDataPointList.add(dataGroupDataPoint);
			dataGroupDataPoint = new DataGroupDataPoint();
		}

		dataGroup.setDataPoint(dataGroupDataPointList);
		dataGroupDataPointList = new ArrayList<DataGroupDataPoint>();
	}
	
	private void queryXUncertaintyAttribAndValue(String xU) {
		String bound = readDataPropertyBound(xU);
		if (bound != null && !bound.isEmpty()) {
			xUncertainty.setUncertaintyBound(bound);
		}
		String kind = readDataPropertyKind(xU);
		if (kind != null && !kind.isEmpty()) {
			xUncertainty.setUncertaintyKind(kind);
		}
		String transformation = readDataPropertyTransformation(xU);
		if (transformation != null && !transformation.isEmpty()) {
			xUncertainty.setUncertaintyTransformation(transformation);
		}
		String type = readDataPropertyType(xU);
		if (type != null && !type.isEmpty()) {
			xUncertainty.setUncertaintyType(type);
		}
		
		String val = readDataPropertyValue(xU);
		if (val != null && !val.isEmpty()) {
			xUncertainty.setUncertaintyValue(val);
		}
	}

	private void queryDataPointX(String dataPointInstance) throws OntoChemExpException {
		ArrayList<String> xInstances = readObjPropertyDataPointX(dataPointInstance);
		
		if (xInstances.size() > 0) {
			for (String x : xInstances) {
				List<Object> items = new ArrayList<Object>();
				
				String value = readDataPropertyValue(x);
				items.add(value);
				
				ArrayList<String> xUs = readObjPropertyUncertainty(x);
				for (String xU : xUs) {
					queryXUncertaintyAttribAndValue(xU);
					items.add(xUncertainty);
					xUncertainty = new Uncertainty();
				}

				if (value != null && !value.isEmpty()) {
					switch (x.substring(1, 4)) {
					case "001":
						x1.setItems(items);
						dataGroupDataPoint.setX1(x1);
						x1 = new X1();
						break;
					case "002":
						x2.setItems(items);
						dataGroupDataPoint.setX2(x2);
						x2 = new X2();
						break;
					case "003":
						x3.setItems(items);
						dataGroupDataPoint.setX3(x3);
						x3 = new X3();
						break;
					case "004":
						x4.setItems(items);
						dataGroupDataPoint.setX4(x4);
						x4 = new X4();
						break;
					case "005":
						x5.setItems(items);
						dataGroupDataPoint.setX5(x5);
						x5 = new X5();
						break;
					case "006":
						x6.setItems(items);
						dataGroupDataPoint.setX6(x6);
						x6 = new X6();
						break;
					case "007":
						x7.setItems(items);
						dataGroupDataPoint.setX7(x7);
						x7 = new X7();
						break;
					case "008":
						x8.setItems(items);
						dataGroupDataPoint.setX8(x8);
						x8 = new X8();
						break;
					case "009":
						x9.setItems(items);
						dataGroupDataPoint.setX9(x9);
						x9 = new X9();
						break;
					case "010":
						x10.setItems(items);
						dataGroupDataPoint.setX10(x10);
						x10 = new X10();
						break;
					case "011":
						x11.setItems(items);
						dataGroupDataPoint.setX11(x11);
						x11 = new X11();
						break;
					}
				}
			}
		}
	}
}
