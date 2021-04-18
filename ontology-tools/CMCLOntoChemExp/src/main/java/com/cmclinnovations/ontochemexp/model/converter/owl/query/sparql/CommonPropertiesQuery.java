package com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.Collections;

import org.slf4j.Logger;

import com.cmclinnovations.ontochemexp.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonProperties;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonPropertiesProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Amount;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Component;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.SpeciesLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Uncertainty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Value;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

public class CommonPropertiesQuery extends OwlConverter implements ICommonPropertiesQuery {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ExperimentQuery.class);
	
	public void query() throws OntoChemExpException{
		queryInstance(ontoChemExpKB.getOntoChemNamespace().concat(COLON), ontoChemExpKB.getOntoChemExpKbTBoxIri(),
				ontoChemExpVocabulary.getClassCommonProperties());
		Collections.sort(queryResult);
		ArrayList<String> commonPropertiesInstance = queryResult;
		queryResult = new ArrayList<>();
		if (commonPropertiesInstance.size() > 0 && commonPropertiesInstance.size() < 2) {
			queryAllElement(commonPropertiesInstance.get(0));
			experiment.setCommonProperties(commonProperties);
			commonProperties = new CommonProperties();
		} else if (commonPropertiesInstance.size() > 1) {
			logger.error("There should not be more than one instance of commonProperties in an experiment.");
		} else {
			logger.error("No instance of commonProperties found in an experiment.");
		}
	}
	
	private void queryAllElement(String commonPropertiesInstance) throws OntoChemExpException {
		queryCommonPropertiesProperty(commonPropertiesInstance);
		commonProperties.setProperty(commonPropertiesPropertyList);
		commonPropertiesPropertyList = new ArrayList<CommonPropertiesProperty>();
	}
	
	private void queryCommonPropertiesProperty(String commonPropertiesInstance) throws OntoChemExpException {	
		ArrayList<String> propertyInstances = readObjPropertyProperty(commonPropertiesInstance);
		
		for (String propertyInstance : propertyInstances) {
			queryPropertyAllAttributes(propertyInstance);
			queryPropertyValue(propertyInstance);
			queryPropertyUncertainty(propertyInstance);
			queryPropertyComponent(propertyInstance);
			commonPropertiesPropertyList.add(commonPropertiesProperty);
			commonPropertiesProperty = new CommonPropertiesProperty();
		}
	}
	
	private void queryPropertyAllAttributes(String propertyInstance) throws OntoChemExpException {
		String name = readDataPropertyName(propertyInstance);
		if (name != null && !name.isEmpty()) {
			commonPropertiesProperty.setPropertyName(name);
		}
		
		String id = readDataPropertyID(propertyInstance);
		if (id != null && !id.isEmpty()) {
			commonPropertiesProperty.setPropertyId(id);
		}
		
		String label = readDataPropertyLabel(propertyInstance);
		if (label != null && !label.isEmpty()) {
			commonPropertiesProperty.setPropertyLabel(label);
		}

		String units = readDataPropertyUnits(propertyInstance);
		if (units != null && !units.isEmpty()) {
			commonPropertiesProperty.setPropertyUnits(units);
		}

		String description = readDataPropertyDescription(propertyInstance);
		if (description != null && !description.isEmpty()) {
			commonPropertiesProperty.setPropertyDescription(description);
		}
	}
	
	private void queryPropertyValue(String propertyInstance) throws OntoChemExpException {		
		String propertyValue = readDataPropertyValue(propertyInstance);
		if (propertyValue != null && !propertyValue.isEmpty()) {
			commonPropertiesPropertyValue.setValueValue(propertyValue);
			commonPropertiesProperty.setPropertyValue(commonPropertiesPropertyValue);
			commonPropertiesPropertyValue = new Value();
		}
	}
	
	private void queryPropertyUncertainty(String propertyInstance) throws OntoChemExpException {		
		ArrayList<String> uncertaintyInstance = readObjPropertyUncertainty(propertyInstance);
		
		if (uncertaintyInstance.size() > 0 && uncertaintyInstance.size() < 2) {
			queryPropertyUncertaintyAllAttributes(uncertaintyInstance.get(0));
			quaryPropertyUncertaintyValue(uncertaintyInstance.get(0));
			commonPropertiesProperty.setPropertyUncertainty(commonPropertiesPropertyUncertainty);
			commonPropertiesPropertyUncertainty = new Uncertainty();
		} else if (uncertaintyInstance.size() > 1) {
			logger.error("There should not be more than one instance of property uncertainty of commonPropertiesProperty.");
		}
	}
	
	private void queryPropertyUncertaintyAllAttributes(String uncertaintyInstance) {
		String bound = readDataPropertyBound(uncertaintyInstance);
		if (bound != null && !bound.isEmpty()) {
			commonPropertiesPropertyUncertainty.setUncertaintyBound(bound);
		}
		String kind = readDataPropertyKind(uncertaintyInstance);
		if (kind != null && !kind.isEmpty()) {
			commonPropertiesPropertyUncertainty.setUncertaintyKind(kind);
		}
		String transformation = readDataPropertyTransformation(uncertaintyInstance);
		if (transformation != null && !transformation.isEmpty()) {
			commonPropertiesPropertyUncertainty.setUncertaintyTransformation(transformation);
		}
		String type = readDataPropertyType(uncertaintyInstance);
		if (type != null && !type.isEmpty()) {
			commonPropertiesPropertyUncertainty.setUncertaintyType(type);
		}
	}

	private void quaryPropertyUncertaintyValue(String uncertaintyInstance) {
		String value = readDataPropertyValue(uncertaintyInstance);
		if (value != null && !value.isEmpty()) {
			commonPropertiesPropertyUncertainty.setUncertaintyValue(value);
		}
	}
	
	private void queryPropertyComponent(String propertyInstance) throws OntoChemExpException {		
		ArrayList<String> componentInstances = readObjPropertyComponent(propertyInstance);
		
		for (String componentInstance : componentInstances) {
			queryPropertyComponentAmount(componentInstance);
			queryPropertyComponentSpeciesLink(componentInstance);
			queryPropertyComponentUncertainty(componentInstance);
			commonPropertiesPropertyComponentList.add(commonPropertiesPropertyComponent);
			commonPropertiesPropertyComponent = new Component();
		}
		commonPropertiesProperty.setComponent(commonPropertiesPropertyComponentList);
		commonPropertiesPropertyComponentList = new ArrayList<Component>();
	}
	
	private void queryPropertyComponentAmount(String componentInstance) throws OntoChemExpException {
		ArrayList<String> amountInstance = readObjPropertyAmount(componentInstance);
		
		if (amountInstance.size() > 0 && amountInstance.size() < 2) {
			String units = readDataPropertyUnits(amountInstance.get(0));
			if (units != null && !units.isEmpty()) {
				commonPropertiesPropertyComponentAmount.setAmountUnits(units);
			}
			
			String value = readDataPropertyValue(amountInstance.get(0));
			if (value != null && !value.isEmpty()) {
				commonPropertiesPropertyComponentAmount.setAmountValue(value);
			}
			
			commonPropertiesPropertyComponent.setComponentAmount(commonPropertiesPropertyComponentAmount);
			commonPropertiesPropertyComponentAmount = new Amount();
		} else if (amountInstance.size() > 1) {
			logger.error("There should not be more than one instance of component amount of commonPropertiesProperty.");
		}
		
	}
	
	private void queryPropertyComponentSpeciesLink(String componentInstance) throws OntoChemExpException {
		ArrayList<String> speciesLinkInstance = readObjPropertySpeciesLink(componentInstance);
		
		if (speciesLinkInstance.size() > 0 && speciesLinkInstance.size() < 2) {
			String preferredKey = readDataPropertyPreferredKey(speciesLinkInstance.get(0));
			if (preferredKey != null && !preferredKey.isEmpty()) {
				commonPropertiesPropertyComponentSpeciesLink.setSpeciesLinkPreferredKey(preferredKey);
			}
			
			String primeID = readDataPropertyPrimeID(speciesLinkInstance.get(0));
			if (primeID != null && !primeID.isEmpty()) {
				commonPropertiesPropertyComponentSpeciesLink.setSpeciesLinkPrimeID(primeID);
			}
			
			String value = readDataPropertyValue(speciesLinkInstance.get(0));
			if (value != null && !value.isEmpty()) {
				commonPropertiesPropertyComponentSpeciesLink.setSpeciesLinkValue(value);
			}
			
			commonPropertiesPropertyComponent.setComponentSpeciesLink(commonPropertiesPropertyComponentSpeciesLink);
			commonPropertiesPropertyComponentSpeciesLink = new SpeciesLink();
		} else if (speciesLinkInstance.size() > 1) {
			logger.error("There should not be more than one instance of amount speciesLink of commonPropertiesProperty.");
		}
		
	}
	
	private void queryPropertyComponentUncertainty(String componentInstance) throws OntoChemExpException {
		ArrayList<String> uncertaintyInstance = readObjPropertyUncertainty(componentInstance);
		
		if (uncertaintyInstance.size() > 0 && uncertaintyInstance.size() < 2) {
			queryPropertyComponentUncertaintyAllAttributes(uncertaintyInstance.get(0));
			queryPropertyComponentUncertaintyValue(uncertaintyInstance.get(0));
			commonPropertiesPropertyComponent.setComponentUncertainty(commonPropertiesPropertyComponentUncertainty);
			commonPropertiesPropertyComponentUncertainty = new Uncertainty();
		} else if (uncertaintyInstance.size() > 1) {
			logger.error("There should not be more than one instance of component uncertainty of commonPropertiesPropertyComponent.");
		}
	}
	
	private void queryPropertyComponentUncertaintyAllAttributes(String uncertaintyInstance) {
		String bound = readDataPropertyBound(uncertaintyInstance);
		if (bound != null && !bound.isEmpty()) {
			commonPropertiesPropertyComponentUncertainty.setUncertaintyBound(bound);
		}
		String kind = readDataPropertyKind(uncertaintyInstance);
		if (kind != null && !kind.isEmpty()) {
			commonPropertiesPropertyComponentUncertainty.setUncertaintyKind(kind);
		}
		String transformation = readDataPropertyTransformation(uncertaintyInstance);
		if (transformation != null && !transformation.isEmpty()) {
			commonPropertiesPropertyComponentUncertainty.setUncertaintyTransformation(transformation);
		}
		String type = readDataPropertyType(uncertaintyInstance);
		if (type != null && !type.isEmpty()) {
			commonPropertiesPropertyComponentUncertainty.setUncertaintyType(type);
		}
	}

	private void queryPropertyComponentUncertaintyValue(String uncertaintyInstance) {
		String value = readDataPropertyValue(uncertaintyInstance);
		if (value != null && !value.isEmpty()) {
			commonPropertiesPropertyComponentUncertainty.setUncertaintyValue(value);
		}
	}	
}
