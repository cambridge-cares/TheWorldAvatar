package com.cmclinnovations.ontochemexp.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all PrIMe tags and properties provided in the
 * prime.vocabulary.properties file.</br>
 * 
 * This will empower users to use PrIMe, if some of its tags or properties
 * change at a later stage, without changing the source code of
 * OntoChemExp.</br>
 * 
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
@Configuration
@PropertySource("classpath:prime.vocabulary.properties")
public class PrimeVocabulary {
	////////////////////////////////////////////////////////////////
	//////////////////////// Prime element /////////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${prime.element.experiment}")
	private String elemExperiment;

	public String getElemExperiment() {
		return elemExperiment;
	}

	public void setElemExperiment(String elemExperiment) {
		this.elemExperiment = elemExperiment;
	}

	@Value("${prime.element.copyright}")
	private String elemCopyright;

	public String getElemCopyright() {
		return elemCopyright;
	}

	public void setElemCopyright(String elemCopyright) {
		this.elemCopyright = elemCopyright;
	}

	@Value("${prime.element.bibliographyLink}")
	private String elemBibliographyLink;

	public String getElemBibliographyLink() {
		return elemBibliographyLink;
	}

	public void setElemBibliographyLink(String elemBibliographyLink) {
		this.elemBibliographyLink = elemBibliographyLink;
	}

	@Value("${prime.element.apparatus}")
	private String elemApparatus;

	public String getElemApparatus() {
		return elemApparatus;
	}

	public void setElemApparatus(String elemApparatus) {
		this.elemApparatus = elemApparatus;
	}

	@Value("${prime.element.commonProperties}")
	private String elemCommonProperties;

	public String getElemCommonProperties() {
		return elemCommonProperties;
	}

	public void setElemCommonProperties(String elemCommonProperties) {
		this.elemCommonProperties = elemCommonProperties;
	}

	@Value("${prime.element.dataGroup}")
	private String elemDataGroup;

	public String getElemDataGroup() {
		return elemDataGroup;
	}

	public void setElemDataGroup(String elemDataGroup) {
		this.elemDataGroup = elemDataGroup;
	}

	@Value("${prime.element.additionalDataItem}")
	private String elemAdditionalDataItem;

	public String getElemAdditionalDataItem() {
		return elemAdditionalDataItem;
	}

	public void setElemAdditionalDataItem(String elemAdditionalDataItem) {
		this.elemAdditionalDataItem = elemAdditionalDataItem;
	}

	@Value("${prime.element.preferredKey}")
	private String elemPreferredKey;

	public String getElemPreferredKey() {
		return elemPreferredKey;
	}

	public void setElemPreferredKey(String elemPreferredKey) {
		this.elemPreferredKey = elemPreferredKey;
	}

	@Value("${prime.element.kind}")
	private String elemKind;

	public String getElemKind() {
		return elemKind;
	}

	public void setElemKind(String elemKind) {
		this.elemKind = elemKind;
	}

	@Value("${prime.element.mode}")
	private String elemMode;

	public String getElemMode() {
		return elemMode;
	}

	public void setElemMode(String elemMode) {
		this.elemMode = elemMode;
	}

	@Value("${prime.element.property}")
	private String elemProperty;

	public String getElemProperty() {
		return elemProperty;
	}

	public void setElemProperty(String elemProperty) {
		this.elemProperty = elemProperty;
	}
	
	@Value("${prime.element.dataGroupLink}")
	private String elemDataGroupLink;

	public String getElemDataGroupLink() {
		return elemDataGroupLink;
	}

	public void setElemDataGroupLink(String elemDataGroupLink) {
		this.elemDataGroupLink = elemDataGroupLink;
	}

	@Value("${prime.element.dataPoint}")
	private String elemDataPoint;

	public String getElemDataPoint() {
		return elemDataPoint;
	}

	public void setElemDataPoint(String elemDataPoint) {
		this.elemDataPoint = elemDataPoint;
	}

	@Value("${prime.element.component}")
	private String elemComponent;

	public String getElemComponent() {
		return elemComponent;
	}

	public void setElemComponent(String elemComponent) {
		this.elemComponent = elemComponent;
	}

	@Value("${prime.element.value}")
	private String elemValue;

	public String getElemValue() {
		return elemValue;
	}

	public void setElemValue(String elemValue) {
		this.elemValue = elemValue;
	}

	@Value("${prime.element.uncertainty}")
	private String elemUncertainty;

	public String getElemUncertainty() {
		return elemUncertainty;
	}

	public void setElemUncertainty(String elemUncertainty) {
		this.elemUncertainty = elemUncertainty;
	}

	@Value("${prime.element.speciesLink}")
	private String elemSpeciesLink;

	public String getElemSpeciesLink() {
		return elemSpeciesLink;
	}

	public void setElemSpeciesLink(String elemSpeciesLink) {
		this.elemSpeciesLink = elemSpeciesLink;
	}

	@Value("${prime.element.derivedProperty}")
	private String elemDerivedProperty;

	public String getElemDerivedProperty() {
		return elemDerivedProperty;
	}

	public void setElemDerivedProperty(String elemDerivedProperty) {
		this.elemDerivedProperty = elemDerivedProperty;
	}

	@Value("${prime.element.amount}")
	private String elemAmount;

	public String getElemAmount() {
		return elemAmount;
	}

	public void setElemAmount(String elemAmount) {
		this.elemAmount = elemAmount;
	}

	@Value("${prime.element.feature}")
	private String elemFeature;

	public String getElemFeature() {
		return elemFeature;
	}

	public void setElemFeature(String elemFeature) {
		this.elemFeature = elemFeature;
	}

	@Value("${prime.element.indicator}")
	private String elemIndicator;

	public String getElemIndicator() {
		return elemIndicator;
	}

	public void setElemIndicator(String elemIndicator) {
		this.elemIndicator = elemIndicator;
	}

	@Value("${prime.element.observable}")
	private String elemObservable;

	public String getElemObservable() {
		return elemObservable;
	}

	public void setElemObservable(String elemObservable) {
		this.elemObservable = elemObservable;
	}

	@Value("${prime.element.propertyLink}")
	private String elemPropertyLink;

	public String getElemPropertyLink() {
		return elemPropertyLink;
	}

	public void setElemPropertyLink(String elemPropertyLink) {
		this.elemPropertyLink = elemPropertyLink;
	}

	@Value("${prime.element.dataAttributeLink}")
	private String elemDataAttributeLink;

	public String getElemDataAttributeLink() {
		return elemDataAttributeLink;
	}

	public void setElemDataAttributeLink(String elemDataAttributeLink) {
		this.elemDataAttributeLink = elemDataAttributeLink;
	}
	
	@Value("${prime.element.dataPointX}")
	private String elemDataPointX;
	
	public String getElemDataPointX() {
		return elemDataPointX;
	}
	
	public void setElemDataPointX(String elemDataPointX) {
		this.elemDataPointX = elemDataPointX;
	}
	
	////////////////////////////////////////////////////////////////
	/////////////////////// Prime attribute ////////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${prime.attribute.hasMIME}")
	private String attribMIME;

	public String getAttribMIME() {
		return attribMIME;
	}

	public void setAttribMIME(String attribMIME) {
		this.attribMIME = attribMIME;
	}

	@Value("${prime.attribute.hasItemType}")
	private String attribItemType;

	public String getAttribItemType() {
		return attribItemType;
	}

	public void setAttribItemType(String attribItemType) {
		this.attribItemType = attribItemType;
	}

	@Value("${prime.attribute.hasDescription}")
	private String attribDescription;

	public String getAttribDescription() {
		return attribDescription;
	}

	public void setAttribDescription(String attribDescription) {
		this.attribDescription = attribDescription;
	}

	@Value("${prime.attribute.hasPreferredKey}")
	private String attribPreferredKey;

	public String getAttribPreferredKey() {
		return attribPreferredKey;
	}

	public void setAttribPreferredKey(String attribPreferredKey) {
		this.attribPreferredKey = attribPreferredKey;
	}

	@Value("${prime.attribute.hasPrimeID}")
	private String attribPrimeID;

	public String getAttribPrimeID() {
		return attribPrimeID;
	}

	public void setAttribPrimeID(String attribPrimeID) {
		this.attribPrimeID = attribPrimeID;
	}

	@Value("${prime.attribute.hasID}")
	private String attribID;

	public String getAttribID() {
		return attribID;
	}

	public void setAttribID(String attribID) {
		this.attribID = attribID;
	}

	@Value("${prime.attribute.hasLabel}")
	private String attribLabel;

	public String getAttribLabel() {
		return attribLabel;
	}

	public void setAttribLabel(String attribLabel) {
		this.attribLabel = attribLabel;
	}

	@Value("${prime.attribute.hasDataPointForm}")
	private String attribDataPointForm;

	public String getAttribDataPointForm() {
		return attribDataPointForm;
	}

	public void setAttribDataPointForm(String attribDataPointForm) {
		this.attribDataPointForm = attribDataPointForm;
	}

	@Value("${prime.attribute.hasType}")
	private String attribType;

	public String getAttribType() {
		return attribType;
	}

	public void setAttribType(String attribType) {
		this.attribType = attribType;
	}

	@Value("${prime.attribute.hasName}")
	private String attribName;

	public String getAttribName() {
		return attribName;
	}

	public void setAttribName(String attribName) {
		this.attribName = attribName;
	}

	@Value("${prime.attribute.hasUnits}")
	private String attribUnits;

	public String getAttribUnits() {
		return attribUnits;
	}

	public void setAttribUnits(String attribUnits) {
		this.attribUnits = attribUnits;
	}

	@Value("${prime.attribute.hasDerivedPropertyExists}")
	private String attribDerivedPropertyExists;

	public String getAttribDerivedPropertyExists() {
		return attribDerivedPropertyExists;
	}

	public void setAttribDerivedPropertyExists(String attribDerivedPropertyExists) {
		this.attribDerivedPropertyExists = attribDerivedPropertyExists;
	}

	@Value("${prime.attribute.hasBound}")
	private String attribBound;

	public String getAttribBound() {
		return attribBound;
	}

	public void setAttribBound(String attribBound) {
		this.attribBound = attribBound;
	}

	@Value("${prime.attribute.hasKind}")
	private String attribKind;

	public String getAttribKind() {
		return attribKind;
	}

	public void setAttribKind(String attribKind) {
		this.attribKind = attribKind;
	}

	@Value("${prime.attribute.hasTransformation}")
	private String attribTransformation;

	public String getAttribTransformation() {
		return attribTransformation;
	}

	public void setAttribTransformation(String attribTransformation) {
		this.attribTransformation = attribTransformation;
	}
	
	@Value("${prime.attribute.hasVariableID}")
	private String attribVariableID;
	
	public String getAttribVariableID() {
		return attribVariableID;
	}
	
	public void setAttribVariableID(String attribVariableID) {
		this.attribVariableID = attribVariableID;
	}
	
	@Value("${prime.attribute.hasDataGroupID}")
	private String attribDataGroupID;
	
	public String getAttribDataGroupID() {
		return attribDataGroupID;
	}
	
	public void setAttribDataGroupID(String attribDataGroupID) {
		this.attribDataGroupID = attribDataGroupID;
	}
	
	@Value("${prime.attribute.hasDataPointID}")
	private String attribDataPointID;
	
	public String getAttribDataPointID() {
		return attribDataPointID;
	}
	
	public void setAttribDataPointID(String attribDataPointID) {
		this.attribDataPointID = attribDataPointID;
	}
	
//	@Value("${prime.attribute.hasXmlns}")
//	private String attribXmlns;
//	
//	public String getAttribXmlns() {
//		return attribXmlns;
//	}
//	
//	public void setAttribXmlns(String attribXmlns) {
//		this.attribXmlns = attribXmlns;
//	}
//	
//	@Value("${prime.attribute.hasXmlnsXsi}")
//	private String attribXmlnsXsi;
//	
//	public String getAttribXmlnsXsi() {
//		return attribXmlnsXsi;
//	}
//	
//	public void setAttribXmlnsXsi(String attribXmlnsXsi) {
//		this.attribXmlnsXsi = attribXmlnsXsi;
//	}
//	
//	@Value("${prime.attribute.hasXsiSchemaLocation}")
//	private String attribXsiSchemaLocation;
//	
//	public String getAttribXsiSchemaLocation() {
//		return attribXsiSchemaLocation;
//	}
//	
//	public void setAttribXsiSchemaLocation(String attribXsiSchemaLocation) {
//		this.attribXsiSchemaLocation = attribXsiSchemaLocation;
//	}
	
	@Value("${prime.attribute.hasPropertyID}")
	private String attribPropertyID;
	
	public String getAttribPropertyID() {
		return attribPropertyID;
	}
	
	public void setAttribPropertyID(String attribPropertyID) {
		this.attribPropertyID = attribPropertyID;
	}
	
	@Value("${prime.attribute.hasSourceType}")
	private String attribSourceType;
	
	@Value("${prime.attribute.hasReference}")
	private String attribReference;
	
	@Value("${prime.attribute.hasMethod}")
	private String attribMethod;

	public String getAttribSourceType() {
		return attribSourceType;
	}

	public void setAttribSourceType(String attribSourceType) {
		this.attribSourceType = attribSourceType;
	}

	public String getAttribReference() {
		return attribReference;
	}

	public void setAttribReference(String attribReference) {
		this.attribReference = attribReference;
	}

	public String getAttribMethod() {
		return attribMethod;
	}

	public void setAttribMethod(String attribMethod) {
		this.attribMethod = attribMethod;
	}
	
	@Value("${prime.attribute.hasCAS}")
	private String attribCAS;
	
	@Value("${prime.attribute.hasInChI}")
	private String attribInChI;
	
	@Value("${prime.attribute.hasSMILES}")
	private String attribSMILES;

	public String getAttribCAS() {
		return attribCAS;
	}

	public void setAttribCAS(String attribCAS) {
		this.attribCAS = attribCAS;
	}

	public String getAttribInChI() {
		return attribInChI;
	}

	public void setAttribInChI(String attribInChI) {
		this.attribInChI = attribInChI;
	}

	public String getAttribSMILES() {
		return attribSMILES;
	}

	public void setAttribSMILES(String attribSMILES) {
		this.attribSMILES = attribSMILES;
	}
	
	@Value("${prime.attribute.hasChemName}")
	private String attribChemName;

	public String getAttribChemName() {
		return attribChemName;
	}

	public void setAttribChemName(String attribChemName) {
		this.attribChemName = attribChemName;
	}
}