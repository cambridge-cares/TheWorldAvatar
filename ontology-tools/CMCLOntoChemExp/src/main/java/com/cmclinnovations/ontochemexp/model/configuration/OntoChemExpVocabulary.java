package com.cmclinnovations.ontochemexp.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all OntoChemExp class and property names and name spaces
 * provided in the ontochemexp.vocabulary.properties file.</br>
 * 
 * This will empower users to use OntoChemExp, if some of its classes,
 * properties or name spaces change at a later stage, without changing its
 * source code.</br>
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
@Configuration
@PropertySource("classpath:ontochemexp.vocabulary.properties")
public class OntoChemExpVocabulary {
	////////////////////////////////////////////////////////////////
	/////////////////////// OntoChemExp class////////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${ontochemexp.class.experiment}")
	private String classExperiment;

	public String getClassExperiment() {
		return classExperiment;
	}

	public void setClassExperiment(String classExperiment) {
		this.classExperiment = classExperiment;
	}

	@Value("${ontochemexp.class.copyright}")
	private String classCopyright;

	public String getClassCopyright() {
		return classCopyright;
	}

	public void setClassCopyright(String classCopyright) {
		this.classCopyright = classCopyright;
	}

	@Value("${ontochemexp.class.bibliographyLink}")
	private String classBibliographyLink;

	public String getClassBibliographyLink() {
		return classBibliographyLink;
	}

	public void setClassBibliographyLink(String classBibliographyLink) {
		this.classBibliographyLink = classBibliographyLink;
	}

	@Value("${ontochemexp.class.apparatus}")
	private String classApparatus;

	public String getClassApparatus() {
		return classApparatus;
	}

	public void setClassApparatus(String classApparatus) {
		this.classApparatus = classApparatus;
	}

	@Value("${ontochemexp.class.commonProperties}")
	private String classCommonProperties;

	public String getClassCommonProperties() {
		return classCommonProperties;
	}

	public void setClassCommonProperties(String classCommonProperties) {
		this.classCommonProperties = classCommonProperties;
	}

	@Value("${ontochemexp.class.dataGroup}")
	private String classDataGroup;

	public String getClassDataGroup() {
		return classDataGroup;
	}

	public void setClassDataGroup(String classDataGroup) {
		this.classDataGroup = classDataGroup;
	}

	@Value("${ontochemexp.class.additionalDataItem}")
	private String classAdditionalDataItem;

	public String getClassAdditionalDataItem() {
		return classAdditionalDataItem;
	}

	public void setClassAdditionalDataItem(String classAdditionalDataItem) {
		this.classAdditionalDataItem = classAdditionalDataItem;
	}

	@Value("${ontochemexp.class.kind}")
	private String classKind;

	public String getClassKind() {
		return classKind;
	}

	public void setClassKind(String classKind) {
		this.classKind = classKind;
	}

	@Value("${ontochemexp.class.mode}")
	private String classMode;

	public String getClassMode() {
		return classMode;
	}

	public void setClassMode(String classMode) {
		this.classMode = classMode;
	}

	@Value("${ontochemexp.class.property}")
	private String classProperty;

	public String getClassProperty() {
		return classProperty;
	}

	public void setClassProperty(String classProperty) {
		this.classProperty = classProperty;
	}

	@Value("${ontochemexp.class.initialComposition}")
	private String classInitialComposition;
	
	public String getClassInitialComposition() {
		return classInitialComposition;
	}

	public void setClassInitialComposition(String classInitialComposition) {
		this.classInitialComposition = classInitialComposition;
	}
		
	@Value("${ontochemexp.class.donor}")
	private String classDonor;
	
	public String getClassDonor() {
		return classDonor;
	}

	public void setClassDonor(String classDonor) {
		this.classDonor = classDonor;
	}
	
	@Value("${ontochemexp.class.acceptor}")
	private String classAcceptor;
	
	public String getClassAcceptor() {
		return classAcceptor;
	}

	public void setClassAcceptor(String classAcceptor) {
		this.classAcceptor = classAcceptor;
	}

	@Value("${ontochemexp.class.composition}")
	private String classComposition;
	
	public String getClassComposition() {
		return classComposition;
	}

	public void setClassComposition(String classComposition) {
		this.classComposition = classComposition;
	}
	
	@Value("${ontochemexp.class.concentration}")
	private String classConcentration;
	
	public String getClassConcentration() {
		return classConcentration;
	}

	public void setClassConcentration(String classConcentration) {
		this.classConcentration = classConcentration;
	}
	
	@Value("${ontochemexp.class.dataGroupLink}")
	private String classDataGroupLink;

	public String getClassDataGroupLink() {
		return classDataGroupLink;
	}

	public void setClassDataGroupLink(String classDataGroupLink) {
		this.classDataGroupLink = classDataGroupLink;
	}

	@Value("${ontochemexp.class.dataPoint}")
	private String classDataPoint;

	public String getClassDataPoint() {
		return classDataPoint;
	}

	public void setClassDataPoint(String classDataPoint) {
		this.classDataPoint = classDataPoint;
	}

	@Value("${ontochemexp.class.component}")
	private String classComponent;

	public String getClassComponent() {
		return classComponent;
	}

	public void setClassComponent(String classComponent) {
		this.classComponent = classComponent;
	}

	@Value("${ontochemexp.class.value}")
	private String classValue;

	public String getClassValue() {
		return classValue;
	}

	public void setClassValue(String classValue) {
		this.classValue = classValue;
	}

	@Value("${ontochemexp.class.uncertainty}")
	private String classUncertainty;

	public String getClassUncertainty() {
		return classUncertainty;
	}

	public void setClassUncertainty(String classUncertainty) {
		this.classUncertainty = classUncertainty;
	}

	@Value("${ontochemexp.class.speciesLink}")
	private String classSpeciesLink;

	public String getClassSpeciesLink() {
		return classSpeciesLink;
	}

	public void setClassSpeciesLink(String classSpeciesLink) {
		this.classSpeciesLink = classSpeciesLink;
	}

	@Value("${ontochemexp.class.derivedProperty}")
	private String classDerivedProperty;

	public String getClassDerivedProperty() {
		return classDerivedProperty;
	}

	public void setClassDerivedProperty(String classDerivedProperty) {
		this.classDerivedProperty = classDerivedProperty;
	}

	@Value("${ontochemexp.class.amount}")
	private String classAmount;

	public String getClassAmount() {
		return classAmount;
	}

	public void setClassAmount(String classAmount) {
		this.classAmount = classAmount;
	}

	@Value("${ontochemexp.class.feature}")
	private String classFeature;

	public String getClassFeature() {
		return classFeature;
	}

	public void setClassFeature(String classFeature) {
		this.classFeature = classFeature;
	}

	@Value("${ontochemexp.class.indicator}")
	private String classIndicator;

	public String getClassIndicator() {
		return classIndicator;
	}

	public void setClassIndicator(String classIndicator) {
		this.classIndicator = classIndicator;
	}

	@Value("${ontochemexp.class.observable}")
	private String classObservable;

	public String getClassObservable() {
		return classObservable;
	}

	public void setClassObservable(String classObservable) {
		this.classObservable = classObservable;
	}

	@Value("${ontochemexp.class.propertyLink}")
	private String classPropertyLink;

	public String getClassPropertyLink() {
		return classPropertyLink;
	}

	public void setClassPropertyLink(String classPropertyLink) {
		this.classPropertyLink = classPropertyLink;
	}

	@Value("${ontochemexp.class.dataAttributeLink}")
	private String classDataAttributeLink;

	public String getClassDataAttributeLink() {
		return classDataAttributeLink;
	}

	public void setClassDataAttributeLink(String classDataAttributeLink) {
		this.classDataAttributeLink = classDataAttributeLink;
	}

	@Value("${ontochemexp.class.dataPointX1}")
	private String classDataPointX1;

	public String getClassDataPointX1() {
		return classDataPointX1;
	}

	public void setClassDataPointX1(String classDataPointX1) {
		this.classDataPointX1 = classDataPointX1;
	}

	@Value("${ontochemexp.class.dataPointX2}")
	private String classDataPointX2;

	public String getClassDataPointX2() {
		return classDataPointX2;
	}

	public void setClassDataPointX2(String classDataPointX2) {
		this.classDataPointX2 = classDataPointX2;
	}

	@Value("${ontochemexp.class.dataPointX3}")
	private String classDataPointX3;

	public String getClassDataPointX3() {
		return classDataPointX3;
	}

	public void setClassDataPointX3(String classDataPointX3) {
		this.classDataPointX3 = classDataPointX3;
	}
	
	@Value("${ontochemexp.class.dataPointX4}")
	private String classDataPointX4;

	public String getClassDataPointX4() {
		return classDataPointX4;
	}

	public void setClassDataPointX4(String classDataPointX4) {
		this.classDataPointX4 = classDataPointX4;
	}
	
	@Value("${ontochemexp.class.dataPointX5}")
	private String classDataPointX5;

	public String getClassDataPointX5() {
		return classDataPointX5;
	}

	public void setClassDataPointX5(String classDataPointX5) {
		this.classDataPointX5 = classDataPointX5;
	}
	
	@Value("${ontochemexp.class.dataPointX6}")
	private String classDataPointX6;

	public String getClassDataPointX6() {
		return classDataPointX6;
	}

	public void setClassDataPointX6(String classDataPointX6) {
		this.classDataPointX6 = classDataPointX6;
	}
	
	@Value("${ontochemexp.class.dataPointX7}")
	private String classDataPointX7;

	public String getClassDataPointX7() {
		return classDataPointX7;
	}

	public void setClassDataPointX7(String classDataPointX7) {
		this.classDataPointX7 = classDataPointX7;
	}
	
	@Value("${ontochemexp.class.dataPointX8}")
	private String classDataPointX8;

	public String getClassDataPointX8() {
		return classDataPointX8;
	}

	public void setClassDataPointX8(String classDataPointX8) {
		this.classDataPointX8 = classDataPointX8;
	}
	
	@Value("${ontochemexp.class.dataPointX9}")
	private String classDataPointX9;

	public String getClassDataPointX9() {
		return classDataPointX9;
	}

	public void setClassDataPointX9(String classDataPointX9) {
		this.classDataPointX9 = classDataPointX9;
	}
	
	@Value("${ontochemexp.class.dataPointX10}")
	private String classDataPointX10;

	public String getClassDataPointX10() {
		return classDataPointX10;
	}

	public void setClassDataPointX10(String classDataPointX10) {
		this.classDataPointX10 = classDataPointX10;
	}
	
	@Value("${ontochemexp.class.dataPointX11}")
	private String classDataPointX11;

	public String getClassDataPointX11() {
		return classDataPointX11;
	}

	public void setClassDataPointX11(String classDataPointX11) {
		this.classDataPointX11 = classDataPointX11;
	}

	////////////////////////////////////////////////////////////////
	////////////////// OntoChemExp object property///////////////////
	////////////////////////////////////////////////////////////////

	@Value("${ontochemexp.object.property.hasApparatus}")
	private String objPropertyhasApparatus;

	public String getObjPropertyhasApparatus() {
		return objPropertyhasApparatus;
	}

	public void setObjPropertyhasApparatus(String objPropertyhasApparatus) {
		this.objPropertyhasApparatus = objPropertyhasApparatus;
	}

	@Value("${ontochemexp.object.property.hasAdditionalDataItem}")
	private String objPropertyhasAdditionalDataItem;

	public String getObjPropertyhasAdditionalDataItem() {
		return objPropertyhasAdditionalDataItem;
	}

	public void setObjPropertyhasAdditionalDataItem(String objPropertyhasAdditionalDataItem) {
		this.objPropertyhasAdditionalDataItem = objPropertyhasAdditionalDataItem;
	}

	@Value("${ontochemexp.object.property.hasCommonProperties}")
	private String objPropertyhasCommonProperties;

	public String getObjPropertyhasCommonProperties() {
		return objPropertyhasCommonProperties;
	}

	public void setObjPropertyhasCommonProperties(String objPropertyhasCommonProperties) {
		this.objPropertyhasCommonProperties = objPropertyhasCommonProperties;
	}

	@Value("${ontochemexp.object.property.hasCopyright}")
	private String objPropertyhasCopyright;

	public String getObjPropertyhasCopyright() {
		return objPropertyhasCopyright;
	}

	public void setObjPropertyhasCopyright(String objPropertyhasCopyright) {
		this.objPropertyhasCopyright = objPropertyhasCopyright;
	}

	@Value("${ontochemexp.object.property.hasBibliographyLink}")
	private String objPropertyhasBibliographyLink;

	public String getObjPropertyhasBibliographyLink() {
		return objPropertyhasBibliographyLink;
	}

	public void setObjPropertyhasBibliographyLink(String objPropertyhasBibliographyLink) {
		this.objPropertyhasBibliographyLink = objPropertyhasBibliographyLink;
	}

	@Value("${ontochemexp.object.property.hasDataGroup}")
	private String objPropertyhasDataGroup;

	public String getObjPropertyhasDataGroup() {
		return objPropertyhasDataGroup;
	}

	public void setObjPropertyhasDataGroup(String objPropertyhasDataGroup) {
		this.objPropertyhasDataGroup = objPropertyhasDataGroup;
	}

	@Value("${ontochemexp.object.property.hasKind}")
	private String objPropertyhasKind;

	public String getObjPropertyhasKind() {
		return objPropertyhasKind;
	}

	public void setObjPropertyhasKind(String objPropertyhasKind) {
		this.objPropertyhasKind = objPropertyhasKind;
	}

	@Value("${ontochemexp.object.property.hasMode}")
	private String objPropertyhasMode;

	public String getObjPropertyhasMode() {
		return objPropertyhasMode;
	}

	public void setObjPropertyhasMode(String objPropertyhasMode) {
		this.objPropertyhasMode = objPropertyhasMode;
	}

	@Value("${ontochemexp.object.property.hasProperty}")
	private String objPropertyhasProperty;

	public String getObjPropertyhasProperty() {
		return objPropertyhasProperty;
	}

	public void setObjPropertyhasProperty(String objPropertyhasProperty) {
		this.objPropertyhasProperty = objPropertyhasProperty;
	}

	@Value("${ontochemexp.object.property.hasUncertainty}")
	private String objPropertyhasUncertainty;

	public String getObjPropertyhasUncertainty() {
		return objPropertyhasUncertainty;
	}

	public void setObjPropertyhasUncertainty(String objPropertyhasUncertainty) {
		this.objPropertyhasUncertainty = objPropertyhasUncertainty;
	}

	@Value("${ontochemexp.object.property.hasComponent}")
	private String objPropertyhasComponent;

	public String getObjPropertyhasComponent() {
		return objPropertyhasComponent;
	}

	public void setObjPropertyhasComponent(String objPropertyhasComponent) {
		this.objPropertyhasComponent = objPropertyhasComponent;
	}

	@Value("${ontochemexp.object.property.hasDataGroupLink}")
	private String objPropertyhasDataGroupLink;

	public String getObjPropertyhasDataGroupLink() {
		return objPropertyhasDataGroupLink;
	}

	public void setObjPropertyhasDataGroupLink(String objPropertyhasDataGroupLink) {
		this.objPropertyhasDataGroupLink = objPropertyhasDataGroupLink;
	}

	@Value("${ontochemexp.object.property.hasSpeciesLink}")
	private String objPropertyhasSpeciesLink;

	public String getObjPropertyhasSpeciesLink() {
		return objPropertyhasSpeciesLink;
	}

	public void setObjPropertyhasSpeciesLink(String objPropertyhasSpeciesLink) {
		this.objPropertyhasSpeciesLink = objPropertyhasSpeciesLink;
	}

	@Value("${ontochemexp.object.property.hasDerivedProperty}")
	private String objPropertyhasDerivedProperty;

	public String getObjPropertyhasDerivedProperty() {
		return objPropertyhasDerivedProperty;
	}

	public void setObjPropertyhasDerivedProperty(String objPropertyhasDerivedProperty) {
		this.objPropertyhasDerivedProperty = objPropertyhasDerivedProperty;
	}

	@Value("${ontochemexp.object.property.hasDataPoint}")
	private String objPropertyhasDataPoint;

	public String getObjPropertyhasDataPoint() {
		return objPropertyhasDataPoint;
	}

	public void setObjPropertyhasDataPoint(String objPropertyhasDataPoint) {
		this.objPropertyhasDataPoint = objPropertyhasDataPoint;
	}

	@Value("${ontochemexp.object.property.hasAmount}")
	private String objPropertyhasAmount;

	public String getObjPropertyhasAmount() {
		return objPropertyhasAmount;
	}

	public void setObjPropertyhasAmount(String objPropertyhasAmount) {
		this.objPropertyhasAmount = objPropertyhasAmount;
	}

	@Value("${ontochemexp.object.property.hasFeature}")
	private String objPropertyhasFeature;

	public String getObjPropertyhasFeature() {
		return objPropertyhasFeature;
	}

	public void setObjPropertyhasFeature(String objPropertyhasFeature) {
		this.objPropertyhasFeature = objPropertyhasFeature;
	}

	@Value("${ontochemexp.object.property.hasIndicator}")
	private String objPropertyhasIndicator;

	public String getObjPropertyhasIndicator() {
		return objPropertyhasIndicator;
	}

	public void setObjPropertyhasIndicator(String objPropertyhasIndicator) {
		this.objPropertyhasIndicator = objPropertyhasIndicator;
	}

	@Value("${ontochemexp.object.property.hasObservable}")
	private String objPropertyhasObservable;

	public String getObjPropertyhasObservable() {
		return objPropertyhasObservable;
	}

	public void setObjPropertyhasObservable(String objPropertyhasObservable) {
		this.objPropertyhasObservable = objPropertyhasObservable;
	}

	@Value("${ontochemexp.object.property.hasPropertyLink}")
	private String objPropertyhasPropertyLink;

	public String getObjPropertyhasPropertyLink() {
		return objPropertyhasPropertyLink;
	}

	public void setObjPropertyhasPropertyLink(String objPropertyhasPropertyLink) {
		this.objPropertyhasPropertyLink = objPropertyhasPropertyLink;
	}

	@Value("${ontochemexp.object.property.hasDataAttributeLink}")
	private String objPropertyhasDataAttributeLink;

	public String getObjPropertyhasDataAttributeLink() {
		return objPropertyhasDataAttributeLink;
	}

	public void setObjPropertyhasDataAttributeLink(String objPropertyhasDataAttributeLink) {
		this.objPropertyhasDataAttributeLink = objPropertyhasDataAttributeLink;
	}

	@Value("${ontochemexp.object.property.hasDataPointX}")
	private String objPropertyhasDataPointX;

	public String getObjPropertyhasDataPointX() {
		return objPropertyhasDataPointX;
	}

	public void setObjPropertyhasDataPointX(String objPropertyhasDataPointX) {
		this.objPropertyhasDataPointX = objPropertyhasDataPointX;
	}
	
	@Value("${ontochemexp.object.property.refersTo}")
	private String objPropertyrefersTo;

	public String getObjPropertyrefersTo() {
		return objPropertyrefersTo;
	}

	public void setObjPropertyrefersTo(String objPropertyrefersTo) {
		this.objPropertyrefersTo = objPropertyrefersTo;
	}

	@Value("${ontochemexp.object.property.hasUniqueSpecies}")
	private String objPropertyhasUniqueSpecies;

	public String getObjPropertyhasUniqueSpecies() {
		return objPropertyhasUniqueSpecies;
	}

	public void setObjPropertyhasUniqueSpecies(String objPropertyhasUniqueSpecies) {
		this.objPropertyhasUniqueSpecies = objPropertyhasUniqueSpecies;
	}

	////////////////////////////////////////////////////////////////
	/////////////////// OntoChemExp data property////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${ontochemexp.data.property.hasMIME}")
	private String dataPropertyhasMIME;

	public String getDataPropertyhasMIME() {
		return dataPropertyhasMIME;
	}

	public void setDataPropertyhasMIME(String dataPropertyhasMIME) {
		this.dataPropertyhasMIME = dataPropertyhasMIME;
	}

	@Value("${ontochemexp.data.property.hasItemType}")
	private String dataPropertyhasItemType;

	public String getDataPropertyhasItemType() {
		return dataPropertyhasItemType;
	}

	public void setDataPropertyhasItemType(String dataPropertyhasItemType) {
		this.dataPropertyhasItemType = dataPropertyhasItemType;
	}

	@Value("${ontochemexp.data.property.hasDescription}")
	private String dataPropertyhasDescription;

	public String getDataPropertyhasDescription() {
		return dataPropertyhasDescription;
	}

	public void setDataPropertyhasDescription(String dataPropertyhasDescription) {
		this.dataPropertyhasDescription = dataPropertyhasDescription;
	}

	@Value("${ontochemexp.data.property.hasPreferredKey}")
	private String dataPropertyhasPreferredKey;

	public String getDataPropertyhasPreferredKey() {
		return dataPropertyhasPreferredKey;
	}

	public void setDataPropertyhasPreferredKey(String dataPropertyhasPreferredKey) {
		this.dataPropertyhasPreferredKey = dataPropertyhasPreferredKey;
	}

	@Value("${ontochemexp.data.property.hasPrimeID}")
	private String dataPropertyhasPrimeID;

	public String getDataPropertyhasPrimeID() {
		return dataPropertyhasPrimeID;
	}

	public void setDataPropertyhasPrimeID(String dataPropertyhasPrimeID) {
		this.dataPropertyhasPrimeID = dataPropertyhasPrimeID;
	}

	@Value("${ontochemexp.data.property.hasID}")
	private String dataPropertyhasID;

	public String getDataPropertyhasID() {
		return dataPropertyhasID;
	}

	public void setDataPropertyhasID(String dataPropertyhasID) {
		this.dataPropertyhasID = dataPropertyhasID;
	}

	@Value("${ontochemexp.data.property.hasLabel}")
	private String dataPropertyhasLabel;

	public String getDataPropertyhasLabel() {
		return dataPropertyhasLabel;
	}

	public void setDataPropertyhasLabel(String dataPropertyhasLabel) {
		this.dataPropertyhasLabel = dataPropertyhasLabel;
	}

	@Value("${ontochemexp.data.property.hasDataPointForm}")
	private String dataPropertyhasDataPointForm;

	public String getDataPropertyhasDataPointForm() {
		return dataPropertyhasDataPointForm;
	}

	public void setDataPropertyhasDataPointForm(String dataPropertyhasDataPointForm) {
		this.dataPropertyhasDataPointForm = dataPropertyhasDataPointForm;
	}

	@Value("${ontochemexp.data.property.hasType}")
	private String dataPropertyhasType;

	public String getDataPropertyhasType() {
		return dataPropertyhasType;
	}

	public void setDataPropertyhasType(String dataPropertyhasType) {
		this.dataPropertyhasType = dataPropertyhasType;
	}

	@Value("${ontochemexp.data.property.hasName}")
	private String dataPropertyhasName;

	public String getDataPropertyhasName() {
		return dataPropertyhasName;
	}

	public void setDataPropertyhasName(String dataPropertyhasName) {
		this.dataPropertyhasName = dataPropertyhasName;
	}

	@Value("${ontochemexp.data.property.hasUnits}")
	private String dataPropertyhasUnits;

	public String getDataPropertyhasUnits() {
		return dataPropertyhasUnits;
	}

	public void setDataPropertyhasUnits(String dataPropertyhasUnits) {
		this.dataPropertyhasUnits = dataPropertyhasUnits;
	}

	@Value("${ontochemexp.data.property.hasDerivedPropertyExists}")
	private String dataPropertyhasDerivedPropertyExists;

	public String getDataPropertyhasDerivedPropertyExists() {
		return dataPropertyhasDerivedPropertyExists;
	}

	public void setDataPropertyhasDerivedPropertyExists(String dataPropertyhasDerivedPropertyExists) {
		this.dataPropertyhasDerivedPropertyExists = dataPropertyhasDerivedPropertyExists;
	}

	@Value("${ontochemexp.data.property.hasBound}")
	private String dataPropertyhasBound;

	public String getDataPropertyhasBound() {
		return dataPropertyhasBound;
	}

	public void setDataPropertyhasBound(String dataPropertyhasBound) {
		this.dataPropertyhasBound = dataPropertyhasBound;
	}

	@Value("${ontochemexp.data.property.hasKind}")
	private String dataPropertyhasKind;

	public String getDataPropertyhasKind() {
		return dataPropertyhasKind;
	}

	public void setDataPropertyhasKind(String dataPropertyhasKind) {
		this.dataPropertyhasKind = dataPropertyhasKind;
	}

	@Value("${ontochemexp.data.property.hasTransformation}")
	private String dataPropertyhasTransformation;

	public String getDataPropertyhasTransformation() {
		return dataPropertyhasTransformation;
	}

	public void setDataPropertyhasTransformation(String dataPropertyhasTransformation) {
		this.dataPropertyhasTransformation = dataPropertyhasTransformation;
	}

	@Value("${ontochemexp.data.property.hasVariableID}")
	private String dataPropertyhasVariableID;

	public String getDataPropertyhasVariableID() {
		return dataPropertyhasVariableID;
	}

	public void setDataPropertyhasVariableID(String dataPropertyhasVariableID) {
		this.dataPropertyhasVariableID = dataPropertyhasVariableID;
	}

	@Value("${ontochemexp.data.property.hasDataGroupID}")
	private String dataPropertyhasDataGroupID;

	public String getDataPropertyhasDataGroupID() {
		return dataPropertyhasDataGroupID;
	}

	public void setDataPropertyhasDataGroupID(String dataPropertyhasDataGroupID) {
		this.dataPropertyhasDataGroupID = dataPropertyhasDataGroupID;
	}

	@Value("${ontochemexp.data.property.hasDataPointID}")
	private String dataPropertyhasDataPointID;

	public String getDataPropertyhasDataPointID() {
		return dataPropertyhasDataPointID;
	}

	public void setDataPropertyhasDataPointID(String dataPropertyhasDataPointID) {
		this.dataPropertyhasDataPointID = dataPropertyhasDataPointID;
	}

	@Value("${ontochemexp.data.property.hasValue}")
	private String dataPropertyhasValue;

	public String getDataPropertyhasValue() {
		return dataPropertyhasValue;
	}

	public void setDataPropertyhasValue(String dataPropertyhasValue) {
		this.dataPropertyhasValue = dataPropertyhasValue;
	}

	@Value("${ontochemexp.data.property.hasPropertyID}")
	private String dataPropertyhasPropertyID;

	public String getDataPropertyhasPropertyID() {
		return dataPropertyhasPropertyID;
	}

	public void setDataPropertyhasPropertyID(String dataPropertyhasPropertyID) {
		this.dataPropertyhasPropertyID = dataPropertyhasPropertyID;
	}
	
	@Value("${ontochemexp.data.property.hasSourceType}")
	private String dataPropertyhasSourceType;

	public String getDataPropertyhasSourceType() {
		return dataPropertyhasSourceType;
	}

	public void setDataPropertyhasSourceType(String dataPropertyhasSourceType) {
		this.dataPropertyhasSourceType = dataPropertyhasSourceType;
	}
	
	@Value("${ontochemexp.data.property.hasCAS}")
	private String dataPropertyhasCAS;
	
	public String getDataPropertyhasCAS() {
		return dataPropertyhasCAS;
	}

	public void setDataPropertyhasCAS(String dataPropertyhasCAS) {
		this.dataPropertyhasCAS = dataPropertyhasCAS;
	}
	
	@Value("${ontochemexp.data.property.hasInChI}")
	private String dataPropertyhasInChI;
	
	public String getDataPropertyhasInChI() {
		return dataPropertyhasInChI;
	}

	public void setDataPropertyhasInChI(String dataPropertyhasInChI) {
		this.dataPropertyhasInChI = dataPropertyhasInChI;
	}
	
	@Value("${ontochemexp.data.property.hasSMILES}")
	private String dataPropertyhasSMILES;
	
	public String getDataPropertyhasSMILES() {
		return dataPropertyhasSMILES;
	}

	public void setDataPropertyhasSMILES(String dataPropertyhasSMILES) {
		this.dataPropertyhasSMILES = dataPropertyhasSMILES;
	}
	
	////////////////////////////////////////////////////////////////
	///////////////////// Publication-related //////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${ontokin.reference.agent}")
	private String ontoKinReferenceAgent;
	
	@Value("${ontokin.reference.person}")
	private String ontoKinReferencePerson;
	
	@Value("${ontokin.reference.organization}")
	private String ontoKinReferenceOrganization;
	
	@Value("${ontokin.reference.journal}")
	private String ontoKinReferenceJournal;
	
	@Value("${ontokin.reference.publication.specification}")
	private String ontoKinReferencePublicationSpecification;
	
	@Value("${ontokin.reference.journal.specification}")
	private String ontoKinReferenceJournalSpecification;
	
	@Value("${ontokin.reference.conference.proceedings.specification}")
	private String ontoKinReferenceProceedingsSpecification;
	
	@Value("${ontokin.reference.preprint.specification}")
	private String ontoKinReferencePreprintSpecification;
	
	@Value("${ontokin.reference.specifies}")
	private String ontoKinReferenceSpecifies;
	
	@Value("${ontokin.reference.has.publication.specification}")
	private String ontoKinReferencehasPublicationSpecification;
	
	@Value("${ontokin.reference.has.contributor}")
	private String ontoKinReferencehasContributor;
	
	@Value("${ontochemexp.bibliographyLink.hasDOI}")
	private String dataPropertyhasDOI;
	
	@Value("${ontokin.reference.issn}")
	private String ontoKinReferenceISSN;
	
	@Value("${ontokin.reference.publisher}")
	private String ontoKinReferencePublisher;
	
	@Value("${ontokin.reference.title}")
	private String ontoKinReferenceTitle;
	
	@Value("${ontokin.reference.family.name}")
	private String ontoKinReferenceFamilyName;
	
	@Value("${ontokin.reference.given.name}")
	private String ontoKinReferenceGivenName;
	
	@Value("${ontokin.reference.name}")
	private String ontoKinReferenceName;
	
	@Value("${ontokin.reference.journal.volume}")
	private String ontoKinReferenceJournalVolume;
	
	@Value("${ontokin.reference.document.page.start}")
	private String ontoKinReferenceDocumentPageStart;
	
	@Value("${ontokin.reference.document.page.end}")
	private String ontoKinReferenceDocumentPageEnd;

	public String getOntoKinReferenceAgent() {
		return ontoKinReferenceAgent;
	}

	public void setOntoKinReferenceAgent(String ontoKinReferenceAgent) {
		this.ontoKinReferenceAgent = ontoKinReferenceAgent;
	}

	public String getOntoKinReferencePerson() {
		return ontoKinReferencePerson;
	}

	public void setOntoKinReferencePerson(String ontoKinReferencePerson) {
		this.ontoKinReferencePerson = ontoKinReferencePerson;
	}

	public String getOntoKinReferenceOrganization() {
		return ontoKinReferenceOrganization;
	}

	public void setOntoKinReferenceOrganization(String ontoKinReferenceOrganization) {
		this.ontoKinReferenceOrganization = ontoKinReferenceOrganization;
	}

	public String getOntoKinReferenceJournal() {
		return ontoKinReferenceJournal;
	}

	public void setOntoKinReferenceJournal(String ontoKinReferenceJournal) {
		this.ontoKinReferenceJournal = ontoKinReferenceJournal;
	}

	public String getOntoKinReferencePublicationSpecification() {
		return ontoKinReferencePublicationSpecification;
	}

	public void setOntoKinReferencePublicationSpecification(String ontoKinReferencePublicationSpecification) {
		this.ontoKinReferencePublicationSpecification = ontoKinReferencePublicationSpecification;
	}

	public String getOntoKinReferenceJournalSpecification() {
		return ontoKinReferenceJournalSpecification;
	}

	public void setOntoKinReferenceJournalSpecification(String ontoKinReferenceJournalSpecification) {
		this.ontoKinReferenceJournalSpecification = ontoKinReferenceJournalSpecification;
	}

	public String getOntoKinReferenceProceedingsSpecification() {
		return ontoKinReferenceProceedingsSpecification;
	}

	public void setOntoKinReferenceProceedingsSpecification(String ontoKinReferenceProceedingsSpecification) {
		this.ontoKinReferenceProceedingsSpecification = ontoKinReferenceProceedingsSpecification;
	}

	public String getOntoKinReferencePreprintSpecification() {
		return ontoKinReferencePreprintSpecification;
	}

	public void setOntoKinReferencePreprintSpecification(String ontoKinReferencePreprintSpecification) {
		this.ontoKinReferencePreprintSpecification = ontoKinReferencePreprintSpecification;
	}

	public String getOntoKinReferenceSpecifies() {
		return ontoKinReferenceSpecifies;
	}

	public void setOntoKinReferenceSpecifies(String ontoKinReferenceSpecifies) {
		this.ontoKinReferenceSpecifies = ontoKinReferenceSpecifies;
	}

	public String getOntoKinReferencehasPublicationSpecification() {
		return ontoKinReferencehasPublicationSpecification;
	}

	public void setOntoKinReferencehasPublicationSpecification(String ontoKinReferencehasPublicationSpecification) {
		this.ontoKinReferencehasPublicationSpecification = ontoKinReferencehasPublicationSpecification;
	}

	public String getOntoKinReferencehasContributor() {
		return ontoKinReferencehasContributor;
	}

	public void setOntoKinReferencehasContributor(String ontoKinReferencehasContributor) {
		this.ontoKinReferencehasContributor = ontoKinReferencehasContributor;
	}

	public String getDataPropertyhasDOI() {
		return dataPropertyhasDOI;
	}

	public void setDataPropertyhasDOI(String dataPropertyhasDOI) {
		this.dataPropertyhasDOI = dataPropertyhasDOI;
	}

	public String getOntoKinReferenceISSN() {
		return ontoKinReferenceISSN;
	}

	public void setOntoKinReferenceISSN(String ontoKinReferenceISSN) {
		this.ontoKinReferenceISSN = ontoKinReferenceISSN;
	}

	public String getOntoKinReferencePublisher() {
		return ontoKinReferencePublisher;
	}

	public void setOntoKinReferencePublisher(String ontoKinReferencePublisher) {
		this.ontoKinReferencePublisher = ontoKinReferencePublisher;
	}

	public String getOntoKinReferenceTitle() {
		return ontoKinReferenceTitle;
	}

	public void setOntoKinReferenceTitle(String ontoKinReferenceTitle) {
		this.ontoKinReferenceTitle = ontoKinReferenceTitle;
	}

	public String getOntoKinReferenceFamilyName() {
		return ontoKinReferenceFamilyName;
	}

	public void setOntoKinReferenceFamilyName(String ontoKinReferenceFamilyName) {
		this.ontoKinReferenceFamilyName = ontoKinReferenceFamilyName;
	}

	public String getOntoKinReferenceGivenName() {
		return ontoKinReferenceGivenName;
	}

	public void setOntoKinReferenceGivenName(String ontoKinReferenceGivenName) {
		this.ontoKinReferenceGivenName = ontoKinReferenceGivenName;
	}

	public String getOntoKinReferenceName() {
		return ontoKinReferenceName;
	}

	public void setOntoKinReferenceName(String ontoKinReferenceName) {
		this.ontoKinReferenceName = ontoKinReferenceName;
	}

	public String getOntoKinReferenceJournalVolume() {
		return ontoKinReferenceJournalVolume;
	}

	public void setOntoKinReferenceJournalVolume(String ontoKinReferenceJournalVolume) {
		this.ontoKinReferenceJournalVolume = ontoKinReferenceJournalVolume;
	}

	public String getOntoKinReferenceDocumentPageStart() {
		return ontoKinReferenceDocumentPageStart;
	}

	public void setOntoKinReferenceDocumentPageStart(String ontoKinReferenceDocumentPageStart) {
		this.ontoKinReferenceDocumentPageStart = ontoKinReferenceDocumentPageStart;
	}

	public String getOntoKinReferenceDocumentPageEnd() {
		return ontoKinReferenceDocumentPageEnd;
	}

	public void setOntoKinReferenceDocumentPageEnd(String ontoKinReferenceDocumentPageEnd) {
		this.ontoKinReferenceDocumentPageEnd = ontoKinReferenceDocumentPageEnd;
	}
	
	////////////////////////////////////////////////////////////////
	///////////////////// Provenance-related ///////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${ontochemexp.provenance}")
	private String ontoChemExpProvenance;
	
	@Value("${ontochemexp.provenance.modification}")
	private String ontoChemExpProvenanceModification;
	
	@Value("${ontochemexp.experiment.hasPerformer}")
	private String ontoChemExpExperimenthasPerformer;
	
	@Value("${ontochemexp.provenance.hasProvenance}")
	private String ontoChemExpProvenancehasProvenance;
	
	@Value("${ontochemexp.provenance.createdBy}")
	private String ontoChemExpProvenancecreatedBy;
	
	@Value("${ontochemexp.provenance.hasModification}")
	private String ontoChemExpProvenancehasModification;
	
	@Value("${ontochemexp.provenance.modification.modifiedBy}")
	private String ontoChemExpProvenanceModificationmodifiedBy;
	
	@Value("${ontochemexp.provenance.createdAt}")
	private String ontoChemExpProvenancecreatedAt;
	
	@Value("${ontochemexp.provenance.hasDataSource}")
	private String ontoChemExpProvenancehasDataSource;
	
	@Value("${ontochemexp.provenance.hasPatent}")
	private String ontoChemExpProvenancehasPatent;
	
	@Value("${ontochemexp.provenance.modification.modifiedAt}")
	private String ontoChemExpProvenanceModificationmodifiedAt;
	
	@Value("${ontochemexp.provenance.modification.hasModificationDetails}")
	private String ontoChemExpProvenanceModificationhasModificationDetails;

	public String getOntoChemExpProvenance() {
		return ontoChemExpProvenance;
	}

	public void setOntoChemExpProvenance(String ontoChemExpProvenance) {
		this.ontoChemExpProvenance = ontoChemExpProvenance;
	}

	public String getOntoChemExpProvenanceModification() {
		return ontoChemExpProvenanceModification;
	}

	public void setOntoChemExpProvenanceModification(String ontoChemExpProvenanceModification) {
		this.ontoChemExpProvenanceModification = ontoChemExpProvenanceModification;
	}

	public String getOntoChemExpExperimenthasPerformer() {
		return ontoChemExpExperimenthasPerformer;
	}

	public void setOntoChemExpExperimenthasPerformer(String ontoChemExpExperimenthasPerformer) {
		this.ontoChemExpExperimenthasPerformer = ontoChemExpExperimenthasPerformer;
	}

	public String getOntoChemExpProvenancehasProvenance() {
		return ontoChemExpProvenancehasProvenance;
	}

	public void setOntoChemExpProvenancehasProvenance(String ontoChemExpProvenancehasProvenance) {
		this.ontoChemExpProvenancehasProvenance = ontoChemExpProvenancehasProvenance;
	}

	public String getOntoChemExpProvenancecreatedBy() {
		return ontoChemExpProvenancecreatedBy;
	}

	public void setOntoChemExpProvenancecreatedBy(String ontoChemExpProvenancecreatedBy) {
		this.ontoChemExpProvenancecreatedBy = ontoChemExpProvenancecreatedBy;
	}

	public String getOntoChemExpProvenancehasModification() {
		return ontoChemExpProvenancehasModification;
	}

	public void setOntoChemExpProvenancehasModification(String ontoChemExpProvenancehasModification) {
		this.ontoChemExpProvenancehasModification = ontoChemExpProvenancehasModification;
	}

	public String getOntoChemExpProvenanceModificationmodifiedBy() {
		return ontoChemExpProvenanceModificationmodifiedBy;
	}

	public void setOntoChemExpProvenanceModificationmodifiedBy(String ontoChemExpProvenanceModificationmodifiedBy) {
		this.ontoChemExpProvenanceModificationmodifiedBy = ontoChemExpProvenanceModificationmodifiedBy;
	}

	public String getOntoChemExpProvenancecreatedAt() {
		return ontoChemExpProvenancecreatedAt;
	}

	public void setOntoChemExpProvenancecreatedAt(String ontoChemExpProvenancecreatedAt) {
		this.ontoChemExpProvenancecreatedAt = ontoChemExpProvenancecreatedAt;
	}

	public String getOntoChemExpProvenancehasDataSource() {
		return ontoChemExpProvenancehasDataSource;
	}

	public void setOntoChemExpProvenancehasDataSource(String ontoChemExpProvenancehasDataSource) {
		this.ontoChemExpProvenancehasDataSource = ontoChemExpProvenancehasDataSource;
	}

	public String getOntoChemExpProvenancehasPatent() {
		return ontoChemExpProvenancehasPatent;
	}

	public void setOntoChemExpProvenancehasPatent(String ontoChemExpProvenancehasPatent) {
		this.ontoChemExpProvenancehasPatent = ontoChemExpProvenancehasPatent;
	}

	public String getOntoChemExpProvenanceModificationmodifiedAt() {
		return ontoChemExpProvenanceModificationmodifiedAt;
	}

	public void setOntoChemExpProvenanceModificationmodifiedAt(String ontoChemExpProvenanceModificationmodifiedAt) {
		this.ontoChemExpProvenanceModificationmodifiedAt = ontoChemExpProvenanceModificationmodifiedAt;
	}

	public String getOntoChemExpProvenanceModificationhasModificationDetails() {
		return ontoChemExpProvenanceModificationhasModificationDetails;
	}

	public void setOntoChemExpProvenanceModificationhasModificationDetails(
			String ontoChemExpProvenanceModificationhasModificationDetails) {
		this.ontoChemExpProvenanceModificationhasModificationDetails = ontoChemExpProvenanceModificationhasModificationDetails;
	}
	
	@Value("${ontochemexp.expSpecs}")
	private String ontoChemExpExpSpecs;
	
	@Value("${ontochemexp.expSpecs.ignitionType}")
	private String ontoChemExpExpSpecsIgnitionType;
	
	@Value("${ontochemexp.expSpecs.timeShift}")
	private String ontoChemExpExpSpecsTimeShift;
	
	@Value("${ontochemexp.expSpecs.hasExpSpecs}")
	private String ontoChemExphasExpSpecs;
	
	@Value("${ontochemexp.expSpecs.hasIgnitionType}")
	private String ontoChemExpExpSpecshasIgnitionType;
	
	@Value("${ontochemexp.expSpecs.hasTimeShift}")
	private String ontoChemExpExpSpecshasTimeShift;
	
	@Value("${ontochemexp.expSpecs.hasExpType}")
	private String ontoChemExpExpSpecshasExpType;
	
	@Value("${ontochemexp.expSpecs.hasDatTarget}")
	private String ontoChemExpExpSpecshasDatTarget;
	
	@Value("${ontochemexp.expSpecs.hasDatType}")
	private String ontoChemExpExpSpecshasDatType;
	
	@Value("${ontochemexp.expSpecs.hasDatAmount}")
	private String ontoChemExpExpSpecshasDatAmount;

	public String getOntoChemExpExpSpecs() {
		return ontoChemExpExpSpecs;
	}

	public void setOntoChemExpExpSpecs(String ontoChemExpExpSpecs) {
		this.ontoChemExpExpSpecs = ontoChemExpExpSpecs;
	}

	public String getOntoChemExpExpSpecsIgnitionType() {
		return ontoChemExpExpSpecsIgnitionType;
	}

	public void setOntoChemExpExpSpecsIgnitionType(String ontoChemExpExpSpecsIgnitionType) {
		this.ontoChemExpExpSpecsIgnitionType = ontoChemExpExpSpecsIgnitionType;
	}

	public String getOntoChemExpExpSpecsTimeShift() {
		return ontoChemExpExpSpecsTimeShift;
	}

	public void setOntoChemExpExpSpecsTimeShift(String ontoChemExpExpSpecsTimeShift) {
		this.ontoChemExpExpSpecsTimeShift = ontoChemExpExpSpecsTimeShift;
	}

	public String getOntoChemExphasExpSpecs() {
		return ontoChemExphasExpSpecs;
	}

	public void setOntoChemExphasExpSpecs(String ontoChemExphasExpSpecs) {
		this.ontoChemExphasExpSpecs = ontoChemExphasExpSpecs;
	}

	public String getOntoChemExpExpSpecshasIgnitionType() {
		return ontoChemExpExpSpecshasIgnitionType;
	}

	public void setOntoChemExpExpSpecshasIgnitionType(String ontoChemExpExpSpecshasIgnitionType) {
		this.ontoChemExpExpSpecshasIgnitionType = ontoChemExpExpSpecshasIgnitionType;
	}

	public String getOntoChemExpExpSpecshasTimeShift() {
		return ontoChemExpExpSpecshasTimeShift;
	}

	public void setOntoChemExpExpSpecshasTimeShift(String ontoChemExpExpSpecshasTimeShift) {
		this.ontoChemExpExpSpecshasTimeShift = ontoChemExpExpSpecshasTimeShift;
	}

	public String getOntoChemExpExpSpecshasExpType() {
		return ontoChemExpExpSpecshasExpType;
	}

	public void setOntoChemExpExpSpecshasExpType(String ontoChemExpExpSpecshasExpType) {
		this.ontoChemExpExpSpecshasExpType = ontoChemExpExpSpecshasExpType;
	}

	public String getOntoChemExpExpSpecshasDatTarget() {
		return ontoChemExpExpSpecshasDatTarget;
	}

	public void setOntoChemExpExpSpecshasDatTarget(String ontoChemExpExpSpecshasDatTarget) {
		this.ontoChemExpExpSpecshasDatTarget = ontoChemExpExpSpecshasDatTarget;
	}

	public String getOntoChemExpExpSpecshasDatType() {
		return ontoChemExpExpSpecshasDatType;
	}

	public void setOntoChemExpExpSpecshasDatType(String ontoChemExpExpSpecshasDatType) {
		this.ontoChemExpExpSpecshasDatType = ontoChemExpExpSpecshasDatType;
	}

	public String getOntoChemExpExpSpecshasDatAmount() {
		return ontoChemExpExpSpecshasDatAmount;
	}

	public void setOntoChemExpExpSpecshasDatAmount(String ontoChemExpExpSpecshasDatAmount) {
		this.ontoChemExpExpSpecshasDatAmount = ontoChemExpExpSpecshasDatAmount;
	}
}
