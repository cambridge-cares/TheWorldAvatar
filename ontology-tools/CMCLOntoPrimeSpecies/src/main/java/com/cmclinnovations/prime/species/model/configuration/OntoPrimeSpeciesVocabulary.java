package com.cmclinnovations.prime.species.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:ontoprimespecies.vocabulary.properties")
public class OntoPrimeSpeciesVocabulary {
	////////////////////////////////////////////////////////////////
	/////////////////////// OntoSpecies class////////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${ontokin.class.element}")
	private String osClassElement;
	
	public String getOSClassElement() {
		return osClassElement;
	}
	
	public void setOSClassElement(String osClassElement) {
		this.osClassElement = osClassElement;
	}
	
	@Value("${ontokin.class.elementNumber}")
	private String osClassElementNumber;
	
	public String getOSClassElementNumber() {
		return osClassElementNumber;
	}
	
	public void setOSClassElementNumber(String osClassElementNumber) {
		this.osClassElementNumber = osClassElementNumber;
	}
	
	@Value("${ontokin.class.reference}")
	private String osClassReference;
	
	public String getOSClassReference() {
		return osClassReference;
	}
	
	public void setOSClassReference(String osClassReference) {
		this.osClassReference = osClassReference;
	}
	
	@Value("${ontospecies.class.empiricalFormula}")
	private String osClassEmpiricalFormula;
	
	public String getOSClassEmpiricalFormula() {
		return osClassEmpiricalFormula;
	}
	
	public void setOSClassEmpiricalFormula(String osClassEmpiricalFormula) {
		this.osClassEmpiricalFormula = osClassEmpiricalFormula;
	}
	
	@Value("${ontospecies.class.enthalpyOfFormationCalculationEBR}")
	private String osClassEnthalpyOfFormationCalculationEBR;
	
	public String getOSClassEnthalpyOfFormationCalculationEBR() {
		return osClassEnthalpyOfFormationCalculationEBR;
	}
	
	public void setOSClassEnthalpyOfFormationCalculationEBR(String osClassEnthalpyOfFormationCalculationEBR) {
		this.osClassEnthalpyOfFormationCalculationEBR = osClassEnthalpyOfFormationCalculationEBR;
	}
	
	@Value("${ontospecies.class.provenance}")
	private String osClassProvenance;
	
	public String getOSClassProvenance() {
		return osClassProvenance;
	}
	
	public void setOSClassProvenance(String osClassProvenance) {
		this.osClassProvenance = osClassProvenance;
	}
	
	@Value("${ontospecies.class.referenceSpecies}")
	private String osClassReferenceSpecies;
	
	public String getOSClassReferenceSpecies() {
		return osClassReferenceSpecies;
	}
	
	public void setOSClassReferenceSpecies(String osClassReferenceSpecies) {
		this.osClassReferenceSpecies = osClassReferenceSpecies;
	}
	
	@Value("${ontospecies.class.species}")
	private String osClassSpecies;
	
	public String getOSClassSpecies() {
		return osClassSpecies;
	}
	
	public void setOSClassSpecies(String osClassSpecies) {
		this.osClassSpecies = osClassSpecies;
	}
	
	@Value("${ontospecies.class.standardEnthalpyOfFormation}")
	private String osClassStandardEnthalpyOfFormation;
	
	public String getOSClassStandardEnthalpyOfFormation() {
		return osClassStandardEnthalpyOfFormation;
	}
	
	public void setOSClassStandardEnthalpyOfFormation(String osClassStandardEnthalpyOfFormation) {
		this.osClassStandardEnthalpyOfFormation = osClassStandardEnthalpyOfFormation;
	}
	
	@Value("${ontospecies.class.temperature}")
	private String osClassTemperature;
	
	public String getOSClassTemperature() {
		return osClassTemperature;
	}
	
	public void setOSClassTemperature(String osClassTemperature) {
		this.osClassTemperature = osClassTemperature;
	}
	
	@Value("${ontospecies.class.weblink}")
	private String osClassWeblink;
	
	public String getOSClassWeblink() {
		return osClassWeblink;
	}
	
	public void setOSClassWeblink(String osClassWeblink) {
		this.osClassWeblink = osClassWeblink;
	}
	
	////////////////////////////////////////////////////////////////
	////////////////// OntoSpecies object property///////////////////
	////////////////////////////////////////////////////////////////
	@Value("${ontokin.object.property.hasElement}")
	private String osObjPropertyhasElement;

	public String getOSObjPropertyhasElement() {
		return osObjPropertyhasElement;
	}

	public void setOSObjPropertyhasElement(String osObjPropertyhasElement) {
		this.osObjPropertyhasElement = osObjPropertyhasElement;
	}
	
	@Value("${ontokin.object.property.hasElementNumber}")
	private String osObjPropertyhasElementNumber;

	public String getOSObjPropertyhasElementNumber() {
		return osObjPropertyhasElementNumber;
	}

	public void setOSObjPropertyhasElementNumber(String osObjPropertyhasElementNumber) {
		this.osObjPropertyhasElementNumber = osObjPropertyhasElementNumber;
	}
	
	@Value("${ontospecies.object.property.hasEmpiricalFormula}")
	private String osObjPropertyhasEmpiricalFormula;

	public String getOSObjPropertyhasEmpiricalFormula() {
		return osObjPropertyhasEmpiricalFormula;
	}

	public void setOSObjPropertyhasEmpiricalFormula(String osObjPropertyhasEmpiricalFormula) {
		this.osObjPropertyhasEmpiricalFormula = osObjPropertyhasEmpiricalFormula;
	}
	
	@Value("${ontospecies.object.property.hasProvenance}")
	private String osObjPropertyhasProvenance;

	public String getOSObjPropertyhasProvenance() {
		return osObjPropertyhasProvenance;
	}

	public void setOSObjPropertyhasProvenance(String osObjPropertyhasProvenance) {
		this.osObjPropertyhasProvenance = osObjPropertyhasProvenance;
	}
	
	@Value("${ontospecies.object.property.hasReferenceSpecies}")
	private String osObjPropertyhasReferenceSpecies;

	public String getOSObjPropertyhasReferenceSpecies() {
		return osObjPropertyhasReferenceSpecies;
	}

	public void setOSObjPropertyhasReferenceSpecies(String osObjPropertyhasReferenceSpecies) {
		this.osObjPropertyhasReferenceSpecies = osObjPropertyhasReferenceSpecies;
	}
	
	@Value("${ontospecies.object.property.hasReferenceTemperature}")
	private String osObjPropertyhasReferenceTemperature;

	public String getOSObjPropertyhasReferenceTemperature() {
		return osObjPropertyhasReferenceTemperature;
	}

	public void setOSObjPropertyhasReferenceTemperature(String osObjPropertyhasReferenceTemperature) {
		this.osObjPropertyhasReferenceTemperature = osObjPropertyhasReferenceTemperature;
	}
	
	@Value("${ontospecies.object.property.hasSpecies}")
	private String osObjPropertyhasSpecies;

	public String getOSObjPropertyhasSpecies() {
		return osObjPropertyhasSpecies;
	}

	public void setOSObjPropertyhasSpecies(String osObjPropertyhasSpecies) {
		this.osObjPropertyhasSpecies = osObjPropertyhasSpecies;
	}
	
	@Value("${ontospecies.object.property.hasStandardEnthalpyOfFormation}")
	private String osObjPropertyhasStandardEnthalpyOfFormation;

	public String getOSObjPropertyhasStandardEnthalpyOfFormation() {
		return osObjPropertyhasStandardEnthalpyOfFormation;
	}

	public void setOSObjPropertyhasStandardEnthalpyOfFormation(String osObjPropertyhasStandardEnthalpyOfFormation) {
		this.osObjPropertyhasStandardEnthalpyOfFormation = osObjPropertyhasStandardEnthalpyOfFormation;
	}
	
	@Value("${ontospecies.object.property.hasWeblink}")
	private String osObjPropertyhasWeblink;

	public String getOSObjPropertyhasWeblink() {
		return osObjPropertyhasWeblink;
	}

	public void setOSObjPropertyhasWeblink(String osObjPropertyhasWeblink) {
		this.osObjPropertyhasWeblink = osObjPropertyhasWeblink;
	}
	
	@Value("${ontokin.object.property.indicatesNumberOf}")
	private String osObjPropertyindicatesNumberOf;
	
	public String getOSObjPropertyindicatesNumberOf() {
		return osObjPropertyindicatesNumberOf;
	}
	
	public void setOSObjPropertyindicatesNumberOf(String osObjPropertyindicatesNumberOf) {
		this.osObjPropertyindicatesNumberOf = osObjPropertyindicatesNumberOf;
	}
	
	////////////////////////////////////////////////////////////////
	/////////////////// OntoSpecies data property////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${ontospecies.data.property.hasCasRegistryID}")
	private String osDataPropertyhasCasRegistryID;

	public String getOSDataPropertyhasCasRegistryID() {
		return osDataPropertyhasCasRegistryID;
	}

	public void setOSDataPropertyhasCasRegistryID(String osDataPropertyhasCasRegistryID) {
		this.osDataPropertyhasCasRegistryID = osDataPropertyhasCasRegistryID;
	}
	
	@Value("${ontospecies.data.property.hasDateOfAccess}")
	private String osDataPropertyhasDateOfAccess;

	public String getOSDataPropertyhasDateOfAccess() {
		return osDataPropertyhasDateOfAccess;
	}

	public void setOSDataPropertyhasDateOfAccess(String osDataPropertyhasDateOfAccess) {
		this.osDataPropertyhasDateOfAccess = osDataPropertyhasDateOfAccess;
	}
	
	@Value("${ontospecies.data.property.hasInChI}")
	private String osDataPropertyhasInChI;

	public String getOSDataPropertyhasInChI() {
		return osDataPropertyhasInChI;
	}

	public void setOSDataPropertyhasInChI(String osDataPropertyhasInChI) {
		this.osDataPropertyhasInChI = osDataPropertyhasInChI;
	}
	
	@Value("${ontospecies.data.property.hasInChIKey}")
	private String osDataPropertyhasInChIKey;

	public String getOSDataPropertyhasInChIKey() {
		return osDataPropertyhasInChIKey;
	}

	public void setOSDataPropertyhasInChIKey(String osDataPropertyhasInChIKey) {
		this.osDataPropertyhasInChIKey = osDataPropertyhasInChIKey;
	}
	
	@Value("${ontospecies.data.property.isReliable}")
	private String osDataPropertyisReliable;

	public String getOSDataPropertyisReliable() {
		return osDataPropertyisReliable;
	}

	public void setOSDataPropertyisReliable(String osDataPropertyisReliable) {
		this.osDataPropertyisReliable = osDataPropertyisReliable;
	}
	
	@Value("${ontospecies.data.property.hasMass}")
	private String osDataPropertyhasMass;

	public String getOSDataPropertyhasMass() {
		return osDataPropertyhasMass;
	}

	public void setOSDataPropertyhasMass(String osDataPropertyhasMass) {
		this.osDataPropertyhasMass = osDataPropertyhasMass;
	}
	
	@Value("${ontospecies.data.property.hasUnits}")
	private String osDataPropertyhasUnits;

	public String getOSDataPropertyhasUnits() {
		return osDataPropertyhasUnits;
	}

	public void setOSDataPropertyhasUnits(String osDataPropertyhasUnits) {
		this.osDataPropertyhasUnits = osDataPropertyhasUnits;
	}
	
	@Value("${ontospecies.data.property.hasValue}")
	private String osDataPropertyhasValue;

	public String getOSDataPropertyhasValue() {
		return osDataPropertyhasValue;
	}

	public void setOSDataPropertyhasValue(String osDataPropertyhasValue) {
		this.osDataPropertyhasValue = osDataPropertyhasValue;
	}
	
	@Value("${skos.data.property.hasAltLabel}")
	private String osDataPropertyhasAltLabel;

	public String getOSDataPropertyhasAltLabel() {
		return osDataPropertyhasAltLabel;
	}

	public void setOSDataPropertyhasAltLabel(String osDataPropertyhasAltLabel) {
		this.osDataPropertyhasAltLabel = osDataPropertyhasAltLabel;
	}
	
	@Value("${ontokin.data.property.hasNumberOfElement}")
	private String osDataPropertyhasNumberOfElement;
	
	public String getOSDataPropertyhasNumberOfElement() {
		return osDataPropertyhasNumberOfElement;
	}
	
	public void setOSDataPropertyhasNumberOfElement(String osDataPropertyhasNumberOfElement) {
		this.osDataPropertyhasNumberOfElement = osDataPropertyhasNumberOfElement;
	}
	
	
	
	////////////////////////////////////////////////////////////////
	/////////////////////// OntoPrimeS class////////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${ontoprimespecies.class.chemicalSpecies}")
	private String classChemicalSpecies;

	public String getClassChemicalSpecies() {
		return classChemicalSpecies;
	}

	public void setClassChemicalSpecies(String classChemicalSpecies) {
		this.classChemicalSpecies = classChemicalSpecies;
	}

	@Value("${ontoprimespecies.class.copyright}")
	private String classCopyright;

	public String getClassCopyright() {
		return classCopyright;
	}

	public void setClassCopyright(String classCopyright) {
		this.classCopyright = classCopyright;
	}

	@Value("${ontoprimespecies.class.content}")
	private String classContent;

	public String getClassContent() {
		return classContent;
	}

	public void setClassContent(String classContent) {
		this.classContent = classContent;
	}

	@Value("${ontoprimespecies.class.bibliographyLink}")
	private String classBibliographyLink;

	public String getClassBibliographyLink() {
		return classBibliographyLink;
	}

	public void setClassBibliographyLink(String classBibliographyLink) {
		this.classBibliographyLink = classBibliographyLink;
	}

	@Value("${ontoprimespecies.class.preferredKey}")
	private String classPreferredKey;

	public String getClassPreferredKey() {
		return classPreferredKey;
	}

	public void setClassPreferredKey(String classPreferredKey) {
		this.classPreferredKey = classPreferredKey;
	}

	@Value("${ontoprimespecies.class.chemicalIdentifier}")
	private String classChemicalIdentifier;

	public String getClassChemicalIdentifier() {
		return classChemicalIdentifier;
	}

	public void setClassChemicalIdentifier(String classChemicalIdentifier) {
		this.classChemicalIdentifier = classChemicalIdentifier;
	}

	@Value("${ontoprimespecies.class.chemicalComposition}")
	private String classChemicalComposition;

	public String getClassChemicalComposition() {
		return classChemicalComposition;
	}

	public void setClassChemicalComposition(String classChemicalComposition) {
		this.classChemicalComposition = classChemicalComposition;
	}

	@Value("${ontoprimespecies.class.additionalDataItem}")
	private String classAdditionalDataItem;

	public String getClassAdditionalDataItem() {
		return classAdditionalDataItem;
	}

	public void setClassAdditionalDataItem(String classAdditionalDataItem) {
		this.classAdditionalDataItem = classAdditionalDataItem;
	}

	@Value("${ontoprimespecies.class.name}")
	private String className;

	public String getClassName() {
		return className;
	}

	public void setClassName(String className) {
		this.className = className;
	}

	@Value("${ontoprimespecies.class.atom}")
	private String classAtom;

	public String getClassAtom() {
		return classAtom;
	}

	public void setClassAtom(String classAtom) {
		this.classAtom = classAtom;
	}

	@Value("${ontoprimespecies.class.component}")
	private String classComponent;

	public String getClassComponent() {
		return classComponent;
	}

	public void setClassComponent(String classComponent) {
		this.classComponent = classComponent;
	}

	@Value("${ontoprimespecies.class.coal}")
	private String classCoal;

	public String getClassCoal() {
		return classCoal;
	}

	public void setClassCoal(String classCoal) {
		this.classCoal = classCoal;
	}

	@Value("${ontoprimespecies.class.speciesLink}")
	private String classSpeciesLink;

	public String getClassSpeciesLink() {
		return classSpeciesLink;
	}

	public void setClassSpeciesLink(String classSpeciesLink) {
		this.classSpeciesLink = classSpeciesLink;
	}

	@Value("${ontoprimespecies.class.amount}")
	private String classAmount;

	public String getClassAmount() {
		return classAmount;
	}

	public void setClassAmount(String classAmount) {
		this.classAmount = classAmount;
	}

	@Value("${ontoprimespecies.class.uncertainty}")
	private String classUncertainty;

	public String getClassUncertainty() {
		return classUncertainty;
	}

	public void setClassUncertainty(String classUncertainty) {
		this.classUncertainty = classUncertainty;
	}

	////////////////////////////////////////////////////////////////
	////////////////// OntoPrimeS object property///////////////////
	////////////////////////////////////////////////////////////////
	@Value("${ontoprimespecies.object.property.hasCopyright}")
	private String objPropertyhasCopyright;

	public String getObjPropertyhasCopyright() {
		return objPropertyhasCopyright;
	}

	public void setObjPropertyhasCopyright(String objPropertyhasCopyright) {
		this.objPropertyhasCopyright = objPropertyhasCopyright;
	}

	@Value("${ontoprimespecies.object.property.hasContent}")
	private String objPropertyhasContent;

	public String getObjPropertyhasContent() {
		return objPropertyhasContent;
	}

	public void setObjPropertyhasContent(String objPropertyhasContent) {
		this.objPropertyhasContent = objPropertyhasContent;
	}

	@Value("${ontoprimespecies.object.property.hasBibliographyLink}")
	private String objPropertyhasBibliographyLink;

	public String getObjPropertyhasBibliographyLink() {
		return objPropertyhasBibliographyLink;
	}

	public void setObjPropertyhasBibliographyLink(String objPropertyhasBibliographyLink) {
		this.objPropertyhasBibliographyLink = objPropertyhasBibliographyLink;
	}

	@Value("${ontoprimespecies.object.property.hasPreferredKey}")
	private String objPropertyhasPreferredKey;

	public String getObjPropertyhasPreferredKey() {
		return objPropertyhasPreferredKey;
	}

	public void setObjPropertyhasPreferredKey(String objPropertyhasPreferredKey) {
		this.objPropertyhasPreferredKey = objPropertyhasPreferredKey;
	}

	@Value("${ontoprimespecies.object.property.hasChemicalIdentifier}")
	private String objPropertyhasChemicalIdentifier;

	public String getObjPropertyhasChemicalIdentifier() {
		return objPropertyhasChemicalIdentifier;
	}

	public void setObjPropertyhasChemicalIdentifier(String objPropertyhasChemicalIdentifier) {
		this.objPropertyhasChemicalIdentifier = objPropertyhasChemicalIdentifier;
	}

	@Value("${ontoprimespecies.object.property.hasChemicalComposition}")
	private String objPropertyhasChemicalComposition;

	public String getObjPropertyhasChemicalComposition() {
		return objPropertyhasChemicalComposition;
	}

	public void setObjPropertyhasChemicalComposition(String objPropertyhasChemicalComposition) {
		this.objPropertyhasChemicalComposition = objPropertyhasChemicalComposition;
	}

	@Value("${ontoprimespecies.object.property.hasAdditionalDataItem}")
	private String objPropertyhasAdditionalDataItem;

	public String getObjPropertyhasAdditionalDataItem() {
		return objPropertyhasAdditionalDataItem;
	}

	public void setObjPropertyhasAdditionalDataItem(String objPropertyhasAdditionalDataItem) {
		this.objPropertyhasAdditionalDataItem = objPropertyhasAdditionalDataItem;
	}

	@Value("${ontoprimespecies.object.property.hasName}")
	private String objPropertyhasName;

	public String getObjPropertyhasName() {
		return objPropertyhasName;
	}

	public void setObjPropertyhasName(String objPropertyhasName) {
		this.objPropertyhasName = objPropertyhasName;
	}

	@Value("${ontoprimespecies.object.property.hasAtom}")
	private String objPropertyhasAtom;

	public String getObjPropertyhasAtom() {
		return objPropertyhasAtom;
	}

	public void setObjPropertyhasAtom(String objPropertyhasAtom) {
		this.objPropertyhasAtom = objPropertyhasAtom;
	}

	@Value("${ontoprimespecies.object.property.hasComponent}")
	private String objPropertyhasComponent;

	public String getObjPropertyhasComponent() {
		return objPropertyhasComponent;
	}

	public void setObjPropertyhasComponent(String objPropertyhasComponent) {
		this.objPropertyhasComponent = objPropertyhasComponent;
	}

	@Value("${ontoprimespecies.object.property.hasCoal}")
	private String objPropertyhasCoal;

	public String getObjPropertyhasCoal() {
		return objPropertyhasCoal;
	}

	public void setObjPropertyhasCoal(String objPropertyhasCoal) {
		this.objPropertyhasCoal = objPropertyhasCoal;
	}

	@Value("${ontoprimespecies.object.property.hasSpeciesLink}")
	private String objPropertyhasSpeciesLink;

	public String getObjPropertyhasSpeciesLink() {
		return objPropertyhasSpeciesLink;
	}

	public void setObjPropertyhasSpeciesLink(String objPropertyhasSpeciesLink) {
		this.objPropertyhasSpeciesLink = objPropertyhasSpeciesLink;
	}

	@Value("${ontoprimespecies.object.property.hasAmount}")
	private String objPropertyhasAmount;

	public String getObjPropertyhasAmount() {
		return objPropertyhasAmount;
	}

	public void setObjPropertyhasAmount(String objPropertyhasAmount) {
		this.objPropertyhasAmount = objPropertyhasAmount;
	}

	@Value("${ontoprimespecies.object.property.hasUncertainty}")
	private String objPropertyhasUncertainty;

	public String getObjPropertyhasUncertainty() {
		return objPropertyhasUncertainty;
	}

	public void setObjPropertyhasUncertainty(String objPropertyhasUncertainty) {
		this.objPropertyhasUncertainty = objPropertyhasUncertainty;
	}

	////////////////////////////////////////////////////////////////
	/////////////////// OntoPrimeS data property////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${ontoprimespecies.data.property.hasPrimeID}")
	private String dataPropertyhasPrimeID;

	public String getDataPropertyhasPrimeID() {
		return dataPropertyhasPrimeID;
	}

	public void setDataPropertyhasPrimeID(String dataPropertyhasPrimeID) {
		this.dataPropertyhasPrimeID = dataPropertyhasPrimeID;
	}

	@Value("${ontoprimespecies.data.property.hasSource}")
	private String dataPropertyhasSource;

	public String getDataPropertyhasSource() {
		return dataPropertyhasSource;
	}

	public void setDataPropertyhasSource(String dataPropertyhasSource) {
		this.dataPropertyhasSource = dataPropertyhasSource;
	}

	@Value("${ontoprimespecies.data.property.hasCopyrighted}")
	private String dataPropertyhasCopyrighted;

	public String getDataPropertyhasCopyrighted() {
		return dataPropertyhasCopyrighted;
	}

	public void setDataPropertyhasCopyrighted(String dataPropertyhasCopyrighted) {
		this.dataPropertyhasCopyrighted = dataPropertyhasCopyrighted;
	}

	@Value("${ontoprimespecies.data.property.hasBibliography}")
	private String dataPropertyhasBibliography;

	public String getDataPropertyhasBibliography() {
		return dataPropertyhasBibliography;
	}

	public void setDataPropertyhasBibliography(String dataPropertyhasBibliography) {
		this.dataPropertyhasBibliography = dataPropertyhasBibliography;
	}

	@Value("${ontoprimespecies.data.property.hasPreferredKey}")
	private String dataPropertyhasPreferredKey;

	public String getDataPropertyhasPreferredKey() {
		return dataPropertyhasPreferredKey;
	}

	public void setDataPropertyhasPreferredKey(String dataPropertyhasPreferredKey) {
		this.dataPropertyhasPreferredKey = dataPropertyhasPreferredKey;
	}

	@Value("${ontoprimespecies.data.property.hasType}")
	private String dataPropertyhasType;

	public String getDataPropertyhasType() {
		return dataPropertyhasType;
	}

	public void setDataPropertyhasType(String dataPropertyhasType) {
		this.dataPropertyhasType = dataPropertyhasType;
	}

	@Value("${ontoprimespecies.data.property.hasGroup}")
	private String dataPropertyhasGroup;

	public String getDataPropertyhasGroup() {
		return dataPropertyhasGroup;
	}

	public void setDataPropertyhasGroup(String dataPropertyhasGroup) {
		this.dataPropertyhasGroup = dataPropertyhasGroup;
	}

	@Value("${ontoprimespecies.data.property.hasDescriptor}")
	private String dataPropertyhasDescriptor;

	public String getDataPropertyhasDescriptor() {
		return dataPropertyhasDescriptor;
	}

	public void setDataPropertyhasDescriptor(String dataPropertyhasDescriptor) {
		this.dataPropertyhasDescriptor = dataPropertyhasDescriptor;
	}

	@Value("${ontoprimespecies.data.property.hasSymbol}")
	private String dataPropertyhasSymbol;

	public String getDataPropertyhasSymbol() {
		return dataPropertyhasSymbol;
	}

	public void setDataPropertyhasSymbol(String dataPropertyhasSymbol) {
		this.dataPropertyhasSymbol = dataPropertyhasSymbol;
	}

	@Value("${ontoprimespecies.data.property.hasIsotope}")
	private String dataPropertyhasIsotope;

	public String getDataPropertyhasIsotope() {
		return dataPropertyhasIsotope;
	}

	public void setDataPropertyhasIsotope(String dataPropertyhasIsotope) {
		this.dataPropertyhasIsotope = dataPropertyhasIsotope;
	}

	@Value("${ontoprimespecies.data.property.hasUnits}")
	private String dataPropertyhasUnits;

	public String getDataPropertyhasUnits() {
		return dataPropertyhasUnits;
	}

	public void setDataPropertyhasUnits(String dataPropertyhasUnits) {
		this.dataPropertyhasUnits = dataPropertyhasUnits;
	}

	@Value("${ontoprimespecies.data.property.hasBound}")
	private String dataPropertyhasBound;

	public String getDataPropertyhasBound() {
		return dataPropertyhasBound;
	}

	public void setDataPropertyhasBound(String dataPropertyhasBound) {
		this.dataPropertyhasBound = dataPropertyhasBound;
	}

	@Value("${ontoprimespecies.data.property.hasKind}")
	private String dataPropertyhasKind;

	public String getDataPropertyhasKind() {
		return dataPropertyhasKind;
	}

	public void setDataPropertyhasKind(String dataPropertyhasKind) {
		this.dataPropertyhasKind = dataPropertyhasKind;
	}

	@Value("${ontoprimespecies.data.property.hasTransformation}")
	private String dataPropertyhasTransformation;

	public String getDataPropertyhasTransformation() {
		return dataPropertyhasTransformation;
	}

	public void setDataPropertyhasTransformation(String dataPropertyhasTransformation) {
		this.dataPropertyhasTransformation = dataPropertyhasTransformation;
	}

	@Value("${ontoprimespecies.data.property.hasSpecifiedBy}")
	private String dataPropertyhasSpecifiedBy;

	public String getDataPropertyhasSpecifiedBy() {
		return dataPropertyhasSpecifiedBy;
	}

	public void setDataPropertyhasSpecifiedBy(String dataPropertyhasSpecifiedBy) {
		this.dataPropertyhasSpecifiedBy = dataPropertyhasSpecifiedBy;
	}

	@Value("${ontoprimespecies.data.property.hasItemType}")
	private String dataPropertyhasItemType;

	public String getDataPropertyhasItemType() {
		return dataPropertyhasItemType;
	}

	public void setDataPropertyhasItemType(String dataPropertyhasItemType) {
		this.dataPropertyhasItemType = dataPropertyhasItemType;
	}

	@Value("${ontoprimespecies.data.property.hasDescription}")
	private String dataPropertyhasDescription;

	public String getDataPropertyhasDescription() {
		return dataPropertyhasDescription;
	}

	public void setDataPropertyhasDescription(String dataPropertyhasDescription) {
		this.dataPropertyhasDescription = dataPropertyhasDescription;
	}

	@Value("${ontoprimespecies.data.property.hasMIME}")
	private String dataPropertyhasMIME;

	public String getDataPropertyhasMIME() {
		return dataPropertyhasMIME;
	}

	public void setDataPropertyhasMIME(String dataPropertyhasMIME) {
		this.dataPropertyhasMIME = dataPropertyhasMIME;
	}
	
	@Value("${ontoprimespecies.data.property.hasXmlns}")
	private String dataPropertyhasXmlns;

	public String getDataPropertyhasXmlns() {
		return dataPropertyhasXmlns;
	}

	public void setDataPropertyhasXmlns(String dataPropertyhasXmlns) {
		this.dataPropertyhasXmlns = dataPropertyhasXmlns;
	}

	@Value("${ontoprimespecies.data.property.hasXmlnsXsi}")
	private String dataPropertyhasXmlnsXsi;

	public String getDataPropertyhasXmlnsXsi() {
		return dataPropertyhasXmlnsXsi;
	}

	public void setDataPropertyhasXmlnsXsi(String dataPropertyhasXmlnsXsi) {
		this.dataPropertyhasXmlnsXsi = dataPropertyhasXmlnsXsi;
	}

	@Value("${ontoprimespecies.data.property.hasXsiSchemaLocation}")
	private String dataPropertyhasXsiSchemaLocation;

	public String getDataPropertyhasXsiSchemaLocation() {
		return dataPropertyhasXsiSchemaLocation;
	}

	public void setDataPropertyhasXsiSchemaLocation(String dataPropertyhasXsiSchemaLocation) {
		this.dataPropertyhasXsiSchemaLocation = dataPropertyhasXsiSchemaLocation;
	}

	@Value("${ontoprimespecies.data.property.hasVal}")
	private String dataPropertyhasVal;

	public String getDataPropertyhasVal() {
		return dataPropertyhasVal;
	}

	public void setDataPropertyhasVal(String dataPropertyhasVal) {
		this.dataPropertyhasVal = dataPropertyhasVal;
	}

}
