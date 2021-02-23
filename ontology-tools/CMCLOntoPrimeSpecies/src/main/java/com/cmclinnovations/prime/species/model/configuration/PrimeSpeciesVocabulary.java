package com.cmclinnovations.prime.species.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:primespecies.vocabulary.properties")
public class PrimeSpeciesVocabulary {
	////////////////////////////////////////////////////////////////
	//////////////////////// PrimeS element /////////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${primespecies.element.chemicalSpecies}")
	private String elemChemicalSpecies;

	public String getElemChemicalSpecies() {
		return elemChemicalSpecies;
	}

	public void setElemChemicalSpecies(String elemChemicalSpecies) {
		this.elemChemicalSpecies = elemChemicalSpecies;
	}

	@Value("${primespecies.element.copyright}")
	private String elemCopyright;

	public String getElemCopyright() {
		return elemCopyright;
	}

	public void setElemCopyright(String elemCopyright) {
		this.elemCopyright = elemCopyright;
	}

	@Value("${primespecies.element.content}")
	private String elemContent;

	public String getElemContent() {
		return elemContent;
	}

	public void setElemContent(String elemContent) {
		this.elemContent = elemContent;
	}

	@Value("${primespecies.element.bibliographyLink}")
	private String elemBibliographyLink;

	public String getElemBibliographyLink() {
		return elemBibliographyLink;
	}

	public void setElemBibliographyLink(String elemBibliographyLink) {
		this.elemBibliographyLink = elemBibliographyLink;
	}

	@Value("${primespecies.element.preferredKey}")
	private String elemPreferredKey;

	public String getElemPreferredKey() {
		return elemPreferredKey;
	}

	public void setElemPreferredKey(String elemPreferredKey) {
		this.elemPreferredKey = elemPreferredKey;
	}

	@Value("${primespecies.element.chemicalIdentifier}")
	private String elemChemicalIdentifier;

	public String getElemChemicalIdentifier() {
		return elemChemicalIdentifier;
	}

	public void setElemChemicalIdentifier(String elemChemicalIdentifier) {
		this.elemChemicalIdentifier = elemChemicalIdentifier;
	}

	@Value("${primespecies.element.chemicalComposition}")
	private String elemChemicalComposition;

	public String getElemChemicalComposition() {
		return elemChemicalComposition;
	}

	public void setElemChemicalComposition(String elemChemicalComposition) {
		this.elemChemicalComposition = elemChemicalComposition;
	}

	@Value("${primespecies.element.additionalDataItem}")
	private String elemAdditionalDataItem;

	public String getElemAdditionalDataItem() {
		return elemAdditionalDataItem;
	}

	public void setElemAdditionalDataItem(String elemAdditionalDataItem) {
		this.elemAdditionalDataItem = elemAdditionalDataItem;
	}

	@Value("${primespecies.element.name}")
	private String elemName;

	public String getElemName() {
		return elemName;
	}

	public void setElemName(String elemName) {
		this.elemName = elemName;
	}

	@Value("${primespecies.element.atom}")
	private String elemAtom;

	public String getElemAtom() {
		return elemAtom;
	}

	public void setElemAtom(String elemAtom) {
		this.elemAtom = elemAtom;
	}

	@Value("${primespecies.element.component}")
	private String elemComponent;

	public String getElemComponent() {
		return elemComponent;
	}

	public void setElemComponent(String elemComponent) {
		this.elemComponent = elemComponent;
	}

	@Value("${primespecies.element.coal}")
	private String elemCoal;

	public String getElemCoal() {
		return elemCoal;
	}

	public void setElemCoal(String elemCoal) {
		this.elemCoal = elemCoal;
	}

	@Value("${primespecies.element.speciesLink}")
	private String elemSpeciesLink;

	public String getElemSpeciesLink() {
		return elemSpeciesLink;
	}

	public void setElemSpeciesLink(String elemSpeciesLink) {
		this.elemSpeciesLink = elemSpeciesLink;
	}

	@Value("${primespecies.element.amount}")
	private String elemAmount;

	public String getElemAmount() {
		return elemAmount;
	}

	public void setElemAmount(String elemAmount) {
		this.elemAmount = elemAmount;
	}

	@Value("${primespecies.element.uncertainty}")
	private String elemUncertainty;

	public String getElemUncertainty() {
		return elemUncertainty;
	}

	public void setElemUncertainty(String elemUncertainty) {
		this.elemUncertainty = elemUncertainty;
	}

	////////////////////////////////////////////////////////////////
	/////////////////////// PrimeS attribute ////////////////////////
	////////////////////////////////////////////////////////////////
	@Value("${primespecies.attribute.hasPrimeID}")
	private String attribPrimeID;

	public String getAttribPrimeID() {
		return attribPrimeID;
	}

	public void setAttribPrimeID(String attribPrimeID) {
		this.attribPrimeID = attribPrimeID;
	}

	@Value("${primespecies.attribute.hasSource}")
	private String attribSource;

	public String getAttribSource() {
		return attribSource;
	}

	public void setAttribSource(String attribSource) {
		this.attribSource = attribSource;
	}

	@Value("${primespecies.attribute.hasCopyrighted}")
	private String attribCopyrighted;

	public String getAttribCopyrighted() {
		return attribCopyrighted;
	}

	public void setAttribCopyrighted(String attribCopyrighted) {
		this.attribCopyrighted = attribCopyrighted;
	}

	@Value("${primespecies.attribute.hasBibliography}")
	private String attribBibliography;

	public String getAttribBibliography() {
		return attribBibliography;
	}

	public void setAttribBibliography(String attribBibliography) {
		this.attribBibliography = attribBibliography;
	}
	
	@Value("${primespecies.attribute.hasPreferredKey}")
	private String attribPreferredKey;

	public String getAttribPreferredKey() {
		return attribPreferredKey;
	}

	public void setAttribPreferredKey(String attribPreferredKey) {
		this.attribPreferredKey = attribPreferredKey;
	}
		
	@Value("${primespecies.attribute.hasType}")
	private String attribType;

	public String getAttribType() {
		return attribType;
	}

	public void setAttribType(String attribType) {
		this.attribType = attribType;
	}

	@Value("${primespecies.attribute.hasGroup}")
	private String attribGroup;

	public String getAttribGroup() {
		return attribGroup;
	}

	public void setAttribGroup(String attribGroup) {
		this.attribGroup = attribGroup;
	}

	@Value("${primespecies.attribute.hasDescriptor}")
	private String attribDescriptor;

	public String getAttribDescriptor() {
		return attribDescriptor;
	}

	public void setAttribDescriptor(String attribDescriptor) {
		this.attribDescriptor = attribDescriptor;
	}

	@Value("${primespecies.attribute.hasSymbol}")
	private String attribSymbol;

	public String getAttribSymbol() {
		return attribSymbol;
	}

	public void setAttribSymbol(String attribSymbol) {
		this.attribSymbol = attribSymbol;
	}	

	@Value("${primespecies.attribute.hasIsotope}")
	private String attribIsotope;

	public String getAttribIsotope() {
		return attribIsotope;
	}

	public void setAttribIsotope(String attribIsotope) {
		this.attribIsotope = attribIsotope;
	}

	@Value("${primespecies.attribute.hasUnits}")
	private String attribUnits;

	public String getAttribUnits() {
		return attribUnits;
	}

	public void setAttribUnits(String attribUnits) {
		this.attribUnits = attribUnits;
	}

	@Value("${primespecies.attribute.hasBound}")
	private String attribBound;

	public String getAttribBound() {
		return attribBound;
	}

	public void setAttribBound(String attribBound) {
		this.attribBound = attribBound;
	}

	@Value("${primespecies.attribute.hasKind}")
	private String attribKind;

	public String getAttribKind() {
		return attribKind;
	}

	public void setAttribKind(String attribKind) {
		this.attribKind = attribKind;
	}

	@Value("${primespecies.attribute.hasTransformation}")
	private String attribTransformation;

	public String getAttribTransformation() {
		return attribTransformation;
	}

	public void setAttribTransformation(String attribTransformation) {
		this.attribTransformation = attribTransformation;
	}

	@Value("${primespecies.attribute.hasSpecifiedBy}")
	private String attribSpecifiedBy;

	public String getAttribSpecifiedBy() {
		return attribSpecifiedBy;
	}

	public void setAttribSpecifiedBy(String attribSpecifiedBy) {
		this.attribSpecifiedBy = attribSpecifiedBy;
	}

	@Value("${primespecies.attribute.hasItemType}")
	private String attribItemType;

	public String getAttribItemType() {
		return attribItemType;
	}

	public void setAttribItemType(String attribItemType) {
		this.attribItemType = attribItemType;
	}

	@Value("${primespecies.attribute.hasDescription}")
	private String attribDescription;

	public String getAttribDescription() {
		return attribDescription;
	}

	public void setAttribDescription(String attribDescription) {
		this.attribDescription = attribDescription;
	}

	@Value("${primespecies.attribute.hasMIME}")
	private String attribMIME;

	public String getAttribMIME() {
		return attribMIME;
	}

	public void setAttribMIME(String attribMIME) {
		this.attribMIME = attribMIME;
	}
	
	@Value("${primespecies.attribute.hasXmlns}")
	private String attribXmlns;
	
	public String getAttribXmlns() {
		return attribXmlns;
	}
	
	public void setAttribXmlns(String attribXmlns) {
		this.attribXmlns = attribXmlns;
	}
	
	@Value("${primespecies.attribute.hasXmlnsXsi}")
	private String attribXmlnsXsi;
	
	public String getAttribXmlnsXsi() {
		return attribXmlnsXsi;
	}
	
	public void setAttribXmlnsXsi(String attribXmlnsXsi) {
		this.attribXmlnsXsi = attribXmlnsXsi;
	}
	
	@Value("${primespecies.attribute.hasXsiSchemaLocation}")
	private String attribXsiSchemaLocation;
	
	public String getAttribXsiSchemaLocation() {
		return attribXsiSchemaLocation;
	}
	
	public void setAttribXsiSchemaLocation(String attribXsiSchemaLocation) {
		this.attribXsiSchemaLocation = attribXsiSchemaLocation;
	}
}
