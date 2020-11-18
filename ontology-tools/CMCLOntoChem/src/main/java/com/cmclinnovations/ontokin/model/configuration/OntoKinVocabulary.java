package com.cmclinnovations.ontokin.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all OntoKin class, property names and name spaces provided
 * in the ontokin.properties file.</br>
 * 
 * This will empower users to use OntoKin, if some
 * of its classes, properties or name spaces change at a later stage,
 * without changing the source code of OntoKin.</br>
 * 
 * @author msff2
 *
 */
@Configuration
@PropertySource("classpath:ontokin.vocabulary.properties")
public class OntoKinVocabulary {

	/**
	 * A name space property representing the URL
	 * of the Eccenca vocabulary
	 */
	@Value("${ontokin.eccenca}")
	private String eccenca;

	@Value("${ontokin.representation.language}")
	private String representationLanguage;
	
	@Value("${ontokin.ctml}")
	private String ctml;
	
	@Value("${ontokin.ctml.version}")
	private String ctmlVersion;

	@Value("${ontokin.ctml.commit}")
	private String ctmlCommit;
	
	@Value("${ontokin.ctml.source.comment}")
	private String sourceComment;
	
	@Value("${ontokin.ctml.comment}")
	private String ctmlComment;
	
	@Value("${ontokin.ctml.validate.reactions}")
	private String ctmlValidateReactions;

	@Value("${ontokin.ctml.validate.species}")
	private String ctmlValidateSpecies;
	
	@Value("${ontokin.extracted.from}")
	private String extractedFrom;

	@Value("${ontokin.mechanism}")
	private String ontokinMechanism;
	
	@Value("${ontokin.phase}")
	private String ontokinPhase;
	
	@Value("${ontokin.phase.exist.in}")
	private String objectPropertyExistsIn;
	
	@Value("${ontokin.material}")
	private String classMaterial;
			
	@Value("${ontokin.element}")
	private String ontokinElement;
	
	@Value("${ontokin.phase.has.element.property}")
	private String objectPropertyHasElement;

	@Value("${ontokin.phase.has.atom.property}")
	private String objectPropertyHasAtom;
			
	@Value("${ontokin.ctml.element.atomicWt.units}")
	private String dataPropertyAtomicWtUnits;
	
	@Value("${ontokin.geosparql}")
	private String geoSparqlNS;
	
	@Value("${ontokin.ctml.phase.dimension}")
	private String phaseDimension;
	
	@Value("${ontokin.dbpedia.ontology}")
	private String geoDBPediaOntNS;

	@Value("${ontokin.ctml.phase.material}")
	private String phaseMaterial;
	
	@Value("${ontokin.ctml.phase.element.array}")
	private String phaseChemEArray;
	
	@Value("${ontokin.eepsa}")
	private String eepsaNS;
	
	@Value("${ontokin.ctml.element.data.source}")
	private String elementDataSource;
	
	@Value("${ontokin.ctml.phase.species.array}")
	private String phaseSpeciesArray;
	
	@Value("${ontokin.ctml.species.data.source}")
	private String speciesDataSource;
	
	@Value("${ontokin.ctml.phase.reaction.array}")
	private String phaseReactionArray;

	@Value("${ontokin.ctml.reaction.data.source}")
	private String reactionDataSource;
	
	@Value("${ontokin.ctml.phase.state}")
	private String phaseState;
	
	@Value("${ontokin.ctml.phase.thermo.model}")
	private String thermoModel;
	
	@Value("${ontokin.ctml.phase.thermo.site_density}")
	private String siteDensity;

	@Value("${ontokin.ctml.phase.thermo.site_density.units}")
	private String siteDensityUnits;
	
	@Value("${ontokin.ctml.phase.kinetics.model}")
	private String kineticsModel;

	@Value("${ontokin.ctml.phase.transport.model}")
	private String transportModel;

	@Value("${ontokin.ctml.phase.phase.array}")
	private String phaseArray;

	@Value("${ontokin.ctml.element.data.id}")
	private String elementDataId;
	
	@Value("${ontokin.ctml.element.data.casesensitivity}")
	private String elementDataCaseSensitivity;
	
	@Value("${ontokin.ctml.element.name}")
	private String elementName;
	
	@Value("${ontokin.ctml.element.atomicWt}")
	private String elementAtomicWt;
	
	@Value("${ontokin.element.metadata}")
	private String elementMetadata;

	@Value("${ontokin.element.metadata.property}")
	private String elementMetadataProperty;

	@Value("${ontokin.ctml.species.data.casesensitivity}")
	private String speciesDataCaseSensitivity;

	@Value("${ontokin.species.metadata}")
	private String speciesMetadata;

	@Value("${ontokin.species.metadata.property}")
	private String speciesMetadataProperty;

	@Value("${ontokin.reaction.metadata.property}")
	private String reactionMetadataProperty;
	
	@Value("${ontokin.ctml.species.name}")
	private String speciesName;

	@Value("${ontokin.species.has.atom.array.specification}")
	private String objectPropertyElementSpecification;
	
	@Value("${ontokin.reaction.has.product.specification}")
	private String objectPropertyProductSpecification;

	@Value("${ontokin.reaction.has.reactant.specification}")
	private String objectPropertyReactantSpecification;
			
	@Value("${ontokin.species}")
	private String classSpecies;

	
	@Value("${ontokin.species.atom.array.specification}")
	private String classElementSpecification;
	
	@Value("${ontokin.reaction.product.specification}")
	private String classProductSpecification;

	@Value("${ontokin.reaction.reactant.specification}")
	private String classReactantSpecification;

	@Value("${ontokin.ctml.species.phase}")
	private String ontokinSpeciesPhase;
	
	@Value("${ontokin.species.belongs.to}")
	private String ontokinSpeciesBelongsTo;

	@Value("${ontokin.reaction.belongs.to.phase}")
	private String ontokinBelongsToPhase;

	@Value("${ontokin.reaction.belongs.to.material}")
	private String ontokinBelongsToMaterial;
	
	@Value("${ontokin.ctml.species.note}")
	private String ontokinNote;

	@Value("${ontokin.ctml.species.thermo.comment}")
	private String ontokinThermoComment;

	@Value("${ontokin.ctml.species.atom.array}")
	private String ontokinAtomArray;

	@Value("${ontokin.ctml.species.has.number.of.element}")
	private String dataPropertyNumberOfElement;

	@Value("${ontokin.ctml.species.has.stoichiometric.coefficient}")
	private String dataPropertyStoichiometricCoefficient;
			
	@Value("${ontokin.ctml.species.size}")
	private String ontokinSpeciesSize;

	@Value("${ontokin.ctml.species.size.units}")
	private String ontokinSpeciesSizeUnits;

	@Value("${ontokin.ctml.species.density}")
	private String ontokinSpeciesDensity;

	@Value("${ontokin.ctml.species.density.units}")
	private String ontokinSpeciesDensityUnits;
	
	@Value("${ontokin.thermo.model.coefficient.nasa}")
	private String ontokinNasaPolyCoefficient;
	
	@Value("${ontokin.ctml.species.thermo.nasa.tmax}")
	private String ontokinNASACoefficientTmax;

	@Value("${ontokin.ctml.species.thermo.nasa.tmin}")
	private String ontokinNASACoefficientTmin;

	@Value("${ontokin.ctml.species.thermo.nasa.p0}")
	private String ontokinNASACoefficientP0;

	@Value("${ontokin.ctml.species.thermo.nasa.floatarray.name}")
	private String ontokinNASACoefficientArrayName;

	@Value("${ontokin.ctml.has.number.of.coefficients}")
	private String ontokinHasNumberOfCoefficients;
	
	@Value("${ontokin.ctml.species.has.nasa.polynomial.coefficient.value}")
	private String dataPropertyNasaPolynomialCoeffValue;
	
	@Value("${ontokin.ctml.species.transport.model}")
	private String ontokinTransportModel;

	@Value("${ontokin.ctml.species.transport.comment}")
	private String ontokinTransportComment;

	@Value("${ontokin.ctml.species.transport.string}")
	private String ontokinTransportString;

	@Value("${ontokin.ctml.species.transport.string.title}")
	private String ontokinTransportStringTitle;

	@Value("${ontokin.ctml.species.transport.LJWellDepth}")
	private String ontokinTransportLJWellDepth;

	@Value("${ontokin.ctml.species.transport.LJWellDepth.units}")
	private String ontokinTransportLJWellDepthUnits;

	@Value("${ontokin.ctml.species.transport.LJDiameter}")
	private String ontokinTransportLJDiameter;

	@Value("${ontokin.ctml.species.transport.LJDiameter.units}")
	private String ontokinTransportLJDiameterUnits;

	@Value("${ontokin.ctml.species.transport.dipoleMoment}")
	private String ontokinTransportDipoleMoment;

	@Value("${ontokin.ctml.species.transport.dipoleMoment.units}")
	private String ontokinTransportDipoleMomentUnits;

	@Value("${ontokin.ctml.species.transport.polarizability}")
	private String ontokinTransportPolarizability;

	@Value("${ontokin.ctml.species.transport.polarizability.units}")
	private String ontokinTransportPolarizabilityUnits;

	@Value("${ontokin.ctml.species.transport.rotRelax}")
	private String ontokinTransportRotRelax;
	
	@Value("${ontokin.ctml.species.transport.rotRelax.units}")
	private String ontokinTransportRotRelaxUnits;
	
	@Value("${ontokin.species.has.transport.parameter}")
	private String ontokinHasTransportParameter;

	@Value("${ontokin.transport.parameter}")
	private String ontokinTransportParameter;
	
	@Value("${ontokin.ctml.reaction.data.casesensitivity}")
	private String reactionDataCaseSensitivity;

	@Value("${ontokin.reaction.metadata}")
	private String reactionMetadata;
	
	@Value("${ontokin.reaction}")
	private String classReaction;

	@Value("${ontokin.reaction.rate.sticking.coeff}")
	private String classStickingCoefficient;
	
	@Value("${ontokin.phase.gas}")
	private String classGasPhase;

	@Value("${ontokin.phase.site}")
	private String classSitePhase;

	@Value("${ontokin.phase.bulk}")
	private String classBulkPhase;
	
	@Value("${ontokin.ctml.reaction.duplicate}")
	private String reactionDuplicate;
			
	@Value("${ontokin.ctml.reaction.reversible}")
	private String reactionReverisble;
	
	@Value("${ontokin.ctml.reaction.landauTeller}")
	private String reactionLandauTeller;
	
	@Value("${ontokin.ctml.reaction.noncon}")
	private String reactionMayNotConserveSite;
	
	@Value("${ontokin.ctml.reaction.partialpressure}")
	private String convertToPartialPressure;

	@Value("${ontokin.reaction.type.surface}")
	private String classSurfaceReaction;

	@Value("${ontokin.reaction.type.threebody}")
	private String classThreeBodyReaction;

	@Value("${ontokin.reaction.type.falloff}")
	private String classFallOffReaction;
	
	@Value("${ontokin.reaction.type.falloff.subtype.troe}")
	private String classTroeReaction;

	@Value("${ontokin.reaction.type.falloff.subtype.plog}")
	private String classPLOGReaction;

	@Value("${ontokin.reaction.type.falloff.subtype.lindemann}")
	private String classLindemannReaction;

	@Value("${ontokin.reaction.type.falloff.subtype.sri}")
	private String classSRIReaction;

	@Value("${ontokin.reaction.type.falloff.subtype.cheb}")
	private String classCHEBReaction;
	
	@Value("${ontokin.ctml.reaction.sitefrac}")
	private String convertToSiteFraction;

	@Value("${ontokin.ctml.reaction.equation}")
	private String ontoKinEquation;
	
	@Value("${ontokin.ctml.reaction.reactant}")
	private String ontoKinReactant;

	@Value("${ontokin.ctml.reaction.product}")
	private String ontoKinProduct;
	
	@Value("${ontokin.reaction.order}")
	private String ReactionOrder;

	@Value("${ontokin.reaction.has.reaction.order}")
	private String objectPropertyReactionOrder;

	@Value("${ontokin.ctml.reaction.order.direction}")
	private String dataPropertyOrderDirection;

	@Value("${ontokin.ctml.reaction.order.species}")
	private String objectPropertyOrderSpecies;
	
	@Value("${ontokin.reaction.rate.coeff.cheb}")
	private String classCHEBCoefficient;
	
	@Value("${ontokin.reaction.rate.coeff.arrhenius}")
	private String classArrheniusCoefficient;
	
	@Value("${ontokin.reaction.rate.coeff.landauteller}")
	private String classLandauTellerCoefficient;
	
	@Value("${ontokin.reaction.rate.coeff.coverage.dependency}")
	private String classCoverageCoefficient;
	
	@Value("${ontokin.reaction.rate.coeff.falloff.model}")
	private String classFallOffModelCoefficient;
	
	@Value("${ontokin.reaction.has.arrhenius.coeff}")
	private String objectPropertyArrheniusRateCoeff;

	@Value("${ontokin.reaction.has.sticking.coeff}")
	private String objectPropertyStickingCoeff;
	
	@Value("${ontokin.reaction.has.landauteller.coeff}")
	private String objectPropertyLandauTellerRateCoeff;

	@Value("${ontokin.reaction.has.coverage.dependency.coeff}")
	private String objectPropertyCoverageCoefficient;
	
	@Value("${ontokin.reaction.has.falloff.model}")
	private String objectPropertyFallOffModelCoeff;
	
	@Value("${ontokin.reaction.has.chebyshev.coeff}")
	private String objectPropertyCHEBRateCoeff;
	
	@Value("${ontokin.ctml.reaction.rate.coeff.tmax}")
	private String dataPropertyTempMax;

	@Value("${ontokin.ctml.reaction.rate.coeff.tmax.units}")
	private String dataPropertyTempMaxUnits;

	@Value("${ontokin.ctml.reaction.rate.coeff.tmin}")
	private String dataPropertyTempMin;

	@Value("${ontokin.ctml.reaction.rate.coeff.tmin.units}")
	private String dataPropertyTempMinUnits;

	@Value("${ontokin.ctml.reaction.rate.coeff.pmax}")
	private String dataPropertyPressureMax;

	@Value("${ontokin.ctml.reaction.rate.coeff.pmax.units}")
	private String dataPropertyPressureMaxUnits;

	@Value("${ontokin.ctml.reaction.rate.coeff.pmin}")
	private String dataPropertyPressureMin;

	@Value("${ontokin.ctml.reaction.rate.coeff.pmin.units}")
	private String dataPropertyPressureMinUnits;
	
	@Value("${ontokin.ctml.species.has.chebyshev.rate.coefficient.value}")
	private String dataPropertyChebyshebRateCoeffsValue;
	
	@Value("${ontokin.ctml.reaction.chebyshev.rate.coeff.floatarray.units}")
	private String dataPropertyChebyshebRateCoeffsUnits;
	
	@Value("${ontokin.ctml.reaction.chebyshev.rate.coeff.floatarray.degreeT}")
	private String dataPropertyChebyshebRateCoeffsTempPoints;

	@Value("${ontokin.ctml.reaction.chebyshev.rate.coeff.floatarray.degreeP}")
	private String dataPropertyChebyshebRateCoeffsPressurePoints;
	
	@Value("${ontokin.ctml.reaction.arrhenius.rate.coeff.motz.wise}")
	private String dataPropertyHasMotzWiseCorrection;
	
	@Value("${ontokin.reaction.rate.coeff.correction.factor}")
	private String classMotzWise;
	
	@Value("${ontokin.has.species}")
	private String objectPropertyHasSpecies;

	@Value("${ontokin.has.reactant}")
	private String objectPropertyHasReactant;

	@Value("${ontokin.has.product}")
	private String objectPropertyHasProduct;
	
	@Value("${ontokin.ctml.reaction.arrhenius.rate.coeff.A}")
	private String dataPropertyHasArrCoeffA;

	@Value("${ontokin.ctml.reaction.arrhenius.rate.coeff.A.units}")
	private String dataPropertyHasArrCoeffAUnits;

	@Value("${ontokin.ctml.reaction.arrhenius.rate.coeff.b}")
	private String dataPropertyHasArrCoeffb;

	@Value("${ontokin.ctml.reaction.arrhenius.rate.coeff.b.units}")
	private String dataPropertyHasArrCoeffbUnits;

	@Value("${ontokin.ctml.reaction.arrhenius.rate.coeff.E}")
	private String dataPropertyHasArrCoeffE;

	@Value("${ontokin.ctml.reaction.arrhenius.rate.coeff.E.units}")
	private String dataPropertyHasArrCoeffEUnits;

	@Value("${ontokin.ctml.reaction.arrhenius.rate.coeff.P}")
	private String dataPropertyHasArrRefPressure;

	@Value("${ontokin.ctml.reaction.arrhenius.rate.coeff.P.units}")
	private String dataPropertyHasArrRefPressureUnits;

	@Value("${ontokin.ctml.reaction.coverage.modifier.a}")
	private String dataPropertyHasCovDepA;

	@Value("${ontokin.ctml.reaction.coverage.modifier.a.units}")
	private String dataPropertyHasCovDepAUnits;

	@Value("${ontokin.ctml.reaction.coverage.modifier.m}")
	private String dataPropertyHasCovDepM;

	@Value("${ontokin.ctml.reaction.coverage.modifier.m.units}")
	private String dataPropertyHasCovDepMUnits;

	@Value("${ontokin.ctml.reaction.coverage.modifier.e}")
	private String dataPropertyHasCovDepE;

	@Value("${ontokin.ctml.reaction.coverage.modifier.e.units}")
	private String dataPropertyHasCovDepEUnits;

	@Value("${ontokin.ctml.reaction.landauteller.rate.coeff.B}")
	private String dataPropertyHasLanTellerCoeffB;

	@Value("${ontokin.ctml.reaction.landauteller.rate.coeff.B.units}")
	private String dataPropertyHasLanTellerCoeffBUnits;

	@Value("${ontokin.ctml.reaction.landauteller.rate.coeff.C}")
	private String dataPropertyHasLanTellerCoeffC;

	@Value("${ontokin.ctml.reaction.landauteller.rate.coeff.C.units}")
	private String dataPropertyHasLanTellerCoeffCUnits;
	
	@Value("${ontokin.reaction.rate.coeff.third.body.efficiency}")
	private String classThirdBodyEfficiency;
	
	@Value("${ontokin.reaction.has.third.body.efficiency}")
	private String objectPropertyThirdBodyEfficiency;

	@Value("${ontokin.ctml.reaction.has.efficiency.value}")
	private String dataPropertyHasEfficiencyValue;
	
	@Value("${ontokin.has.reaction.efficiency.of.species}")
	private String hasEfficiencyOfSpecies;
	
	@Value("${ontokin.ctml.reaction.has.default.thirdbody.efficiency}")
	private String hasDefaultThirdBodyEfficiency;
	
	@Value("${ontokin.reaction.falloff.has.named.third.body}")
	private String hasNamedThirdBody;

	@Value("${ontokin.ctml.reaction.has.falloff.type}")
	private String hasFallOffType;

	@Value("${ontokin.thermo.model}")
	private String classThermoModel;
	
	@Value("${ontokin.species.has.thermo.model}")
	private String objectPropertyHasThermoModel;

	@Value("${ontokin.ctml.species.has.coefficient.values}")
	private String dataPropertyHasCoeffValues;
			
	public String getEccenca() {
		return eccenca;
	}

	public void setEccenca(String eccenca) {
		this.eccenca = eccenca;
	}

	public String getRepresentationLanguage() {
		return representationLanguage;
	}

	public void setRepresentationLanguage(String representationLanguage) {
		this.representationLanguage = representationLanguage;
	}

	public String getCtml() {
		return ctml;
	}

	public void setCtml(String ctml) {
		this.ctml = ctml;
	}

	public String getCtmlVersion() {
		return ctmlVersion;
	}

	public void setCtmlVersion(String ctmlVersion) {
		this.ctmlVersion = ctmlVersion;
	}

	public String getCtmlCommit() {
		return ctmlCommit;
	}

	public void setCtmlCommit(String ctmlCommit) {
		this.ctmlCommit = ctmlCommit;
	}

	public String getCtmlComment() {
		return ctmlComment;
	}

	public void setCtmlComment(String ctmlComment) {
		this.ctmlComment = ctmlComment;
	}
	
	public String getSourceComment() {
		return sourceComment;
	}

	public void setSourceComment(String sourceComment) {
		this.sourceComment = sourceComment;
	}

	public String getCtmlValidateReactions() {
		return ctmlValidateReactions;
	}

	public void setCtmlValidateReactions(String ctmlValidateReactions) {
		this.ctmlValidateReactions = ctmlValidateReactions;
	}

	public String getCtmlValidateSpecies() {
		return ctmlValidateSpecies;
	}

	public void setCtmlValidateSpecies(String ctmlValidateSpecies) {
		this.ctmlValidateSpecies = ctmlValidateSpecies;
	}

	public String getExtractedFrom() {
		return extractedFrom;
	}

	public void setExtractedFrom(String extractedFrom) {
		this.extractedFrom = extractedFrom;
	}

	public String getOntokinMechanism() {
		return ontokinMechanism;
	}

	public void setOntokinMechanism(String ontokinMechanism) {
		this.ontokinMechanism = ontokinMechanism;
	}

	public String getOntokinPhase() {
		return ontokinPhase;
	}

	public void setOntokinPhase(String ontokinPhase) {
		this.ontokinPhase = ontokinPhase;
	}

	public String getGeoSparqlNS() {
		return geoSparqlNS;
	}

	public void setGeoSparqlNS(String geoSparqlNS) {
		this.geoSparqlNS = geoSparqlNS;
	}

	public String getPhaseDimension() {
		return phaseDimension;
	}

	public void setPhaseDimension(String phaseDimension) {
		this.phaseDimension = phaseDimension;
	}

	public String getGeoDBPediaOntNS() {
		return geoDBPediaOntNS;
	}

	public void setGeoDBPediaOntNS(String geoDBPediaOntNS) {
		this.geoDBPediaOntNS = geoDBPediaOntNS;
	}

	public String getPhaseMaterial() {
		return phaseMaterial;
	}

	public void setPhaseMaterial(String phaseMaterial) {
		this.phaseMaterial = phaseMaterial;
	}

	public String getPhaseChemEArray() {
		return phaseChemEArray;
	}
	
	/**
	 * Sets the value of a phase chemical element array
	 * 
	 * @param phaseChemEArray it represents phase chemical element array
	 */
	public void setPhaseChemEArray(String phaseChemEArray) {
		this.phaseChemEArray = phaseChemEArray;
	}

	public String getEepsaNS() {
		return eepsaNS;
	}

	public void setEepsaNS(String eepsaNS) {
		this.eepsaNS = eepsaNS;
	}

	public String getElementDataSource() {
		return elementDataSource;
	}

	public void setElementDataSource(String elementDataSource) {
		this.elementDataSource = elementDataSource;
	}

	public String getPhaseSpeciesArray() {
		return phaseSpeciesArray;
	}

	public void setPhaseSpeciesArray(String phaseSpeciesArray) {
		this.phaseSpeciesArray = phaseSpeciesArray;
	}

	public String getSpeciesDataSource() {
		return speciesDataSource;
	}

	public void setSpeciesDataSource(String speciesDataSource) {
		this.speciesDataSource = speciesDataSource;
	}

	public String getPhaseReactionArray() {
		return phaseReactionArray;
	}

	public void setPhaseReactionArray(String phaseReactionArray) {
		this.phaseReactionArray = phaseReactionArray;
	}

	public String getReactionDataSource() {
		return reactionDataSource;
	}

	public void setReactionDataSource(String reactionDataSource) {
		this.reactionDataSource = reactionDataSource;
	}

	public String getPhaseState() {
		return phaseState;
	}

	public void setPhaseState(String phaseState) {
		this.phaseState = phaseState;
	}

	public String getThermoModel() {
		return thermoModel;
	}

	public void setThermoModel(String thermoModel) {
		this.thermoModel = thermoModel;
	}

	public String getSiteDensity() {
		return siteDensity;
	}

	public void setSiteDensity(String siteDensity) {
		this.siteDensity = siteDensity;
	}

	public String getSiteDensityUnits() {
		return siteDensityUnits;
	}

	public void setSiteDensityUnits(String siteDensityUnits) {
		this.siteDensityUnits = siteDensityUnits;
	}

	public String getKineticsModel() {
		return kineticsModel;
	}

	public void setKineticsModel(String kineticsModel) {
		this.kineticsModel = kineticsModel;
	}

	public String getTransportModel() {
		return transportModel;
	}

	public void setTransportModel(String transportModel) {
		this.transportModel = transportModel;
	}

	public String getPhaseArray() {
		return phaseArray;
	}

	public void setPhaseArray(String phaseArray) {
		this.phaseArray = phaseArray;
	}

	public String getElementDataId() {
		return elementDataId;
	}

	public void setElementDataId(String elementDataId) {
		this.elementDataId = elementDataId;
	}

	public String getElementDataCaseSensitivity() {
		return elementDataCaseSensitivity;
	}

	public void setElementDataCaseSensitivity(String elementDataCaseSensitivity) {
		this.elementDataCaseSensitivity = elementDataCaseSensitivity;
	}

	public String getElementName() {
		return elementName;
	}

	public void setElementName(String elementName) {
		this.elementName = elementName;
	}

	public String getElementAtomicWt() {
		return elementAtomicWt;
	}

	public void setElementAtomicWt(String elementAtomicWt) {
		this.elementAtomicWt = elementAtomicWt;
	}

	public String getOntokinElement() {
		return ontokinElement;
	}
	
	public void setOntokinElement(String ontokinElement) {
		this.ontokinElement = ontokinElement;
	}

	

	public String getElementMetadata() {
		return elementMetadata;
	}

	public void setElementMetadata(String elementMetadata) {
		this.elementMetadata = elementMetadata;
	}

	public String getElementMetadataProperty() {
		return elementMetadataProperty;
	}

	public void setElementMetadataProperty(String elementMetadataProperty) {
		this.elementMetadataProperty = elementMetadataProperty;
	}

	public String getSpeciesDataCaseSensitivity() {
		return speciesDataCaseSensitivity;
	}

	public void setSpeciesDataCaseSensitivity(String speciesDataCaseSensitivity) {
		this.speciesDataCaseSensitivity = speciesDataCaseSensitivity;
	}

	public String getSpeciesMetadata() {
		return speciesMetadata;
	}

	public void setSpeciesMetadata(String speciesMetadata) {
		this.speciesMetadata = speciesMetadata;
	}

	public String getSpeciesMetadataProperty() {
		return speciesMetadataProperty;
	}

	public void setSpeciesMetadataProperty(String speciesMetadataProperty) {
		this.speciesMetadataProperty = speciesMetadataProperty;
	}

	public String getSpeciesName() {
		return speciesName;
	}

	public void setSpeciesName(String speciesName) {
		this.speciesName = speciesName;
	}

	public String getClassSpecies() {
		return classSpecies;
	}

	public void setClassSpecies(String classSpecies) {
		this.classSpecies = classSpecies;
	}

	public String getOntokinSpeciesPhase() {
		return ontokinSpeciesPhase;
	}

	public void setOntokinSpeciesPhase(String ontokinSpeciesPhase) {
		this.ontokinSpeciesPhase = ontokinSpeciesPhase;
	}

	public String getOntokinSpeciesBelongsTo() {
		return ontokinSpeciesBelongsTo;
	}

	public void setOntokinSpeciesBelongsTo(String ontokinSpeciesBelongsTo) {
		this.ontokinSpeciesBelongsTo = ontokinSpeciesBelongsTo;
	}

	public String getOntokinNote() {
		return ontokinNote;
	}

	public void setOntokinNote(String ontokinNote) {
		this.ontokinNote = ontokinNote;
	}

	public String getOntokinThermoComment() {
		return ontokinThermoComment;
	}

	public void setOntokinThermoComment(String ontokinThermoComment) {
		this.ontokinThermoComment = ontokinThermoComment;
	}

	public String getOntokinAtomArray() {
		return ontokinAtomArray;
	}

	public void setOntokinAtomArray(String ontokinAtomArray) {
		this.ontokinAtomArray = ontokinAtomArray;
	}

	public String getOntokinSpeciesSize() {
		return ontokinSpeciesSize;
	}

	public void setOntokinSpeciesSize(String ontokinSpeciesSize) {
		this.ontokinSpeciesSize = ontokinSpeciesSize;
	}

	public String getOntokinSpeciesSizeUnits() {
		return ontokinSpeciesSizeUnits;
	}

	public void setOntokinSpeciesSizeUnits(String ontokinSpeciesSizeUnits) {
		this.ontokinSpeciesSizeUnits = ontokinSpeciesSizeUnits;
	}

	public String getOntokinSpeciesDensity() {
		return ontokinSpeciesDensity;
	}

	public void setOntokinSpeciesDensity(String ontokinSpeciesDensity) {
		this.ontokinSpeciesDensity = ontokinSpeciesDensity;
	}

	public String getOntokinSpeciesDensityUnits() {
		return ontokinSpeciesDensityUnits;
	}

	public void setOntokinSpeciesDensityUnits(String ontokinSpeciesDensityUnits) {
		this.ontokinSpeciesDensityUnits = ontokinSpeciesDensityUnits;
	}

	public String getOntokinNasaPolyCoefficient() {
		return ontokinNasaPolyCoefficient;
	}

	public void setOntokinNasaPolyCoefficient(String ontokinNasaPolyCoefficient) {
		this.ontokinNasaPolyCoefficient = ontokinNasaPolyCoefficient;
	}

	public String getOntokinNASACoefficientTmax() {
		return ontokinNASACoefficientTmax;
	}

	public void setOntokinNASACoefficientTmax(String ontokinNASACoefficientTmax) {
		this.ontokinNASACoefficientTmax = ontokinNASACoefficientTmax;
	}

	public String getOntokinNASACoefficientTmin() {
		return ontokinNASACoefficientTmin;
	}

	public void setOntokinNASACoefficientTmin(String ontokinNASACoefficientTmin) {
		this.ontokinNASACoefficientTmin = ontokinNASACoefficientTmin;
	}

	public String getOntokinNASACoefficientP0() {
		return ontokinNASACoefficientP0;
	}

	public void setOntokinNASACoefficientP0(String ontokinNASACoefficientP0) {
		this.ontokinNASACoefficientP0 = ontokinNASACoefficientP0;
	}

	public String getOntokinNASACoefficientArrayName() {
		return ontokinNASACoefficientArrayName;
	}

	public void setOntokinNASACoefficientArrayName(String ontokinNASACoefficientArrayName) {
		this.ontokinNASACoefficientArrayName = ontokinNASACoefficientArrayName;
	}

	public String getOntokinHasNumberOfCoefficients() {
		return ontokinHasNumberOfCoefficients;
	}

	public void setOntokinHasNumberOfCoefficients(String ontokinHasNumberOfCoefficients) {
		this.ontokinHasNumberOfCoefficients = ontokinHasNumberOfCoefficients;
	}

	public String getOntokinTransportModel() {
		return ontokinTransportModel;
	}

	public void setOntokinTransportModel(String ontokinTransportModel) {
		this.ontokinTransportModel = ontokinTransportModel;
	}

	public String getOntokinTransportComment() {
		return ontokinTransportComment;
	}

	public void setOntokinTransportComment(String ontokinTransportComment) {
		this.ontokinTransportComment = ontokinTransportComment;
	}

	public String getOntokinTransportString() {
		return ontokinTransportString;
	}

	public void setOntokinTransportString(String ontokinTransportString) {
		this.ontokinTransportString = ontokinTransportString;
	}

	public String getOntokinTransportStringTitle() {
		return ontokinTransportStringTitle;
	}

	public void setOntokinTransportStringTitle(String ontokinTransportStringTitle) {
		this.ontokinTransportStringTitle = ontokinTransportStringTitle;
	}

	public String getOntokinTransportLJWellDepth() {
		return ontokinTransportLJWellDepth;
	}

	public void setOntokinTransportLJWellDepth(String ontokinTransportLJWellDepth) {
		this.ontokinTransportLJWellDepth = ontokinTransportLJWellDepth;
	}

	public String getOntokinTransportLJWellDepthUnits() {
		return ontokinTransportLJWellDepthUnits;
	}

	public void setOntokinTransportLJWellDepthUnits(String ontokinTransportLJWellDepthUnits) {
		this.ontokinTransportLJWellDepthUnits = ontokinTransportLJWellDepthUnits;
	}

	public String getOntokinTransportLJDiameter() {
		return ontokinTransportLJDiameter;
	}

	public void setOntokinTransportLJDiameter(String ontokinTransportLJDiameter) {
		this.ontokinTransportLJDiameter = ontokinTransportLJDiameter;
	}

	public String getOntokinTransportLJDiameterUnits() {
		return ontokinTransportLJDiameterUnits;
	}

	public void setOntokinTransportLJDiameterUnits(String ontokinTransportLJDiameterUnits) {
		this.ontokinTransportLJDiameterUnits = ontokinTransportLJDiameterUnits;
	}

	public String getOntokinTransportDipoleMoment() {
		return ontokinTransportDipoleMoment;
	}

	public void setOntokinTransportDipoleMoment(String ontokinTransportDipoleMoment) {
		this.ontokinTransportDipoleMoment = ontokinTransportDipoleMoment;
	}

	public String getOntokinTransportDipoleMomentUnits() {
		return ontokinTransportDipoleMomentUnits;
	}

	public void setOntokinTransportDipoleMomentUnits(String ontokinTransportDipoleMomentUnits) {
		this.ontokinTransportDipoleMomentUnits = ontokinTransportDipoleMomentUnits;
	}

	public String getOntokinTransportPolarizability() {
		return ontokinTransportPolarizability;
	}

	public void setOntokinTransportPolarizability(String ontokinTransportPolarizability) {
		this.ontokinTransportPolarizability = ontokinTransportPolarizability;
	}

	public String getOntokinTransportPolarizabilityUnits() {
		return ontokinTransportPolarizabilityUnits;
	}

	public void setOntokinTransportPolarizabilityUnits(String ontokinTransportPolarizabilityUnits) {
		this.ontokinTransportPolarizabilityUnits = ontokinTransportPolarizabilityUnits;
	}

	public String getOntokinTransportRotRelax() {
		return ontokinTransportRotRelax;
	}

	public void setOntokinTransportRotRelax(String ontokinTransportRotRelax) {
		this.ontokinTransportRotRelax = ontokinTransportRotRelax;
	}

	public String getOntokinTransportRotRelaxUnits() {
		return ontokinTransportRotRelaxUnits;
	}

	public void setOntokinTransportRotRelaxUnits(String ontokinTransportRotRelaxUnits) {
		this.ontokinTransportRotRelaxUnits = ontokinTransportRotRelaxUnits;
	}

	public String getOntokinHasTransportParameter() {
		return ontokinHasTransportParameter;
	}

	public void setOntokinHasTransportParameter(String ontokinHasTransportParameter) {
		this.ontokinHasTransportParameter = ontokinHasTransportParameter;
	}

	public String getOntokinTransportParameter() {
		return ontokinTransportParameter;
	}

	public void setOntokinTransportParameter(String ontokinTransportParameter) {
		this.ontokinTransportParameter = ontokinTransportParameter;
	}

	public String getReactionDataCaseSensitivity() {
		return reactionDataCaseSensitivity;
	}

	public void setReactionDataCaseSensitivity(String reactionDataCaseSensitivity) {
		this.reactionDataCaseSensitivity = reactionDataCaseSensitivity;
	}

	public String getReactionMetadata() {
		return reactionMetadata;
	}

	public void setReactionMetadata(String reactionMetadata) {
		this.reactionMetadata = reactionMetadata;
	}
	
	public String getReactionDuplicate() {
		return reactionDuplicate;
	}

	public void setReactionDuplicate(String reactionDuplicate) {
		this.reactionDuplicate = reactionDuplicate;
	}

	public String getReactionReverisble() {
		return reactionReverisble;
	}

	public void setReactionReverisble(String reactionReverisble) {
		this.reactionReverisble = reactionReverisble;
	}

	public String getClassReaction() {
		return classReaction;
	}

	public void setClassReaction(String classReaction) {
		this.classReaction = classReaction;
	}

	public String getReactionLandauTeller() {
		return reactionLandauTeller;
	}

	public void setReactionLandauTeller(String reactionLandauTeller) {
		this.reactionLandauTeller = reactionLandauTeller;
	}

	public String getReactionMayNotConserveSite() {
		return reactionMayNotConserveSite;
	}

	public void setReactionMayNotConserveSite(String reactionMayNotConserveSite) {
		this.reactionMayNotConserveSite = reactionMayNotConserveSite;
	}

	public String getClassSurfaceReaction() {
		return classSurfaceReaction;
	}

	public void setClassSurfaceReaction(String classSurfaceReaction) {
		this.classSurfaceReaction = classSurfaceReaction;
	}

	public String getClassThreeBodyReaction() {
		return classThreeBodyReaction;
	}

	public void setClassThreeBodyReaction(String classThreeBodyReaction) {
		this.classThreeBodyReaction = classThreeBodyReaction;
	}

	public String getClassFallOffReaction() {
		return classFallOffReaction;
	}

	public void setClassFallOffReaction(String classFallOffReaction) {
		this.classFallOffReaction = classFallOffReaction;
	}

	public String getClassTroeReaction() {
		return classTroeReaction;
	}

	public void setClassTroeReaction(String classTroeReaction) {
		this.classTroeReaction = classTroeReaction;
	}

	public String getClassPLOGReaction() {
		return classPLOGReaction;
	}

	public void setClassPLOGReaction(String classPLOGReaction) {
		this.classPLOGReaction = classPLOGReaction;
	}

	public String getClassLindemannReaction() {
		return classLindemannReaction;
	}

	public void setClassLindemannReaction(String classLindemannReaction) {
		this.classLindemannReaction = classLindemannReaction;
	}

	public String getClassSRIReaction() {
		return classSRIReaction;
	}

	public void setClassSRIReaction(String classSRIReaction) {
		this.classSRIReaction = classSRIReaction;
	}

	public String getClassCHEBReaction() {
		return classCHEBReaction;
	}

	public void setClassCHEBReaction(String classCHEBReaction) {
		this.classCHEBReaction = classCHEBReaction;
	}

	public String getConvertToPartialPressure() {
		return convertToPartialPressure;
	}

	public void setConvertToPartialPressure(String convertToPartialPressure) {
		this.convertToPartialPressure = convertToPartialPressure;
	}

	public String getConvertToSiteFraction() {
		return convertToSiteFraction;
	}

	public void setConvertToSiteFraction(String convertToSiteFraction) {
		this.convertToSiteFraction = convertToSiteFraction;
	}

	public String getOntoKinEquation() {
		return ontoKinEquation;
	}

	public void setOntoKinEquation(String ontoKinEquation) {
		this.ontoKinEquation = ontoKinEquation;
	}

	public String getOntoKinReactant() {
		return ontoKinReactant;
	}

	public void setOntoKinReactant(String ontoKinReactant) {
		this.ontoKinReactant = ontoKinReactant;
	}

	public String getOntoKinProduct() {
		return ontoKinProduct;
	}

	public void setOntoKinProduct(String ontoKinProduct) {
		this.ontoKinProduct = ontoKinProduct;
	}

	public String getReactionMetadataProperty() {
		return reactionMetadataProperty;
	}

	public void setReactionMetadataProperty(String reactionMetadataProperty) {
		this.reactionMetadataProperty = reactionMetadataProperty;
	}

	public String getReactionOrder() {
		return ReactionOrder;
	}

	public void setReactionOrder(String reactionOrder) {
		ReactionOrder = reactionOrder;
	}

	public String getObjectPropertyReactionOrder() {
		return objectPropertyReactionOrder;
	}

	public void setObjectPropertyReactionOrder(String objectPropertyReactionOrder) {
		this.objectPropertyReactionOrder = objectPropertyReactionOrder;
	}

	public String getDataPropertyOrderDirection() {
		return dataPropertyOrderDirection;
	}

	public void setDataPropertyOrderDirection(String dataPropertyOrderDirection) {
		this.dataPropertyOrderDirection = dataPropertyOrderDirection;
	}

	public String getObjectPropertyOrderSpecies() {
		return objectPropertyOrderSpecies;
	}

	public void setObjectPropertyOrderSpecies(String objectPropertyOrderSpecies) {
		this.objectPropertyOrderSpecies = objectPropertyOrderSpecies;
	}

	public String getClassCHEBCoefficient() {
		return classCHEBCoefficient;
	}

	public void setClassCHEBCoefficient(String classCHEBCoefficient) {
		this.classCHEBCoefficient = classCHEBCoefficient;
	}

	public String getObjectPropertyCHEBRateCoeff() {
		return objectPropertyCHEBRateCoeff;
	}

	public void setObjectPropertyCHEBRateCoeff(String objectPropertyCHEBRateCoeff) {
		this.objectPropertyCHEBRateCoeff = objectPropertyCHEBRateCoeff;
	}

	public String getDataPropertyTempMax() {
		return dataPropertyTempMax;
	}

	public void setDataPropertyTempMax(String dataPropertyTempMax) {
		this.dataPropertyTempMax = dataPropertyTempMax;
	}

	public String getDataPropertyTempMaxUnits() {
		return dataPropertyTempMaxUnits;
	}

	public void setDataPropertyTempMaxUnits(String dataPropertyTempMaxUnits) {
		this.dataPropertyTempMaxUnits = dataPropertyTempMaxUnits;
	}

	public String getDataPropertyTempMin() {
		return dataPropertyTempMin;
	}

	public void setDataPropertyTempMin(String dataPropertyTempMin) {
		this.dataPropertyTempMin = dataPropertyTempMin;
	}

	public String getDataPropertyTempMinUnits() {
		return dataPropertyTempMinUnits;
	}

	public void setDataPropertyTempMinUnits(String dataPropertyTempMinUnits) {
		this.dataPropertyTempMinUnits = dataPropertyTempMinUnits;
	}

	public String getDataPropertyPressureMax() {
		return dataPropertyPressureMax;
	}

	public void setDataPropertyPressureMax(String dataPropertyPressureMax) {
		this.dataPropertyPressureMax = dataPropertyPressureMax;
	}

	public String getDataPropertyPressureMaxUnits() {
		return dataPropertyPressureMaxUnits;
	}

	public void setDataPropertyPressureMaxUnits(String dataPropertyPressureMaxUnits) {
		this.dataPropertyPressureMaxUnits = dataPropertyPressureMaxUnits;
	}

	public String getDataPropertyPressureMin() {
		return dataPropertyPressureMin;
	}

	public void setDataPropertyPressureMin(String dataPropertyPressureMin) {
		this.dataPropertyPressureMin = dataPropertyPressureMin;
	}

	public String getDataPropertyPressureMinUnits() {
		return dataPropertyPressureMinUnits;
	}

	public void setDataPropertyPressureMinUnits(String dataPropertyPressureMinUnits) {
		this.dataPropertyPressureMinUnits = dataPropertyPressureMinUnits;
	}

	public String getDataPropertyNasaPolynomialCoeffValue() {
		return dataPropertyNasaPolynomialCoeffValue;
	}

	public void setDataPropertyNasaPolynomialCoeffValue(String dataPropertyNasaPolynomialCoeffValue) {
		this.dataPropertyNasaPolynomialCoeffValue = dataPropertyNasaPolynomialCoeffValue;
	}

	public String getDataPropertyChebyshebRateCoeffsValue() {
		return dataPropertyChebyshebRateCoeffsValue;
	}

	public void setDataPropertyChebyshebRateCoeffsValue(String dataPropertyChebyshebRateCoeffsValue) {
		this.dataPropertyChebyshebRateCoeffsValue = dataPropertyChebyshebRateCoeffsValue;
	}

	public String getDataPropertyChebyshebRateCoeffsUnits() {
		return dataPropertyChebyshebRateCoeffsUnits;
	}

	public void setDataPropertyChebyshebRateCoeffsUnits(String dataPropertyChebyshebRateCoeffsUnits) {
		this.dataPropertyChebyshebRateCoeffsUnits = dataPropertyChebyshebRateCoeffsUnits;
	}

	public String getDataPropertyChebyshebRateCoeffsTempPoints() {
		return dataPropertyChebyshebRateCoeffsTempPoints;
	}

	public void setDataPropertyChebyshebRateCoeffsTempPoints(String dataPropertyChebyshebRateCoeffsTempPoints) {
		this.dataPropertyChebyshebRateCoeffsTempPoints = dataPropertyChebyshebRateCoeffsTempPoints;
	}

	public String getDataPropertyChebyshebRateCoeffsPressurePoints() {
		return dataPropertyChebyshebRateCoeffsPressurePoints;
	}

	public void setDataPropertyChebyshebRateCoeffsPressurePoints(String dataPropertyChebyshebRateCoeffsPressurePoints) {
		this.dataPropertyChebyshebRateCoeffsPressurePoints = dataPropertyChebyshebRateCoeffsPressurePoints;
	}

	public String getClassArrheniusCoefficient() {
		return classArrheniusCoefficient;
	}

	public void setClassArrheniusCoefficient(String classArrheniusCoefficient) {
		this.classArrheniusCoefficient = classArrheniusCoefficient;
	}

	public String getClassLandauTellerCoefficient() {
		return classLandauTellerCoefficient;
	}

	public void setClassLandauTellerCoefficient(String classLandauTellerCoefficient) {
		this.classLandauTellerCoefficient = classLandauTellerCoefficient;
	}

	public String getClassCoverageCoefficient() {
		return classCoverageCoefficient;
	}

	public void setClassCoverageCoefficient(String classCoverageCoefficient) {
		this.classCoverageCoefficient = classCoverageCoefficient;
	}

	public String getClassFallOffModelCoefficient() {
		return classFallOffModelCoefficient;
	}

	public void setClassFallOffModelCoefficient(String classFallOffModelCoefficient) {
		this.classFallOffModelCoefficient = classFallOffModelCoefficient;
	}

	public String getObjectPropertyArrheniusRateCoeff() {
		return objectPropertyArrheniusRateCoeff;
	}

	public void setObjectPropertyArrheniusRateCoeff(String objectPropertyArrheniusRateCoeff) {
		this.objectPropertyArrheniusRateCoeff = objectPropertyArrheniusRateCoeff;
	}

	public String getObjectPropertyLandauTellerRateCoeff() {
		return objectPropertyLandauTellerRateCoeff;
	}

	public void setObjectPropertyLandauTellerRateCoeff(String objectPropertyLandauTellerRateCoeff) {
		this.objectPropertyLandauTellerRateCoeff = objectPropertyLandauTellerRateCoeff;
	}

	public String getObjectPropertyCoverageCoefficient() {
		return objectPropertyCoverageCoefficient;
	}

	public void setObjectPropertyCoverageCoefficient(String objectPropertyCoverageCoefficient) {
		this.objectPropertyCoverageCoefficient = objectPropertyCoverageCoefficient;
	}

	public String getObjectPropertyFallOffModelCoeff() {
		return objectPropertyFallOffModelCoeff;
	}

	public void setObjectPropertyFallOffModelCoeff(String objectPropertyFallOffModelCoeff) {
		this.objectPropertyFallOffModelCoeff = objectPropertyFallOffModelCoeff;
	}

	public String getDataPropertyHasMotzWiseCorrection() {
		return dataPropertyHasMotzWiseCorrection;
	}

	public void setDataPropertyHasMotzWiseCorrection(String dataPropertyHasMotzWiseCorrection) {
		this.dataPropertyHasMotzWiseCorrection = dataPropertyHasMotzWiseCorrection;
	}

	public String getClassMotzWise() {
		return classMotzWise;
	}

	public void setClassMotzWise(String classMotzWise) {
		this.classMotzWise = classMotzWise;
	}

	public String getObjectPropertyHasSpecies() {
		return objectPropertyHasSpecies;
	}

	public void setObjectPropertyHasSpecies(String objectPropertyHasSpecies) {
		this.objectPropertyHasSpecies = objectPropertyHasSpecies;
	}

	public String getObjectPropertyHasReactant() {
		return objectPropertyHasReactant;
	}

	public void setObjectPropertyHasReactant(String objectPropertyHasReactant) {
		this.objectPropertyHasReactant = objectPropertyHasReactant;
	}

	public String getObjectPropertyHasProduct() {
		return objectPropertyHasProduct;
	}

	public void setObjectPropertyHasProduct(String objectPropertyHasProduct) {
		this.objectPropertyHasProduct = objectPropertyHasProduct;
	}

	public String getDataPropertyHasArrCoeffA() {
		return dataPropertyHasArrCoeffA;
	}

	public void setDataPropertyHasArrCoeffA(String dataPropertyHasArrCoeffA) {
		this.dataPropertyHasArrCoeffA = dataPropertyHasArrCoeffA;
	}

	public String getDataPropertyHasArrCoeffAUnits() {
		return dataPropertyHasArrCoeffAUnits;
	}

	public void setDataPropertyHasArrCoeffAUnits(String dataPropertyHasArrCoeffAUnits) {
		this.dataPropertyHasArrCoeffAUnits = dataPropertyHasArrCoeffAUnits;
	}

	public String getDataPropertyHasArrCoeffb() {
		return dataPropertyHasArrCoeffb;
	}

	public void setDataPropertyHasArrCoeffb(String dataPropertyHasArrCoeffb) {
		this.dataPropertyHasArrCoeffb = dataPropertyHasArrCoeffb;
	}

	public String getDataPropertyHasArrCoeffbUnits() {
		return dataPropertyHasArrCoeffbUnits;
	}

	public void setDataPropertyHasArrCoeffbUnits(String dataPropertyHasArrCoeffbUnits) {
		this.dataPropertyHasArrCoeffbUnits = dataPropertyHasArrCoeffbUnits;
	}

	public String getDataPropertyHasArrCoeffE() {
		return dataPropertyHasArrCoeffE;
	}

	public void setDataPropertyHasArrCoeffE(String dataPropertyHasArrCoeffE) {
		this.dataPropertyHasArrCoeffE = dataPropertyHasArrCoeffE;
	}

	public String getDataPropertyHasArrCoeffEUnits() {
		return dataPropertyHasArrCoeffEUnits;
	}

	public void setDataPropertyHasArrCoeffEUnits(String dataPropertyHasArrCoeffEUnits) {
		this.dataPropertyHasArrCoeffEUnits = dataPropertyHasArrCoeffEUnits;
	}

	public String getDataPropertyHasArrRefPressure() {
		return dataPropertyHasArrRefPressure;
	}

	public void setDataPropertyHasArrRefPressure(String dataPropertyHasArrRefPressure) {
		this.dataPropertyHasArrRefPressure = dataPropertyHasArrRefPressure;
	}

	public String getDataPropertyHasArrRefPressureUnits() {
		return dataPropertyHasArrRefPressureUnits;
	}

	public void setDataPropertyHasArrRefPressureUnits(String dataPropertyHasArrRefPressureUnits) {
		this.dataPropertyHasArrRefPressureUnits = dataPropertyHasArrRefPressureUnits;
	}

	public String getDataPropertyHasCovDepA() {
		return dataPropertyHasCovDepA;
	}

	public void setDataPropertyHasCovDepA(String dataPropertyHasCovDepA) {
		this.dataPropertyHasCovDepA = dataPropertyHasCovDepA;
	}

	public String getDataPropertyHasCovDepAUnits() {
		return dataPropertyHasCovDepAUnits;
	}

	public void setDataPropertyHasCovDepAUnits(String dataPropertyHasCovDepAUnits) {
		this.dataPropertyHasCovDepAUnits = dataPropertyHasCovDepAUnits;
	}

	public String getDataPropertyHasCovDepM() {
		return dataPropertyHasCovDepM;
	}

	public void setDataPropertyHasCovDepM(String dataPropertyHasCovDepM) {
		this.dataPropertyHasCovDepM = dataPropertyHasCovDepM;
	}

	public String getDataPropertyHasCovDepMUnits() {
		return dataPropertyHasCovDepMUnits;
	}

	public void setDataPropertyHasCovDepMUnits(String dataPropertyHasCovDepMUnits) {
		this.dataPropertyHasCovDepMUnits = dataPropertyHasCovDepMUnits;
	}

	public String getDataPropertyHasCovDepE() {
		return dataPropertyHasCovDepE;
	}

	public void setDataPropertyHasCovDepE(String dataPropertyHasCovDepE) {
		this.dataPropertyHasCovDepE = dataPropertyHasCovDepE;
	}

	public String getDataPropertyHasCovDepEUnits() {
		return dataPropertyHasCovDepEUnits;
	}

	public void setDataPropertyHasCovDepEUnits(String dataPropertyHasCovDepEUnits) {
		this.dataPropertyHasCovDepEUnits = dataPropertyHasCovDepEUnits;
	}

	public String getDataPropertyHasLanTellerCoeffB() {
		return dataPropertyHasLanTellerCoeffB;
	}

	public void setDataPropertyHasLanTellerCoeffB(String dataPropertyHasLanTellerCoeffB) {
		this.dataPropertyHasLanTellerCoeffB = dataPropertyHasLanTellerCoeffB;
	}

	public String getDataPropertyHasLanTellerCoeffBUnits() {
		return dataPropertyHasLanTellerCoeffBUnits;
	}

	public void setDataPropertyHasLanTellerCoeffBUnits(String dataPropertyHasLanTellerCoeffBUnits) {
		this.dataPropertyHasLanTellerCoeffBUnits = dataPropertyHasLanTellerCoeffBUnits;
	}

	public String getDataPropertyHasLanTellerCoeffC() {
		return dataPropertyHasLanTellerCoeffC;
	}

	public void setDataPropertyHasLanTellerCoeffC(String dataPropertyHasLanTellerCoeffC) {
		this.dataPropertyHasLanTellerCoeffC = dataPropertyHasLanTellerCoeffC;
	}

	public String getDataPropertyHasLanTellerCoeffCUnits() {
		return dataPropertyHasLanTellerCoeffCUnits;
	}

	public void setDataPropertyHasLanTellerCoeffCUnits(String dataPropertyHasLanTellerCoeffCUnits) {
		this.dataPropertyHasLanTellerCoeffCUnits = dataPropertyHasLanTellerCoeffCUnits;
	}

	public String getClassThirdBodyEfficiency() {
		return classThirdBodyEfficiency;
	}

	public void setClassThirdBodyEfficiency(String classThirdBodyEfficiency) {
		this.classThirdBodyEfficiency = classThirdBodyEfficiency;
	}

	public String getObjectPropertyThirdBodyEfficiency() {
		return objectPropertyThirdBodyEfficiency;
	}

	public void setObjectPropertyThirdBodyEfficiency(String objectPropertyThirdBodyEfficiency) {
		this.objectPropertyThirdBodyEfficiency = objectPropertyThirdBodyEfficiency;
	}

	public String getDataPropertyHasEfficiencyValue() {
		return dataPropertyHasEfficiencyValue;
	}

	public void setDataPropertyHasEfficiencyValue(String dataPropertyHasEfficiencyValue) {
		this.dataPropertyHasEfficiencyValue = dataPropertyHasEfficiencyValue;
	}

	public String getHasEfficiencyOfSpecies() {
		return hasEfficiencyOfSpecies;
	}

	public void setHasEfficiencyOfSpecies(String hasEfficiencyOfSpecies) {
		this.hasEfficiencyOfSpecies = hasEfficiencyOfSpecies;
	}

	public String getHasDefaultThirdBodyEfficiency() {
		return hasDefaultThirdBodyEfficiency;
	}

	public void setHasDefaultThirdBodyEfficiency(String hasDefaultThirdBodyEfficiency) {
		this.hasDefaultThirdBodyEfficiency = hasDefaultThirdBodyEfficiency;
	}

	public String getHasNamedThirdBody() {
		return hasNamedThirdBody;
	}

	public void setHasNamedThirdBody(String hasNamedThirdBody) {
		this.hasNamedThirdBody = hasNamedThirdBody;
	}

	public String getHasFallOffType() {
		return hasFallOffType;
	}

	public void setHasFallOffType(String hasFallOffType) {
		this.hasFallOffType = hasFallOffType;
	}

	public String getClassGasPhase() {
		return classGasPhase;
	}

	public void setClassGasPhase(String classGasPhase) {
		this.classGasPhase = classGasPhase;
	}

	public String getClassSitePhase() {
		return classSitePhase;
	}

	public void setClassSitePhase(String classSitePhase) {
		this.classSitePhase = classSitePhase;
	}

	public String getClassBulkPhase() {
		return classBulkPhase;
	}

	public void setClassBulkPhase(String classBulkPhase) {
		this.classBulkPhase = classBulkPhase;
	}

	public String getObjectPropertyExistsIn() {
		return objectPropertyExistsIn;
	}

	public void setObjectPropertyExistsIn(String objectPropertyExistsIn) {
		this.objectPropertyExistsIn = objectPropertyExistsIn;
	}

	public String getClassMaterial() {
		return classMaterial;
	}

	public void setClassMaterial(String classMaterial) {
		this.classMaterial = classMaterial;
	}

	public String getClassThermoModel() {
		return classThermoModel;
	}

	public void setClassThermoModel(String classThermoModel) {
		this.classThermoModel = classThermoModel;
	}

	public String getObjectPropertyHasThermoModel() {
		return objectPropertyHasThermoModel;
	}

	public void setObjectPropertyHasThermoModel(String objectPropertyHasThermoModel) {
		this.objectPropertyHasThermoModel = objectPropertyHasThermoModel;
	}

	public String getDataPropertyHasCoeffValues() {
		return dataPropertyHasCoeffValues;
	}

	public void setDataPropertyHasCoeffValues(String dataPropertyHasCoeffValues) {
		this.dataPropertyHasCoeffValues = dataPropertyHasCoeffValues;
	}

	public String getOntokinBelongsToPhase() {
		return ontokinBelongsToPhase;
	}

	public void setOntokinBelongsToPhase(String ontokinBelongsToPhase) {
		this.ontokinBelongsToPhase = ontokinBelongsToPhase;
	}

	public String getOntokinBelongsToMaterial() {
		return ontokinBelongsToMaterial;
	}

	public void setOntokinBelongsToMaterial(String ontokinBelongsToMaterial) {
		this.ontokinBelongsToMaterial = ontokinBelongsToMaterial;
	}

	public String getObjectPropertyHasElement() {
		return objectPropertyHasElement;
	}

	public void setObjectPropertyHasElement(String objectPropertyHasElement) {
		this.objectPropertyHasElement = objectPropertyHasElement;
	}

	public String getDataPropertyAtomicWtUnits() {
		return dataPropertyAtomicWtUnits;
	}

	public void setDataPropertyAtomicWtUnits(String dataPropertyAtomicWtUnits) {
		this.dataPropertyAtomicWtUnits = dataPropertyAtomicWtUnits;
	}

	public String getDataPropertyNumberOfElement() {
		return dataPropertyNumberOfElement;
	}

	public void setDataPropertyNumberOfElement(String dataPropertyNumberOfElement) {
		this.dataPropertyNumberOfElement = dataPropertyNumberOfElement;
	}

	public String getObjectPropertyElementSpecification() {
		return objectPropertyElementSpecification;
	}

	public void setObjectPropertyElementSpecification(String objectPropertyElementSpecification) {
		this.objectPropertyElementSpecification = objectPropertyElementSpecification;
	}

	public String getClassElementSpecification() {
		return classElementSpecification;
	}

	public void setClassElementSpecification(String classElementSpecification) {
		this.classElementSpecification = classElementSpecification;
	}

	public String getObjectPropertyHasAtom() {
		return objectPropertyHasAtom;
	}

	public void setObjectPropertyHasAtom(String objectPropertyHasAtom) {
		this.objectPropertyHasAtom = objectPropertyHasAtom;
	}

	public String getObjectPropertyProductSpecification() {
		return objectPropertyProductSpecification;
	}

	public void setObjectPropertyProductSpecification(String objectPropertyProductSpecification) {
		this.objectPropertyProductSpecification = objectPropertyProductSpecification;
	}

	public String getClassProductSpecification() {
		return classProductSpecification;
	}

	public void setClassProductSpecification(String classProductSpecification) {
		this.classProductSpecification = classProductSpecification;
	}

	public String getDataPropertyStoichiometricCoefficient() {
		return dataPropertyStoichiometricCoefficient;
	}

	public void setDataPropertyStoichiometricCoefficient(String dataPropertyStoichiometricCoefficient) {
		this.dataPropertyStoichiometricCoefficient = dataPropertyStoichiometricCoefficient;
	}

	public String getObjectPropertyReactantSpecification() {
		return objectPropertyReactantSpecification;
	}

	public void setObjectPropertyReactantSpecification(String objectPropertyReactantSpecification) {
		this.objectPropertyReactantSpecification = objectPropertyReactantSpecification;
	}

	public String getClassReactantSpecification() {
		return classReactantSpecification;
	}

	public void setClassReactantSpecification(String classReactantSpecification) {
		this.classReactantSpecification = classReactantSpecification;
	}

	public String getObjectPropertyStickingCoeff() {
		return objectPropertyStickingCoeff;
	}

	public void setObjectPropertyStickingCoeff(String objectPropertyStickingCoeff) {
		this.objectPropertyStickingCoeff = objectPropertyStickingCoeff;
	}

	public String getClassStickingCoefficient() {
		return classStickingCoefficient;
	}

	public void setClassStickingCoefficient(String classStickingCoefficient) {
		this.classStickingCoefficient = classStickingCoefficient;
	}
}
