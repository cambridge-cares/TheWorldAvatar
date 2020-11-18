package com.cmclinnovations.ontokin.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all CTML tags and properties provided
 * in the ctml.properties file.</br>
 * 
 * This will empower users to use CTML, if some
 * of its tags or properties change at a later stage,
 * without changing the source code of OntoKin.</br>
 * 
 * @author msff2
 *
 */
@Configuration
@PropertySource("classpath:ctml.vocabulary.properties")
public class CtmlVocabulary {

	@Value("${ctml}")
	private String ctml;

	@Value("${ctml.version}")
	private String ctmlVersion;

	@Value("${ctml.commit}")
	private String ctmlCommit;

	@Value("${ctml.comment}")
	private String ctmlComment;
	
	@Value("${ctml.material}")
	private String ctmlMaterial;
			
	@Value("${ctml.validate}")
	private String ctmlValidate;

	@Value("${ctml.validate.reactions}")
	private String ctmlValidateReactions;
	
	@Value("${ctml.validate.species}")
	private String ctmlValidateSpecies;
	
	@Value("${ctml.phase}")
	private String ctmlPhase;

	@Value("${ctml.phase.dimension}")
	private String ctmlPhaseDimension;

	@Value("${ctml.phase.id}")
	private String ctmlPhaseId;
	
	@Value("${ctml.phase.material}")
	private String ctmlPhaseMaterial;	

	@Value("${ctml.phase.elementArray}")
	private String ctmlPhaseElementArray;
	
	@Value("${ctml.phase.elementArray.dataSrc}")
	private String ctmlPhaseElmentArrayDataSrc;
	
	@Value("${ctml.phase.speciesArray}")
	private String ctmlPhaseSpeciesArray;
	
	@Value("${ctml.phase.speciesArray.dataSrc}")
	private String ctmlPhaseSpeciesArrayDataSrc;
	
	@Value("${ctml.phase.reactionArray}")
	private String ctmlPhaseReactionArray;

	@Value("${ctml.phase.reactionArray.dataSrc}")
	private String ctmlPhaseReactionArrayDataSrc;
	
	@Value("${ctml.phase.state}")
	private String ctmlPhaseState;
	
	@Value("${ctml.phase.thermo}")
	private String ctmlPhaseThermo;
	
	@Value("${ctml.phase.thermo.model}")
	private String ctmlPhaseThermoModel;
	
	@Value("${ctml.phase.thermo.site_density}")
	private String ctmlPhaseSiteDensity;
	
	@Value("${ctml.phase.thermo.site_density.units}")
	private String ctmlPhaseSiteDensityUnits;
	
	@Value("${ctml.phase.kinetics}")
	private String ctmlPhaseKinetics;
	
	@Value("${ctml.phase.kinetics.model}")
	private String ctmlPhaseKineticsModel;
	
	@Value("${ctml.phase.transport}")
	private String ctmlPhaseTransport;
	
	@Value("${ctml.phase.transport.model}")
	private String ctmlPhaseTransportModel;

	@Value("${ctml.phase.phaseArray}")
	private String ctmlPhaseArray;
	
	@Value("${ctml.elementData}")
	private String ctmlElementData;
	
	@Value("${ctml.elementData.id}")
	private String ctmlElementDataId;
	
	@Value("${ctml.elementData.caseSensitive}")
	private String ctmlElementDataCaseSensitive;
	
	@Value("${ctml.elementData.element}")
	private String ctmlElementDataElement;
	
	@Value("${ctml.elementData.element.name}")
	private String ctmlElementDataElementName;
	
	@Value("${ctml.elementData.element.atomicWt}")
	private String ctmlElementDataElementAtomicWt;
	
	@Value("${ctml.elementData.element.atomicWt.units}")
	private String ctmlElementDataElementAtomicWtUnits;
	
	@Value("${ctml.element.atomicWt.Default.units}")
	private String ctmlElementAtomicWtDefaultUnits;
	
	@Value("${ctml.elementData.element.comment}")
	private String ctmlElementComment;
	
	@Value("${ctml.speciesData}")
	private String ctmlSpeciesData;
	
	@Value("${ctml.speciesData.id}")
	private String ctmlSpeciesDataId;
	
	@Value("${ctml.speciesData.caseSensitive}")
	private String ctmlSpeciesDataCaseSensitive;
	
	@Value("${ctml.speciesData.species}")
	private String ctmlSpeciesDataSpecies;
	
	@Value("${ctml.speciesData.species.comment}")
	private String ctmlSpeciesDataSpeciesComment;
	
	@Value("${ctml.speciesData.species.name}")
	private String ctmlSpeciesDataSpeciesName;
	
	@Value("${ctml.speciesData.species.phase}")
	private String ctmlSpeciesDataSpeciesPhase;
	
	@Value("${ctml.speciesData.species.note}")
	private String ctmlSpeciesDataSpeciesNote;
	
	@Value("${ctml.speciesData.species.atomArray}")
	private String ctmlSpeciesDataSpeciesAtomArray;

	@Value("${ctml.speciesData.species.size}")
	private String ctmlSpeciesSize;

	@Value("${ctml.speciesData.species.size.units}")
	private String ctmlSpeciesSizeUnits;

	@Value("${ctml.speciesData.species.density}")
	private String ctmlSpeciesDensity;

	@Value("${ctml.speciesData.species.density.units}")
	private String ctmlSpeciesDensityUnits;
	
	@Value("${ctml.speciesData.species.thermo}")
	private String ctmlSpeciesThermo;
	
	@Value("${ctml.speciesData.species.thermo.comment}")
	private String ctmlSpeciesThermoComment;
	
	@Value("${ctml.speciesData.species.thermo.nasa}")
	private String ctmlSpeciesThermoNasa;
	
	@Value("${ctml.speciesData.species.thermo.nasa.tMax}")
	private String ctmlNasaTMax;
	
	@Value("${ctml.speciesData.species.thermo.nasa.tMin}")
	private String ctmlNasaTMin;
	
	@Value("${ctml.speciesData.species.thermo.nasa.p0}")
	private String ctmlNasaP0;
	
	@Value("${ctml.speciesData.species.thermo.nasa.floatArray}")
	private String ctmlNasaPolCoeffArray;
	
	@Value("${ctml.speciesData.species.thermo.nasa.floatArray.name}")
	private String ctmlNasaPolCoeffArrayName;
	
	@Value("${ctml.speciesData.species.thermo.nasa.size}")
	private String ctmlNasaPolCoeffArraySize;
	
	@Value("${ctml.speciesData.species.transport}")
	private String ctmlSpeciesTransport;

	@Value("${ctml.speciesData.species.transport.model}")
	private String ctmlSpeciesTransportModel;
	
	@Value("${ctml.speciesData.species.transport.comment}")
	private String ctmlSpeciesTransportComment;

	@Value("${ctml.speciesData.species.transport.string}")
	private String ctmlSpeciesTransportString;

	@Value("${ctml.speciesData.species.transport.string.title}")
	private String ctmlTransportStringTitle;

	@Value("${ctml.speciesData.species.transport.LJ_welldepth}")
	private String ctmlTransportLJWelldepth;

	@Value("${ctml.speciesData.species.transport.LJ_welldepth.units}")
	private String ctmlTransportLJWelldepthUnits;
	
	@Value("${ctml.speciesData.species.transport.LJ_diameter}")
	private String ctmlTransportLJDiameter;
	
	@Value("${ctml.speciesData.species.transport.LJ_diameter.units}")
	private String ctmlTransportLJDiameterUnits;
	
	@Value("${ctml.speciesData.species.transport.dipoleMoment}")
	private String ctmlTransportDipoleMoment;
	
	@Value("${ctml.speciesData.species.transport.dipoleMoment.units}")
	private String ctmlTransportDipoleMomentUnits;
	
	@Value("${ctml.speciesData.species.transport.polarizability}")
	private String ctmlTransportPolarizability;
	
	@Value("${ctml.speciesData.species.transport.polarizability.units}")
	private String ctmlTransportPolarizabilityUnits;
	
	@Value("${ctml.speciesData.species.transport.rotRelax}")
	private String ctmlTransportRotRelax;
	
	@Value("${ctml.speciesData.species.transport.rotRelax.units}")
	private String ctmlTransportRotRelaxUnits;
	
	@Value("${ctml.reactionData}")
	private String reactionData;
	
	@Value("${ctml.reactionData.id}")
	private String reactionDataId;
	
	@Value("${ctml.reactionData.caseSensitive}")
	private String reactionDataCaseSensitive;
	
	@Value("${ctml.reactionData.reaction}")
	private String reactionDataReaction;
	
	@Value("${ctml.reactionData.reaction.duplicate}")
	private String reactionDuplicate;
			
	@Value("${ctml.reactionData.reaction.reversible}")
	private String reactionReversible;
	
	@Value("${ctml.reactionData.reaction.landauTeller}")
	private String reactionLandauTeller;

	@Value("${ctml.reactionData.reaction.type}")
	private String reactionType;

	@Value("${ctml.reactionData.reaction.type.threeBody.value}")
	private String reactionTypeThreeBodyValue;

	@Value("${ctml.reactionData.reaction.type.surface.value}")
	private String reactionTypeSurfaceValue;

	@Value("${ctml.reactionData.reaction.type.falloff.value}")
	private String reactionTypeFallOffValue;

	@Value("${ctml.reactionData.reaction.type.falloff.subtype.troe.value}")
	private String reactionTypeFallOffTroeValue;

	@Value("${ctml.reactionData.reaction.type.falloff.subtype.plog.value}")
	private String reactionTypeFallOffPLOGValue;

	@Value("${ctml.reactionData.reaction.type.falloff.subtype.lindemann.value}")
	private String reactionTypeFallOffLindemannValue;

	@Value("${ctml.reactionData.reaction.type.falloff.subtype.sri.value}")
	private String reactionTypeFallOffSRIValue;

	@Value("${ctml.reactionData.reaction.type.falloff.subtype.cheb.value}")
	private String reactionTypeFallOffCHEBValue;

	@Value("${ctml.reactionData.reaction.noncon}")
	private String reactionMayNonCon;

	@Value("${ctml.reactionData.reaction.partialpressure}")
	private String reactionPartialPressure;

	@Value("${ctml.reactionData.reaction.sitefrac}")
	private String reactionSiteFrac;
	
	@Value("${ctml.reactionData.reaction.id}")
	private String reactionId;
	
	@Value("${ctml.reactionData.reaction.comment}")
	private String reactionComment;
	
	@Value("${ctml.reactionData.reaction.equation}")
	private String reactionEquation;
	
	@Value("${ctml.reactionData.reaction.order}")
	private String reactionOrder;

	@Value("${ctml.reactionData.reaction.order.direction}")
	private String reactionDirection;

	@Value("${ctml.reactionData.reaction.order.species}")
	private String reactionOrderSpecies;
	
	@Value("${ctml.reactionData.reaction.rateCoeff}")
	private String reactionRateCoeff;
	
	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius}")
	private String reactionRateCoeffArrhenius;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.name}")
	private String reactionRateCoeffArrheniusName;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.type}")
	private String reactionRateCoeffArrheniusType;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.stick}")
	private String reactionRateCoeffArrheniusStick;	
	
	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.motz-wise}")
	private String reactionRateCoeffArrheniusMotzWise;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.species}")
	private String reactionRateCoeffArrheniusSpecies;
		
	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.a}")
	private String rateCoeffArrheniusA;
	
	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.a.units}")
	private String rateCoeffArrheniusAUnits;
	
	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.b}")
	private String rateCoeffArrheniusB;
	
	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.b.units}")
	private String rateCoeffArrheniusBUnits;
	
	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.e}")
	private String rateCoeffArrheniusE;
	
	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.e.units}")
	private String rateCoeffArrheniusEUnits;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.p}")
	private String rateCoeffArrheniusP;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.p.units}")
	private String rateCoeffArrheniusPUnits;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.coverage}")
	private String rateCoeffArrheniusCoverage;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.coverage.species}")
	private String rateCoeffArrheniusCoverageSpecies;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.coverage.a}")
	private String rateCoeffArrheniusCoverageA;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.coverage.a.units}")
	private String rateCoeffArrheniusCoverageAUnits;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.coverage.m}")
	private String rateCoeffArrheniusCoverageM;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.coverage.m.units}")
	private String rateCoeffArrheniusCoverageMUnits;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.coverage.e}")
	private String rateCoeffArrheniusCoverageE;

	@Value("${ctml.reactionData.reaction.rateCoeff.arrhenius.coverage.e.units}")
	private String rateCoeffArrheniusCoverageEUnits;

	@Value("${ctml.reactionData.reaction.rateCoeff.landauTeller}")
	private String rateCoeffLandauTeller;

	@Value("${ctml.reactionData.reaction.rateCoeff.landauTeller.b}")
	private String rateCoeffLandauTellerB;

	@Value("${ctml.reactionData.reaction.rateCoeff.landauTeller.b.units}")
	private String rateCoeffLandauTellerBUnits;

	@Value("${ctml.reactionData.reaction.rateCoeff.landauTeller.c}")
	private String rateCoeffLandauTellerC;

	@Value("${ctml.reactionData.reaction.rateCoeff.landauTeller.c.units}")
	private String rateCoeffLandauTellerCUnits;

	@Value("${ctml.reactionData.reaction.rateCoeff.efficiencies}")
	private String rateCoeffEfficiencies;

	@Value("${ctml.reactionData.reaction.rateCoeff.efficiencies.default}")
	private String rateCoeffEfficienciesDefaultValue;

	@Value("${ctml.reactionData.reaction.rateCoeff.falloff}")
	private String rateCoeffFallOff;

	@Value("${ctml.reactionData.reaction.rateCoeff.falloff.type}")
	private String rateCoeffFallOffType;

	@Value("${ctml.reactionData.reaction.rateCoeff.falloff.namedThirdBody}")
	private String rateCoeffFallOffThirdBody;

	@Value("${ctml.reactionData.reaction.rateCoeff.tmin}")
	private String rateCoeffTMin;

	@Value("${ctml.reactionData.reaction.rateCoeff.tmin.units}")
	private String rateCoeffTMinUnits;
	
	@Value("${ctml.reactionData.reaction.rateCoeff.tmax}")
	private String rateCoeffTMax;

	@Value("${ctml.reactionData.reaction.rateCoeff.tmax.units}")
	private String rateCoeffTMaxUnits;
	
	@Value("${ctml.reactionData.reaction.rateCoeff.pmin}")
	private String rateCoeffPMin;

	@Value("${ctml.reactionData.reaction.rateCoeff.pmin.units}")
	private String rateCoeffPMinUnits;

	@Value("${ctml.reactionData.reaction.rateCoeff.pmax}")
	private String rateCoeffPMax;

	@Value("${ctml.reactionData.reaction.rateCoeff.pmax.units}")
	private String rateCoeffPMaxUnits;
	
	@Value("${ctml.reactionData.reaction.rateCoeff.floatArray}")
	private String rateCoeffFloatArray;

	@Value("${ctml.reactionData.reaction.rateCoeff.floatArray.name}")
	private String rateCoeffFloatArrayName;

	@Value("${ctml.reactionData.reaction.rateCoeff.floatArray.units}")
	private String rateCoeffFloatArrayUnits;

	@Value("${ctml.reactionData.reaction.rateCoeff.floatArray.degreeT}")
	private String rateCoeffFloatArrayDegreeT;

	@Value("${ctml.reactionData.reaction.rateCoeff.floatArray.degreeP}")
	private String rateCoeffFloatArrayDegreeP;
	
	@Value("${ctml.reactionData.reaction.reactants}")
	private String reactionReactants;
	
	@Value("${ctml.reactionData.reaction.products}")
	private String reactionProducts;
	
	@Value("${ctml.phase.gas.species.data.id}")
	private String gasSpeciesDataId;
	
	@Value("${ctml.phase.id.gas}")
	private String phaseIdGas;

	@Value("${ctml.phase.id.site}")
	private String phaseIdSite;

	@Value("${ctml.phase.id.bulk}")
	private String phaseIdBulk;
	
	@Value("${ctml.phase.gas.reaction.data.id}")
	private String gasPhaseReactionDataId;
	
	@Value("${ctml.phase.material.name.starts.with}")
	private String materialNameStartsWith;	

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
	
	public String getCtmlMaterial() {
		return ctmlMaterial;
	}

	public void setCtmlMaterial(String ctmlMaterial) {
		this.ctmlMaterial = ctmlMaterial;
	}

	public void setCtmlComment(String ctmlComment) {
		this.ctmlComment = ctmlComment;
	}

	public String getCtmlValidate() {
		return ctmlValidate;
	}

	public void setCtmlValidate(String ctmlValidate) {
		this.ctmlValidate = ctmlValidate;
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

	public String getCtmlPhase() {
		return ctmlPhase;
	}

	public void setCtmlPhase(String ctmlPhase) {
		this.ctmlPhase = ctmlPhase;
	}

	public String getCtmlPhaseDimension() {
		return ctmlPhaseDimension;
	}

	public void setCtmlPhaseDimension(String ctmlPhaseDimension) {
		this.ctmlPhaseDimension = ctmlPhaseDimension;
	}

	public String getCtmlPhaseId() {
		return ctmlPhaseId;
	}

	public void setCtmlPhaseId(String ctmlPhaseId) {
		this.ctmlPhaseId = ctmlPhaseId;
	}

	public String getCtmlPhaseMaterial() {
		return ctmlPhaseMaterial;
	}

	public void setCtmlPhaseMaterial(String ctmlPhaseMaterial) {
		this.ctmlPhaseMaterial = ctmlPhaseMaterial;
	}

	public String getCtmlPhaseElementArray() {
		return ctmlPhaseElementArray;
	}

	public void setCtmlPhaseElementArray(String ctmlPhaseElementArray) {
		this.ctmlPhaseElementArray = ctmlPhaseElementArray;
	}

	public String getCtmlPhaseElmentArrayDataSrc() {
		return ctmlPhaseElmentArrayDataSrc;
	}

	public void setCtmlPhaseElmentArrayDataSrc(String ctmlPhaseElmentArrayDataSrc) {
		this.ctmlPhaseElmentArrayDataSrc = ctmlPhaseElmentArrayDataSrc;
	}

	public String getCtmlPhaseSpeciesArray() {
		return ctmlPhaseSpeciesArray;
	}

	public void setCtmlPhaseSpeciesArray(String ctmlPhaseSpeciesArray) {
		this.ctmlPhaseSpeciesArray = ctmlPhaseSpeciesArray;
	}

	public String getCtmlPhaseSpeciesArrayDataSrc() {
		return ctmlPhaseSpeciesArrayDataSrc;
	}

	public void setCtmlPhaseSpeciesArrayDataSrc(String ctmlPhaseSpeciesArrayDataSrc) {
		this.ctmlPhaseSpeciesArrayDataSrc = ctmlPhaseSpeciesArrayDataSrc;
	}

	public String getCtmlPhaseReactionArray() {
		return ctmlPhaseReactionArray;
	}

	public void setCtmlPhaseReactionArray(String ctmlPhaseReactionArray) {
		this.ctmlPhaseReactionArray = ctmlPhaseReactionArray;
	}

	public String getCtmlPhaseReactionArrayDataSrc() {
		return ctmlPhaseReactionArrayDataSrc;
	}

	public void setCtmlPhaseReactionArrayDataSrc(String ctmlPhaseReactionArrayDataSrc) {
		this.ctmlPhaseReactionArrayDataSrc = ctmlPhaseReactionArrayDataSrc;
	}

	public String getCtmlPhaseState() {
		return ctmlPhaseState;
	}

	public void setCtmlPhaseState(String ctmlPhaseState) {
		this.ctmlPhaseState = ctmlPhaseState;
	}

	public String getCtmlPhaseThermo() {
		return ctmlPhaseThermo;
	}

	public void setCtmlPhaseThermo(String ctmlPhaseThermo) {
		this.ctmlPhaseThermo = ctmlPhaseThermo;
	}

	public String getCtmlPhaseThermoModel() {
		return ctmlPhaseThermoModel;
	}

	public void setCtmlPhaseThermoModel(String ctmlPhaseThermoModel) {
		this.ctmlPhaseThermoModel = ctmlPhaseThermoModel;
	}

	public String getCtmlPhaseSiteDensity() {
		return ctmlPhaseSiteDensity;
	}

	public void setCtmlPhaseSiteDensity(String ctmlPhaseSiteDensity) {
		this.ctmlPhaseSiteDensity = ctmlPhaseSiteDensity;
	}

	public String getCtmlPhaseSiteDensityUnits() {
		return ctmlPhaseSiteDensityUnits;
	}

	public void setCtmlPhaseSiteDensityUnits(String ctmlPhaseSiteDensityUnits) {
		this.ctmlPhaseSiteDensityUnits = ctmlPhaseSiteDensityUnits;
	}

	public String getCtmlPhaseKinetics() {
		return ctmlPhaseKinetics;
	}

	public void setCtmlPhaseKinetics(String ctmlPhaseKinetics) {
		this.ctmlPhaseKinetics = ctmlPhaseKinetics;
	}

	public String getCtmlPhaseKineticsModel() {
		return ctmlPhaseKineticsModel;
	}

	public void setCtmlPhaseKineticsModel(String ctmlPhaseKineticsModel) {
		this.ctmlPhaseKineticsModel = ctmlPhaseKineticsModel;
	}

	public String getCtmlPhaseTransport() {
		return ctmlPhaseTransport;
	}

	public void setCtmlPhaseTransport(String ctmlPhaseTransport) {
		this.ctmlPhaseTransport = ctmlPhaseTransport;
	}

	public String getCtmlPhaseTransportModel() {
		return ctmlPhaseTransportModel;
	}

	public void setCtmlPhaseTransportModel(String ctmlPhaseTransportModel) {
		this.ctmlPhaseTransportModel = ctmlPhaseTransportModel;
	}

	public String getCtmlPhaseArray() {
		return ctmlPhaseArray;
	}

	public void setCtmlPhaseArray(String ctmlPhaseArray) {
		this.ctmlPhaseArray = ctmlPhaseArray;
	}

	public String getCtmlElementData() {
		return ctmlElementData;
	}

	public void setCtmlElementData(String ctmlElementData) {
		this.ctmlElementData = ctmlElementData;
	}

	public String getCtmlElementDataId() {
		return ctmlElementDataId;
	}

	public void setCtmlElementDataId(String ctmlElementDataId) {
		this.ctmlElementDataId = ctmlElementDataId;
	}

	public String getCtmlElementDataCaseSensitive() {
		return ctmlElementDataCaseSensitive;
	}

	public void setCtmlElementDataCaseSensitive(String ctmlElementDataCaseSensitive) {
		this.ctmlElementDataCaseSensitive = ctmlElementDataCaseSensitive;
	}

	public String getCtmlElementDataElement() {
		return ctmlElementDataElement;
	}

	public void setCtmlElementDataElement(String ctmlElementDataElement) {
		this.ctmlElementDataElement = ctmlElementDataElement;
	}

	public String getCtmlElementDataElementName() {
		return ctmlElementDataElementName;
	}

	public void setCtmlElementDataElementName(String ctmlElementDataElementName) {
		this.ctmlElementDataElementName = ctmlElementDataElementName;
	}

	public String getCtmlElementDataElementAtomicWt() {
		return ctmlElementDataElementAtomicWt;
	}

	public void setCtmlElementDataElementAtomicWt(String ctmlElementDataElementAtomicWt) {
		this.ctmlElementDataElementAtomicWt = ctmlElementDataElementAtomicWt;
	}
	
	public String getCtmlElementDataElementAtomicWtUnits() {
		return ctmlElementDataElementAtomicWtUnits;
	}

	public void setCtmlElementDataElementAtomicWtUnits(String ctmlElementDataElementAtomicWtUnits) {
		this.ctmlElementDataElementAtomicWtUnits = ctmlElementDataElementAtomicWtUnits;
	}

	public String getCtmlSpeciesData() {
		return ctmlSpeciesData;
	}

	public void setCtmlSpeciesData(String ctmlSpeciesData) {
		this.ctmlSpeciesData = ctmlSpeciesData;
	}

	public String getCtmlSpeciesDataId() {
		return ctmlSpeciesDataId;
	}

	public void setCtmlSpeciesDataId(String ctmlSpeciesDataId) {
		this.ctmlSpeciesDataId = ctmlSpeciesDataId;
	}

	public String getCtmlSpeciesDataCaseSensitive() {
		return ctmlSpeciesDataCaseSensitive;
	}

	public void setCtmlSpeciesDataCaseSensitive(String ctmlSpeciesDataCaseSensitive) {
		this.ctmlSpeciesDataCaseSensitive = ctmlSpeciesDataCaseSensitive;
	}

	public String getCtmlSpeciesDataSpecies() {
		return ctmlSpeciesDataSpecies;
	}

	public void setCtmlSpeciesDataSpecies(String ctmlSpeciesDataSpecies) {
		this.ctmlSpeciesDataSpecies = ctmlSpeciesDataSpecies;
	}

	public String getCtmlSpeciesDataSpeciesComment() {
		return ctmlSpeciesDataSpeciesComment;
	}

	public void setCtmlSpeciesDataSpeciesComment(String ctmlSpeciesDataSpeciesComment) {
		this.ctmlSpeciesDataSpeciesComment = ctmlSpeciesDataSpeciesComment;
	}

	public String getCtmlSpeciesDataSpeciesName() {
		return ctmlSpeciesDataSpeciesName;
	}

	public void setCtmlSpeciesDataSpeciesName(String ctmlSpeciesDataSpeciesName) {
		this.ctmlSpeciesDataSpeciesName = ctmlSpeciesDataSpeciesName;
	}

	public String getCtmlSpeciesDataSpeciesPhase() {
		return ctmlSpeciesDataSpeciesPhase;
	}

	public void setCtmlSpeciesDataSpeciesPhase(String ctmlSpeciesDataSpeciesPhase) {
		this.ctmlSpeciesDataSpeciesPhase = ctmlSpeciesDataSpeciesPhase;
	}

	public String getCtmlSpeciesDataSpeciesNote() {
		return ctmlSpeciesDataSpeciesNote;
	}

	public void setCtmlSpeciesDataSpeciesNote(String ctmlSpeciesDataSpeciesNote) {
		this.ctmlSpeciesDataSpeciesNote = ctmlSpeciesDataSpeciesNote;
	}

	public String getCtmlSpeciesDataSpeciesAtomArray() {
		return ctmlSpeciesDataSpeciesAtomArray;
	}

	public void setCtmlSpeciesDataSpeciesAtomArray(String ctmlSpeciesDataSpeciesAtomArray) {
		this.ctmlSpeciesDataSpeciesAtomArray = ctmlSpeciesDataSpeciesAtomArray;
	}

	public String getCtmlSpeciesSize() {
		return ctmlSpeciesSize;
	}

	public void setCtmlSpeciesSize(String ctmlSpeciesSize) {
		this.ctmlSpeciesSize = ctmlSpeciesSize;
	}

	public String getCtmlSpeciesSizeUnits() {
		return ctmlSpeciesSizeUnits;
	}

	public void setCtmlSpeciesSizeUnits(String ctmlSpeciesSizeUnits) {
		this.ctmlSpeciesSizeUnits = ctmlSpeciesSizeUnits;
	}

	public String getCtmlSpeciesDensity() {
		return ctmlSpeciesDensity;
	}

	public void setCtmlSpeciesDensity(String ctmlSpeciesDensity) {
		this.ctmlSpeciesDensity = ctmlSpeciesDensity;
	}

	public String getCtmlSpeciesDensityUnits() {
		return ctmlSpeciesDensityUnits;
	}

	public void setCtmlSpeciesDensityUnits(String ctmlSpeciesDensityUnits) {
		this.ctmlSpeciesDensityUnits = ctmlSpeciesDensityUnits;
	}

	public String getCtmlSpeciesThermo() {
		return ctmlSpeciesThermo;
	}

	public void setCtmlSpeciesThermo(String ctmlSpeciesThermo) {
		this.ctmlSpeciesThermo = ctmlSpeciesThermo;
	}

	public String getCtmlSpeciesThermoComment() {
		return ctmlSpeciesThermoComment;
	}

	public void setCtmlSpeciesThermoComment(String ctmlSpeciesThermoComment) {
		this.ctmlSpeciesThermoComment = ctmlSpeciesThermoComment;
	}

	public String getCtmlSpeciesThermoNasa() {
		return ctmlSpeciesThermoNasa;
	}

	public void setCtmlSpeciesThermoNasa(String ctmlSpeciesThermoNasa) {
		this.ctmlSpeciesThermoNasa = ctmlSpeciesThermoNasa;
	}

	public String getCtmlNasaTMax() {
		return ctmlNasaTMax;
	}

	public void setCtmlNasaTMax(String ctmlNasaTMax) {
		this.ctmlNasaTMax = ctmlNasaTMax;
	}

	public String getCtmlNasaTMin() {
		return ctmlNasaTMin;
	}

	public void setCtmlNasaTMin(String ctmlNasaTMin) {
		this.ctmlNasaTMin = ctmlNasaTMin;
	}

	public String getCtmlNasaP0() {
		return ctmlNasaP0;
	}

	public void setCtmlNasaP0(String ctmlNasaP0) {
		this.ctmlNasaP0 = ctmlNasaP0;
	}

	public String getCtmlNasaPolCoeffArray() {
		return ctmlNasaPolCoeffArray;
	}

	public void setCtmlNasaPolCoeffArray(String ctmlNasaPolCoeffArray) {
		this.ctmlNasaPolCoeffArray = ctmlNasaPolCoeffArray;
	}

	public String getCtmlNasaPolCoeffArrayName() {
		return ctmlNasaPolCoeffArrayName;
	}

	public void setCtmlNasaPolCoeffArrayName(String ctmlNasaPolCoeffArrayName) {
		this.ctmlNasaPolCoeffArrayName = ctmlNasaPolCoeffArrayName;
	}

	public String getCtmlNasaPolCoeffArraySize() {
		return ctmlNasaPolCoeffArraySize;
	}

	public void setCtmlNasaPolCoeffArraySize(String ctmlNasaPolCoeffArraySize) {
		this.ctmlNasaPolCoeffArraySize = ctmlNasaPolCoeffArraySize;
	}

	public String getCtmlSpeciesTransport() {
		return ctmlSpeciesTransport;
	}

	public void setCtmlSpeciesTransport(String ctmlSpeciesTransport) {
		this.ctmlSpeciesTransport = ctmlSpeciesTransport;
	}

	public String getCtmlSpeciesTransportModel() {
		return ctmlSpeciesTransportModel;
	}

	public void setCtmlSpeciesTransportModel(String ctmlSpeciesTransportModel) {
		this.ctmlSpeciesTransportModel = ctmlSpeciesTransportModel;
	}

	public String getCtmlSpeciesTransportComment() {
		return ctmlSpeciesTransportComment;
	}

	public void setCtmlSpeciesTransportComment(String ctmlSpeciesTransportComment) {
		this.ctmlSpeciesTransportComment = ctmlSpeciesTransportComment;
	}

	public String getCtmlSpeciesTransportString() {
		return ctmlSpeciesTransportString;
	}

	public void setCtmlSpeciesTransportString(String ctmlSpeciesTransportString) {
		this.ctmlSpeciesTransportString = ctmlSpeciesTransportString;
	}

	public String getCtmlTransportStringTitle() {
		return ctmlTransportStringTitle;
	}

	public void setCtmlTransportStringTitle(String ctmlTransportStringTitle) {
		this.ctmlTransportStringTitle = ctmlTransportStringTitle;
	}

	public String getCtmlTransportLJWelldepth() {
		return ctmlTransportLJWelldepth;
	}

	public void setCtmlTransportLJWelldepth(String ctmlTransportLJWelldepth) {
		this.ctmlTransportLJWelldepth = ctmlTransportLJWelldepth;
	}

	public String getCtmlTransportLJWelldepthUnits() {
		return ctmlTransportLJWelldepthUnits;
	}

	public void setCtmlTransportLJWelldepthUnits(String ctmlTransportLJWelldepthUnits) {
		this.ctmlTransportLJWelldepthUnits = ctmlTransportLJWelldepthUnits;
	}

	public String getCtmlTransportLJDiameter() {
		return ctmlTransportLJDiameter;
	}

	public void setCtmlTransportLJDiameter(String ctmlTransportLJDiameter) {
		this.ctmlTransportLJDiameter = ctmlTransportLJDiameter;
	}

	public String getCtmlTransportLJDiameterUnits() {
		return ctmlTransportLJDiameterUnits;
	}

	public void setCtmlTransportLJDiameterUnits(String ctmlTransportLJDiameterUnits) {
		this.ctmlTransportLJDiameterUnits = ctmlTransportLJDiameterUnits;
	}

	public String getCtmlTransportDipoleMoment() {
		return ctmlTransportDipoleMoment;
	}

	public void setCtmlTransportDipoleMoment(String ctmlTransportDipoleMoment) {
		this.ctmlTransportDipoleMoment = ctmlTransportDipoleMoment;
	}

	public String getCtmlTransportDipoleMomentUnits() {
		return ctmlTransportDipoleMomentUnits;
	}

	public void setCtmlTransportDipoleMomentUnits(String ctmlTransportDipoleMomentUnits) {
		this.ctmlTransportDipoleMomentUnits = ctmlTransportDipoleMomentUnits;
	}

	public String getCtmlTransportPolarizability() {
		return ctmlTransportPolarizability;
	}

	public void setCtmlTransportPolarizability(String ctmlTransportPolarizability) {
		this.ctmlTransportPolarizability = ctmlTransportPolarizability;
	}

	public String getCtmlTransportPolarizabilityUnits() {
		return ctmlTransportPolarizabilityUnits;
	}

	public void setCtmlTransportPolarizabilityUnits(String ctmlTransportPolarizabilityUnits) {
		this.ctmlTransportPolarizabilityUnits = ctmlTransportPolarizabilityUnits;
	}

	public String getCtmlTransportRotRelax() {
		return ctmlTransportRotRelax;
	}

	public void setCtmlTransportRotRelax(String ctmlTransportRotRelax) {
		this.ctmlTransportRotRelax = ctmlTransportRotRelax;
	}

	public String getCtmlTransportRotRelaxUnits() {
		return ctmlTransportRotRelaxUnits;
	}

	public void setCtmlTransportRotRelaxUnits(String ctmlTransportRotRelaxUnits) {
		this.ctmlTransportRotRelaxUnits = ctmlTransportRotRelaxUnits;
	}

	public String getReactionData() {
		return reactionData;
	}

	public void setReactionData(String reactionData) {
		this.reactionData = reactionData;
	}

	public String getReactionDataId() {
		return reactionDataId;
	}

	public void setReactionDataId(String reactionDataId) {
		this.reactionDataId = reactionDataId;
	}
	
	public String getReactionDataCaseSensitive() {
		return reactionDataCaseSensitive;
	}

	public void setReactionDataCaseSensitive(String reactionDataCaseSensitive) {
		this.reactionDataCaseSensitive = reactionDataCaseSensitive;
	}

	public String getReactionDataReaction() {
		return reactionDataReaction;
	}

	public void setReactionDataReaction(String reactionDataReaction) {
		this.reactionDataReaction = reactionDataReaction;
	}
	
	public String getReactionDuplicate() {
		return reactionDuplicate;
	}

	public void setReactionDuplicate(String reactionDuplicate) {
		this.reactionDuplicate = reactionDuplicate;
	}

	public String getReactionReversible() {
		return reactionReversible;
	}

	public void setReactionReversible(String reactionReversible) {
		this.reactionReversible = reactionReversible;
	}

	public String getReactionLandauTeller() {
		return reactionLandauTeller;
	}

	public void setReactionLandauTeller(String reactionLandauTeller) {
		this.reactionLandauTeller = reactionLandauTeller;
	}

	public String getReactionType() {
		return reactionType;
	}

	public void setReactionType(String reactionType) {
		this.reactionType = reactionType;
	}

	public String getReactionMayNonCon() {
		return reactionMayNonCon;
	}

	public void setReactionMayNonCon(String reactionMayNonCon) {
		this.reactionMayNonCon = reactionMayNonCon;
	}

	public String getReactionPartialPressure() {
		return reactionPartialPressure;
	}

	public void setReactionPartialPressure(String reactionPartialPressure) {
		this.reactionPartialPressure = reactionPartialPressure;
	}

	public String getReactionSiteFrac() {
		return reactionSiteFrac;
	}

	public void setReactionSiteFrac(String reactionSiteFrac) {
		this.reactionSiteFrac = reactionSiteFrac;
	}

	public String getReactionId() {
		return reactionId;
	}

	public void setReactionId(String reactionId) {
		this.reactionId = reactionId;
	}

	public String getReactionComment() {
		return reactionComment;
	}

	public void setReactionComment(String reactionComment) {
		this.reactionComment = reactionComment;
	}

	public String getReactionEquation() {
		return reactionEquation;
	}

	public void setReactionEquation(String reactionEquation) {
		this.reactionEquation = reactionEquation;
	}

	public String getReactionOrder() {
		return reactionOrder;
	}

	public void setReactionOrder(String reactionOrder) {
		this.reactionOrder = reactionOrder;
	}

	public String getReactionDirection() {
		return reactionDirection;
	}

	public void setReactionDirection(String reactionDirection) {
		this.reactionDirection = reactionDirection;
	}

	public String getReactionOrderSpecies() {
		return reactionOrderSpecies;
	}

	public void setReactionOrderSpecies(String reactionOrderSpecies) {
		this.reactionOrderSpecies = reactionOrderSpecies;
	}

	public String getReactionRateCoeff() {
		return reactionRateCoeff;
	}

	public void setReactionRateCoeff(String reactionRateCoeff) {
		this.reactionRateCoeff = reactionRateCoeff;
	}

	public String getReactionRateCoeffArrhenius() {
		return reactionRateCoeffArrhenius;
	}

	public void setReactionRateCoeffArrhenius(String reactionRateCoeffArrhenius) {
		this.reactionRateCoeffArrhenius = reactionRateCoeffArrhenius;
	}

	public String getReactionRateCoeffArrheniusName() {
		return reactionRateCoeffArrheniusName;
	}

	public void setReactionRateCoeffArrheniusName(String reactionRateCoeffArrheniusName) {
		this.reactionRateCoeffArrheniusName = reactionRateCoeffArrheniusName;
	}

	public String getReactionRateCoeffArrheniusType() {
		return reactionRateCoeffArrheniusType;
	}

	public void setReactionRateCoeffArrheniusType(String reactionRateCoeffArrheniusType) {
		this.reactionRateCoeffArrheniusType = reactionRateCoeffArrheniusType;
	}

	public String getReactionRateCoeffArrheniusMotzWise() {
		return reactionRateCoeffArrheniusMotzWise;
	}

	public void setReactionRateCoeffArrheniusMotzWise(String reactionRateCoeffArrheniusMotzWise) {
		this.reactionRateCoeffArrheniusMotzWise = reactionRateCoeffArrheniusMotzWise;
	}

	public String getReactionRateCoeffArrheniusSpecies() {
		return reactionRateCoeffArrheniusSpecies;
	}

	public void setReactionRateCoeffArrheniusSpecies(String reactionRateCoeffArrheniusSpecies) {
		this.reactionRateCoeffArrheniusSpecies = reactionRateCoeffArrheniusSpecies;
	}

	public String getRateCoeffArrheniusA() {
		return rateCoeffArrheniusA;
	}

	public void setRateCoeffArrheniusA(String rateCoeffArrheniusA) {
		this.rateCoeffArrheniusA = rateCoeffArrheniusA;
	}

	public String getRateCoeffArrheniusAUnits() {
		return rateCoeffArrheniusAUnits;
	}

	public void setRateCoeffArrheniusAUnits(String rateCoeffArrheniusAUnits) {
		this.rateCoeffArrheniusAUnits = rateCoeffArrheniusAUnits;
	}

	public String getRateCoeffArrheniusB() {
		return rateCoeffArrheniusB;
	}

	public void setRateCoeffArrheniusB(String rateCoeffArrheniusB) {
		this.rateCoeffArrheniusB = rateCoeffArrheniusB;
	}

	public String getRateCoeffArrheniusBUnits() {
		return rateCoeffArrheniusBUnits;
	}

	public void setRateCoeffArrheniusBUnits(String rateCoeffArrheniusBUnits) {
		this.rateCoeffArrheniusBUnits = rateCoeffArrheniusBUnits;
	}

	public String getRateCoeffArrheniusE() {
		return rateCoeffArrheniusE;
	}

	public void setRateCoeffArrheniusE(String rateCoeffArrheniusE) {
		this.rateCoeffArrheniusE = rateCoeffArrheniusE;
	}

	public String getRateCoeffArrheniusEUnits() {
		return rateCoeffArrheniusEUnits;
	}

	public void setRateCoeffArrheniusEUnits(String rateCoeffArrheniusEUnits) {
		this.rateCoeffArrheniusEUnits = rateCoeffArrheniusEUnits;
	}

	public String getRateCoeffArrheniusP() {
		return rateCoeffArrheniusP;
	}

	public void setRateCoeffArrheniusP(String rateCoeffArrheniusP) {
		this.rateCoeffArrheniusP = rateCoeffArrheniusP;
	}

	public String getRateCoeffArrheniusPUnits() {
		return rateCoeffArrheniusPUnits;
	}

	public void setRateCoeffArrheniusPUnits(String rateCoeffArrheniusPUnits) {
		this.rateCoeffArrheniusPUnits = rateCoeffArrheniusPUnits;
	}

	public String getRateCoeffArrheniusCoverage() {
		return rateCoeffArrheniusCoverage;
	}

	public void setRateCoeffArrheniusCoverage(String rateCoeffArrheniusCoverage) {
		this.rateCoeffArrheniusCoverage = rateCoeffArrheniusCoverage;
	}

	public String getRateCoeffArrheniusCoverageSpecies() {
		return rateCoeffArrheniusCoverageSpecies;
	}

	public void setRateCoeffArrheniusCoverageSpecies(String rateCoeffArrheniusCoverageSpecies) {
		this.rateCoeffArrheniusCoverageSpecies = rateCoeffArrheniusCoverageSpecies;
	}

	public String getRateCoeffArrheniusCoverageA() {
		return rateCoeffArrheniusCoverageA;
	}

	public void setRateCoeffArrheniusCoverageA(String rateCoeffArrheniusCoverageA) {
		this.rateCoeffArrheniusCoverageA = rateCoeffArrheniusCoverageA;
	}

	public String getRateCoeffArrheniusCoverageAUnits() {
		return rateCoeffArrheniusCoverageAUnits;
	}

	public void setRateCoeffArrheniusCoverageAUnits(String rateCoeffArrheniusCoverageAUnits) {
		this.rateCoeffArrheniusCoverageAUnits = rateCoeffArrheniusCoverageAUnits;
	}

	public String getRateCoeffArrheniusCoverageM() {
		return rateCoeffArrheniusCoverageM;
	}

	public void setRateCoeffArrheniusCoverageM(String rateCoeffArrheniusCoverageM) {
		this.rateCoeffArrheniusCoverageM = rateCoeffArrheniusCoverageM;
	}

	public String getRateCoeffArrheniusCoverageMUnits() {
		return rateCoeffArrheniusCoverageMUnits;
	}

	public void setRateCoeffArrheniusCoverageMUnits(String rateCoeffArrheniusCoverageMUnits) {
		this.rateCoeffArrheniusCoverageMUnits = rateCoeffArrheniusCoverageMUnits;
	}

	public String getRateCoeffArrheniusCoverageE() {
		return rateCoeffArrheniusCoverageE;
	}

	public void setRateCoeffArrheniusCoverageE(String rateCoeffArrheniusCoverageE) {
		this.rateCoeffArrheniusCoverageE = rateCoeffArrheniusCoverageE;
	}

	public String getRateCoeffArrheniusCoverageEUnits() {
		return rateCoeffArrheniusCoverageEUnits;
	}

	public void setRateCoeffArrheniusCoverageEUnits(String rateCoeffArrheniusCoverageEUnits) {
		this.rateCoeffArrheniusCoverageEUnits = rateCoeffArrheniusCoverageEUnits;
	}

	public String getRateCoeffLandauTeller() {
		return rateCoeffLandauTeller;
	}

	public void setRateCoeffLandauTeller(String rateCoeffLandauTeller) {
		this.rateCoeffLandauTeller = rateCoeffLandauTeller;
	}

	public String getRateCoeffLandauTellerB() {
		return rateCoeffLandauTellerB;
	}

	public void setRateCoeffLandauTellerB(String rateCoeffLandauTellerB) {
		this.rateCoeffLandauTellerB = rateCoeffLandauTellerB;
	}

	public String getRateCoeffLandauTellerBUnits() {
		return rateCoeffLandauTellerBUnits;
	}

	public void setRateCoeffLandauTellerBUnits(String rateCoeffLandauTellerBUnits) {
		this.rateCoeffLandauTellerBUnits = rateCoeffLandauTellerBUnits;
	}

	public String getRateCoeffLandauTellerC() {
		return rateCoeffLandauTellerC;
	}

	public void setRateCoeffLandauTellerC(String rateCoeffLandauTellerC) {
		this.rateCoeffLandauTellerC = rateCoeffLandauTellerC;
	}

	public String getRateCoeffLandauTellerCUnits() {
		return rateCoeffLandauTellerCUnits;
	}

	public void setRateCoeffLandauTellerCUnits(String rateCoeffLandauTellerCUnits) {
		this.rateCoeffLandauTellerCUnits = rateCoeffLandauTellerCUnits;
	}

	public String getRateCoeffEfficiencies() {
		return rateCoeffEfficiencies;
	}

	public void setRateCoeffEfficiencies(String rateCoeffEfficiencies) {
		this.rateCoeffEfficiencies = rateCoeffEfficiencies;
	}

	public String getRateCoeffEfficienciesDefaultValue() {
		return rateCoeffEfficienciesDefaultValue;
	}

	public void setRateCoeffEfficienciesDefaultValue(String rateCoeffEfficienciesDefaultValue) {
		this.rateCoeffEfficienciesDefaultValue = rateCoeffEfficienciesDefaultValue;
	}

	public String getRateCoeffFallOff() {
		return rateCoeffFallOff;
	}

	public void setRateCoeffFallOff(String rateCoeffFallOff) {
		this.rateCoeffFallOff = rateCoeffFallOff;
	}

	public String getRateCoeffFallOffType() {
		return rateCoeffFallOffType;
	}

	public void setRateCoeffFallOffType(String rateCoeffFallOffType) {
		this.rateCoeffFallOffType = rateCoeffFallOffType;
	}

	public String getRateCoeffFallOffThirdBody() {
		return rateCoeffFallOffThirdBody;
	}

	public void setRateCoeffFallOffThirdBody(String rateCoeffFallOffThirdBody) {
		this.rateCoeffFallOffThirdBody = rateCoeffFallOffThirdBody;
	}

	public String getRateCoeffTMin() {
		return rateCoeffTMin;
	}

	public void setRateCoeffTMin(String rateCoeffTMin) {
		this.rateCoeffTMin = rateCoeffTMin;
	}

	public String getRateCoeffTMax() {
		return rateCoeffTMax;
	}

	public void setRateCoeffTMax(String rateCoeffTMax) {
		this.rateCoeffTMax = rateCoeffTMax;
	}

	public String getRateCoeffPMin() {
		return rateCoeffPMin;
	}

	public void setRateCoeffPMin(String rateCoeffPMin) {
		this.rateCoeffPMin = rateCoeffPMin;
	}

	public String getRateCoeffPMax() {
		return rateCoeffPMax;
	}

	public void setRateCoeffPMax(String rateCoeffPMax) {
		this.rateCoeffPMax = rateCoeffPMax;
	}

	public String getRateCoeffFloatArray() {
		return rateCoeffFloatArray;
	}

	public void setRateCoeffFloatArray(String rateCoeffFloatArray) {
		this.rateCoeffFloatArray = rateCoeffFloatArray;
	}

	public String getRateCoeffFloatArrayName() {
		return rateCoeffFloatArrayName;
	}

	public void setRateCoeffFloatArrayName(String rateCoeffFloatArrayName) {
		this.rateCoeffFloatArrayName = rateCoeffFloatArrayName;
	}

	public String getRateCoeffFloatArrayUnits() {
		return rateCoeffFloatArrayUnits;
	}

	public void setRateCoeffFloatArrayUnits(String rateCoeffFloatArrayUnits) {
		this.rateCoeffFloatArrayUnits = rateCoeffFloatArrayUnits;
	}

	public String getRateCoeffFloatArrayDegreeT() {
		return rateCoeffFloatArrayDegreeT;
	}

	public void setRateCoeffFloatArrayDegreeT(String rateCoeffFloatArrayDegreeT) {
		this.rateCoeffFloatArrayDegreeT = rateCoeffFloatArrayDegreeT;
	}

	public String getRateCoeffFloatArrayDegreeP() {
		return rateCoeffFloatArrayDegreeP;
	}

	public void setRateCoeffFloatArrayDegreeP(String rateCoeffFloatArrayDegreeP) {
		this.rateCoeffFloatArrayDegreeP = rateCoeffFloatArrayDegreeP;
	}

	public String getReactionReactants() {
		return reactionReactants;
	}

	public void setReactionReactants(String reactionReactants) {
		this.reactionReactants = reactionReactants;
	}

	public String getReactionProducts() {
		return reactionProducts;
	}

	public void setReactionProducts(String reactionProducts) {
		this.reactionProducts = reactionProducts;
	}

	public String getRateCoeffTMinUnits() {
		return rateCoeffTMinUnits;
	}

	public void setRateCoeffTMinUnits(String rateCoeffTMinUnits) {
		this.rateCoeffTMinUnits = rateCoeffTMinUnits;
	}

	public String getRateCoeffTMaxUnits() {
		return rateCoeffTMaxUnits;
	}

	public void setRateCoeffTMaxUnits(String rateCoeffTMaxUnits) {
		this.rateCoeffTMaxUnits = rateCoeffTMaxUnits;
	}

	public String getRateCoeffPMinUnits() {
		return rateCoeffPMinUnits;
	}

	public void setRateCoeffPMinUnits(String rateCoeffPMinUnits) {
		this.rateCoeffPMinUnits = rateCoeffPMinUnits;
	}

	public String getRateCoeffPMaxUnits() {
		return rateCoeffPMaxUnits;
	}

	public void setRateCoeffPMaxUnits(String rateCoeffPMaxUnits) {
		this.rateCoeffPMaxUnits = rateCoeffPMaxUnits;
	}

	public String getReactionTypeThreeBodyValue() {
		return reactionTypeThreeBodyValue;
	}

	public void setReactionTypeThreeBodyValue(String reactionTypeThreeBodyValue) {
		this.reactionTypeThreeBodyValue = reactionTypeThreeBodyValue;
	}

	public String getReactionTypeSurfaceValue() {
		return reactionTypeSurfaceValue;
	}

	public void setReactionTypeSurfaceValue(String reactionTypeSurfaceValue) {
		this.reactionTypeSurfaceValue = reactionTypeSurfaceValue;
	}

	public String getReactionTypeFallOffValue() {
		return reactionTypeFallOffValue;
	}

	public void setReactionTypeFallOffValue(String reactionTypeFallOffValue) {
		this.reactionTypeFallOffValue = reactionTypeFallOffValue;
	}

	public String getReactionTypeFallOffTroeValue() {
		return reactionTypeFallOffTroeValue;
	}

	public void setReactionTypeFallOffTroeValue(String reactionTypeFallOffTroeValue) {
		this.reactionTypeFallOffTroeValue = reactionTypeFallOffTroeValue;
	}

	public String getReactionTypeFallOffPLOGValue() {
		return reactionTypeFallOffPLOGValue;
	}

	public void setReactionTypeFallOffPLOGValue(String reactionTypeFallOffPLOGValue) {
		this.reactionTypeFallOffPLOGValue = reactionTypeFallOffPLOGValue;
	}

	public String getReactionTypeFallOffLindemannValue() {
		return reactionTypeFallOffLindemannValue;
	}

	public void setReactionTypeFallOffLindemannValue(String reactionTypeFallOffLindemannValue) {
		this.reactionTypeFallOffLindemannValue = reactionTypeFallOffLindemannValue;
	}

	public String getReactionTypeFallOffSRIValue() {
		return reactionTypeFallOffSRIValue;
	}

	public void setReactionTypeFallOffSRIValue(String reactionTypeFallOffSRIValue) {
		this.reactionTypeFallOffSRIValue = reactionTypeFallOffSRIValue;
	}

	public String getReactionTypeFallOffCHEBValue() {
		return reactionTypeFallOffCHEBValue;
	}

	public void setReactionTypeFallOffCHEBValue(String reactionTypeFallOffCHEBValue) {
		this.reactionTypeFallOffCHEBValue = reactionTypeFallOffCHEBValue;
	}

	public String getGasSpeciesDataId() {
		return gasSpeciesDataId;
	}

	public void setGasSpeciesDataId(String gasSpeciesDataId) {
		this.gasSpeciesDataId = gasSpeciesDataId;
	}

	public String getPhaseIdGas() {
		return phaseIdGas;
	}

	public void setPhaseIdGas(String phaseIdGas) {
		this.phaseIdGas = phaseIdGas;
	}

	public String getPhaseIdSite() {
		return phaseIdSite;
	}

	public void setPhaseIdSite(String phaseIdSite) {
		this.phaseIdSite = phaseIdSite;
	}

	public String getPhaseIdBulk() {
		return phaseIdBulk;
	}

	public void setPhaseIdBulk(String phaseIdBulk) {
		this.phaseIdBulk = phaseIdBulk;
	}

	public String getGasPhaseReactionDataId() {
		return gasPhaseReactionDataId;
	}

	public void setGasPhaseReactionDataId(String gasPhaseReactionDataId) {
		this.gasPhaseReactionDataId = gasPhaseReactionDataId;
	}

	public String getMaterialNameStartsWith() {
		return materialNameStartsWith;
	}

	public void setMaterialNameStartsWith(String materialNameStartsWith) {
		this.materialNameStartsWith = materialNameStartsWith;
	}

	public String getCtmlElementAtomicWtDefaultUnits() {
		return ctmlElementAtomicWtDefaultUnits;
	}

	public void setCtmlElementAtomicWtDefaultUnits(String ctmlElementAtomicWtDefaultUnits) {
		this.ctmlElementAtomicWtDefaultUnits = ctmlElementAtomicWtDefaultUnits;
	}

	public String getCtmlElementComment() {
		return ctmlElementComment;
	}

	public void setCtmlElementComment(String ctmlElementComment) {
		this.ctmlElementComment = ctmlElementComment;
	}

	public String getReactionRateCoeffArrheniusStick() {
		return reactionRateCoeffArrheniusStick;
	}

	public void setReactionRateCoeffArrheniusStick(String reactionRateCoeffArrheniusStick) {
		this.reactionRateCoeffArrheniusStick = reactionRateCoeffArrheniusStick;
	}
}