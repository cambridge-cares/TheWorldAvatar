package com.cmclinnovations.ontochem.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochem.model.configuration.AppConfigOperationControl;
import com.cmclinnovations.ontochem.model.configuration.CtmlVocabulary;
import com.cmclinnovations.ontochem.model.configuration.OntoChemKB;
import com.cmclinnovations.ontochem.model.configuration.OntoKinVocabulary;
import com.cmclinnovations.ontochem.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontochem.model.converter.ctml.ElementConverter;
import com.cmclinnovations.ontochem.model.converter.ctml.MetadataConverter;
import com.cmclinnovations.ontochem.model.converter.ctml.PhaseConverter;
import com.cmclinnovations.ontochem.model.converter.ctml.ReactionConverter;
import com.cmclinnovations.ontochem.model.converter.ctml.SpeciesConverter;
import com.cmclinnovations.ontochem.model.converter.json.CompChemOwlWriter;
import com.cmclinnovations.ontochem.model.converter.owl.query.sparql.ElementQuery;
import com.cmclinnovations.ontochem.model.converter.owl.query.sparql.MetadataQuery;
import com.cmclinnovations.ontochem.model.converter.owl.query.sparql.PhaseQuery;
import com.cmclinnovations.ontochem.model.converter.owl.query.sparql.ReactionQuery;
import com.cmclinnovations.ontochem.model.converter.owl.query.sparql.SpeciesQuery;
import com.cmclinnovations.ontokin.model.data.structure.compchem.CompChem;
import com.cmclinnovations.ontokin.model.data.structure.ctml.Ctml;
import com.cmclinnovations.ontokin.model.data.structure.ctml.CtmlComment;
import com.cmclinnovations.ontokin.model.data.structure.ctml.Validate;
import com.cmclinnovations.ontokin.model.data.structure.ctml.element.Element;
import com.cmclinnovations.ontokin.model.data.structure.ctml.element.ElementData;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.ElementArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Kinetics;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Phase;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.PhaseArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.ReactionArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.SpeciesArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.State;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Thermo;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Transport;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.thermo.SiteDensity;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Arrhenius;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientA;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientE;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientP;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ArrheniusCoefficientb;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Coverage;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.CoverageParameterA;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.CoverageParameterE;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.CoverageParameterM;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Efficiencies;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.FallOff;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.LandauTeller;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.LandauTellerCoefficientB;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.LandauTellerCoefficientC;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.RateCoefficient;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Reaction;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ReactionData;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ReactionOrder;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.PMax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.PMin;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.RateCoeffFloatArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.TMax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.TMin;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Density;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.DipoleMoment;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.FloatArray;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.LJDiameter;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.LJWellDepth;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.NASA;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Polarizability;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.RotRelax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.STRING;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Size;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.Species;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesData;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesThermo;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesTransport;
import com.cmclinnovations.ontochem.model.owl.ElementWriter;
import com.cmclinnovations.ontochem.model.owl.MetadataWriter;
import com.cmclinnovations.ontochem.model.owl.OwlConstructWriter;
import com.cmclinnovations.ontochem.model.owl.PhaseWriter;
import com.cmclinnovations.ontochem.model.owl.ReactionWriter;
import com.cmclinnovations.ontochem.model.owl.SpeciesWriter;
import com.cmclinnovations.ontokin.model.parse.status.ctml.CtmlCommentParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.ElementDataParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.MetaDataParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.PhaseParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.ReactionDataParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.SpeciesDataParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.ValidateParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.elementdata.ElementParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.ElementArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.KineticsParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.PhaseArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.ReactionArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.SpeciesArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.StateParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.ThermoParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.TransportParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.phase.thermo.SiteDensityParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.ReactionParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.RateCoefficientParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ReactionOrderParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.ArrheniusParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.CoverageParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.EfficiencyParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.FallOffParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.LandauTellerParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction.ratecoeff.RateCoeffArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.SpeciesParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.DensityParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.SizeParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.SpeciesThermoParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.SpeciesTransportParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.nasa.CoefficientArrayParseStatus;
import com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species.nasa.NASAPolynomialParseStatus;
import com.cmclinnovations.ontochem.model.parser.ctml.CtmlCommentParser;
import com.cmclinnovations.ontochem.model.parser.ctml.ElementDataParser;
import com.cmclinnovations.ontochem.model.parser.ctml.MetadataParser;
import com.cmclinnovations.ontochem.model.parser.ctml.PhaseParser;
import com.cmclinnovations.ontochem.model.parser.ctml.ReactionDataParser;
import com.cmclinnovations.ontochem.model.parser.ctml.SpeciesDataParser;
import com.cmclinnovations.ontochem.model.parser.ctml.ValidateParser;
import com.cmclinnovations.ontochem.model.parser.ctml.elementdata.ElementParser;
import com.cmclinnovations.ontochem.model.parser.ctml.phase.ElementArrayParser;
import com.cmclinnovations.ontochem.model.parser.ctml.phase.KineticsParser;
import com.cmclinnovations.ontochem.model.parser.ctml.phase.PhaseArrayParser;
import com.cmclinnovations.ontochem.model.parser.ctml.phase.PhaseThermoParser;
import com.cmclinnovations.ontochem.model.parser.ctml.phase.ReactionArrayParser;
import com.cmclinnovations.ontochem.model.parser.ctml.phase.SpeciesArrayParser;
import com.cmclinnovations.ontochem.model.parser.ctml.phase.StateParser;
import com.cmclinnovations.ontochem.model.parser.ctml.phase.TransportParser;
import com.cmclinnovations.ontochem.model.parser.ctml.phase.thermo.SiteDensityParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.ReactionParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.ArrheniusParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.CoverageParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.EfficiencyParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.FallOffParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.LandauTellerParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.RateCoeffArrayParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.RateCoeffPressureMaxParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.RateCoeffPressureMinParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.RateCoeffTempMaxParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.RateCoeffTempMinParser;
import com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction.ReactionOrderParser;
import com.cmclinnovations.ontochem.model.parser.ctml.speciesdata.SpeciesParser;
import com.cmclinnovations.ontochem.model.parser.ctml.speciesdata.species.DensityParser;
import com.cmclinnovations.ontochem.model.parser.ctml.speciesdata.species.SizeParser;
import com.cmclinnovations.ontochem.model.parser.ctml.speciesdata.species.SpeciesThermoParser;
import com.cmclinnovations.ontochem.model.parser.ctml.speciesdata.species.SpeciesTransportParser;
import com.cmclinnovations.ontochem.model.parser.ctml.speciesdata.species.nasa.CoefficentArrayParser;
import com.cmclinnovations.ontochem.model.parser.ctml.speciesdata.species.nasa.NASAPolynomialParser;
import com.cmclinnovations.ontochem.model.reference.Doi;
import com.cmclinnovations.ontochem.model.reference.DoiParser;
import com.cmclinnovations.ontochem.model.reference.data.structure.Creator;
import com.cmclinnovations.ontochem.model.reference.data.structure.Journal;
import com.cmclinnovations.ontochem.model.reference.data.structure.JournalSpecification;
import com.cmclinnovations.ontochem.model.reference.data.structure.PublicationSpecification;
import com.cmclinnovations.ontochem.model.reference.data.structure.Reference;

/**
 * Implemented the following methods of the IInitCtmlConverter interface:</br>
 * 1. the init method;</br>
 * 2. the initPhase method;</br>
 * 3. the initElement method;</br>
 * 4. the initSpecies method; and</br>
 * 5. the initReaction method.
 * @author msff2
 *
 */
public class InitCtmlConverter extends CtmlConverter implements IInitCtmlConverter {
	public void init() {
		
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (opCtrl == null) {
			opCtrl = applicationContext.getBean(AppConfigOperationControl.class);
		}
		if (appConfigOntokin == null) {
			appConfigOntokin = applicationContext.getBean(OntoKinVocabulary.class);
		}
		if (appConfigCtml == null) {
			appConfigCtml = applicationContext.getBean(CtmlVocabulary.class);
		}
		if(ontoChemKB == null){
			ontoChemKB = applicationContext.getBean(OntoChemKB.class);
		}
		
		if(iOwlConstructWriter == null){
			iOwlConstructWriter = new OwlConstructWriter(); 
		}
		
		ctmlMDParseStatus = new MetaDataParseStatus();
		iCtmlMDParser = new MetadataParser();
		ctmlMD = new Ctml();

		ctmlCommentParseStatus = new CtmlCommentParseStatus();
		iCtmlCommentParser = new CtmlCommentParser();
		ctmlComment = new CtmlComment();
		
		validateParseStatus = new ValidateParseStatus();
		iValidateParser = new ValidateParser();
		validateMD = new Validate();

		phaseParseStatus = new PhaseParseStatus();
		iPhaseParser = new PhaseParser();
		phaseMD = new Phase();

		elementArrayParseStatus = new ElementArrayParseStatus();
		iElementArrayParser = new ElementArrayParser();
		elementArray = new ElementArray();

		speciesArrayParseStatus = new SpeciesArrayParseStatus();
		iSpeciesArrayParser = new SpeciesArrayParser();
		speciesArray = new SpeciesArray();

		reactionArrayParseStatus = new ReactionArrayParseStatus();
		iReactionArrayParser = new ReactionArrayParser();
		reactionArray = new ReactionArray();

		phaseStateParseStatus = new StateParseStatus();
		iPhaseStateParser = new StateParser();
		phaseState = new State();

		thermoParseStatus = new ThermoParseStatus();
		iThermoParser = new PhaseThermoParser();
		thermoProperty = new Thermo();

		siteDensityParseStatus = new SiteDensityParseStatus();
		iSiteDensityParser = new SiteDensityParser();
		siteDensity = new SiteDensity();

		kineticsParseStatus = new KineticsParseStatus();
		iKineticsParser = new KineticsParser();
		kinetics = new Kinetics();

		transportParseStatus = new TransportParseStatus();
		iTransportParser = new TransportParser();
		transportProperty = new Transport();

		phaseArrayParseStatus = new PhaseArrayParseStatus();
		iPhaseArrayParser = new PhaseArrayParser();
		phaseArray = new PhaseArray();

		elementDataParseStatus = new ElementDataParseStatus();
		iElementDataParser = new ElementDataParser();
		elementData = new ElementData();

		elementParseStatus = new ElementParseStatus();
		iElementParser = new ElementParser();
		elementDataElement = new Element();

		speciesDataParseStatus = new SpeciesDataParseStatus();
		iSpeciesDataParser = new SpeciesDataParser();
		speciesData = new SpeciesData();

		speciesParseStatus = new SpeciesParseStatus();
		iSpeciesParser = new SpeciesParser();
		species = new Species();

		speciesSizeParseStatus = new SizeParseStatus();
		iSpeciesSizeParser = new SizeParser();
		speciesSize = new Size();

		densityParseStatus = new DensityParseStatus();
		iSpeciesDensityParser = new DensityParser();
		speciesDensity = new Density();
		
		speciesThermoParseStatus = new SpeciesThermoParseStatus();
		iSpeciesThermoParser = new SpeciesThermoParser();
		speciesThermo = new SpeciesThermo();

		nasaPolyParseStatus = new NASAPolynomialParseStatus();
		iNasaPolyParser = new NASAPolynomialParser();
		nasa = new NASA();
		
		coeffArrayParseStatus = new CoefficientArrayParseStatus();
		iCoeffArrayParser = new CoefficentArrayParser();
		coeffArray = new FloatArray();
		
		speciesTransportParseStatus = new SpeciesTransportParseStatus();
		iSpeciesTransportParser = new SpeciesTransportParser();
		speciesTransport = new SpeciesTransport();

		speciesTxString = new STRING();
		speciesTxLJWellDepth = new LJWellDepth();
		speciesTxLJDiameter = new LJDiameter();
		speciesTxDipoleMoment = new DipoleMoment();
		speciesTxPolarizability = new Polarizability();
		speciesTxRotRelax = new RotRelax();
		
		reactionDataParseStatus = new ReactionDataParseStatus();
		iReactionDataParser = new ReactionDataParser();
		reactionData = new ReactionData();
		
		reactionParseStatus = new ReactionParseStatus();
		iReactionParser = new ReactionParser();
		reaction = new Reaction();
		
		reactionOrderParseStatus = new ReactionOrderParseStatus();
		iReactionOrderParser = new ReactionOrderParser();
		reactionOrder = new ReactionOrder();
		
		arrheniusParseStatus = new ArrheniusParseStatus();
		iArrheniusParser = new ArrheniusParser();
		arrhenius = new Arrhenius();
		
		coverageParseStatus = new CoverageParseStatus();
		iCoverageParser = new CoverageParser();
		coverage = new Coverage();
		
		landauTellerParseStatus = new LandauTellerParseStatus();
		iLandauTellerParser = new LandauTellerParser();
		landauTeller = new LandauTeller();

		landauTellerB = new LandauTellerCoefficientB();
		landauTellerC = new LandauTellerCoefficientC();
		
		fallOffParseStatus = new FallOffParseStatus();
		iFallOffParser = new FallOffParser();
		fallOff = new FallOff();
		
		efficiencyParseStatus = new EfficiencyParseStatus();
		iEfficiencyParser = new EfficiencyParser();
		efficiencies = new Efficiencies();
		
		rateCoeffParseStatus = new RateCoefficientParseStatus();
		rateCoefficient = new RateCoefficient();
		
		rateCoeffArrayParseStatus = new RateCoeffArrayParseStatus();
		iRateCoeffArrayParser = new RateCoeffArrayParser();
		rateCoeffArray = new RateCoeffFloatArray();
		
		iRateCoeffTMinParser = new RateCoeffTempMinParser();
		reactionTMin = new TMin();
		
		iRateCoeffTMaxParser = new RateCoeffTempMaxParser();
		reactionTMax = new TMax();
		
		iRateCoeffPMinParser = new RateCoeffPressureMinParser();
		reactionPMin = new PMin();
		
		iRateCoeffPMaxParser = new RateCoeffPressureMaxParser();
		reactionPMax = new PMax();
		
		arrheniusCoeffA = new ArrheniusCoefficientA();
		arrheniusCoeffB = new ArrheniusCoefficientb();
		arrheniusCoeffE = new ArrheniusCoefficientE();
		arrheniusCoeffP = new ArrheniusCoefficientP();
		
		coverageA = new CoverageParameterA();
		coverageE = new CoverageParameterE();
		coverageM = new CoverageParameterM();
		
		reactionDataIdVsPhaseMap = new HashMap<String, String>();	
		materialVsInstanceMap = new HashMap<String, String>();
		speciesPhaseMap = new HashMap<String, String>();
		reactionArrayVsSpeciesArrayMap = new HashMap<String, String>();
		initReactionOrder();
		reactionArrayVsMaterialMap = new HashMap<String, String>();
		gasPhaseDataSrcVsInstanceMap = new HashMap<String, String>();
		speciesUniqueIDMap = new HashMap<String, String>();
		nasaCoeffsInstanceMap = new HashMap<String, String>();
		
		// Creates an instance of the CTML MetadataReader class.
		iCtmlMetadataWriter = new MetadataWriter();
		// Creates an instance of the PhaseReader class.
		iPhaseWriter = new PhaseWriter();
		// Creates an instance of the ElementReader class
		iElementWriter = new ElementWriter();
		// Creates an instance of the SpeciesReader class.
		iSpeciesWriter = new SpeciesWriter();
		// Creates an instance of the ReactionReader class.
		iReactionWriter = new ReactionWriter();
		
		// Creates an instance of the metadata converter.
		iMetaDataConverter = new MetadataConverter();
		// Creates an instance of the phase converter.
		iPhaseConverter = new PhaseConverter();
		// Creates an instance of the element converter.
		iElementConverter = new ElementConverter();
		// Creates an instance of the species converter.
		iSpeciesConverter = new SpeciesConverter();
		// Creates an instance of the reaction converter.
		iReactionConverter = new ReactionConverter();
		
		reactionType = appConfigOntokin.getClassReaction();
		phaseType = appConfigOntokin.getOntokinPhase();
		speciesVsPhaseClassMap = new HashMap<String, String>();
		
		dataFactory = OWLManager.getOWLDataFactory();
		manager = OWLManager.createOWLOntologyManager();
		
		ontology = null;
		ontologyIRI = null;
		ontologyIRIFileSave = null;
		reasonerFactory = new StructuralReasonerFactory();
		engine = null;
		basePath = "";
		basePathTBox = "";
		basePathABox = "";
		mechanismName = "";
		createdTransportParameter = false;
		createdArrheniusArrhenius = false;
		createdArrheniusSticking = false;
		needsToCreateMechanism = true;
		speciesInstanceId = System.nanoTime();
		elementNumberInstanceId = System.nanoTime();
		speciesSerialNo = 0;
		reactionSerialNo = 0;
		//===Bibliographic reference global variable init started==========//		
		creator = new Creator();
		journal = new Journal();
		reference = new Reference();
		publicationSpec = new PublicationSpecification();
		journalSpec = new JournalSpecification();
		creators = new ArrayList<>();
		references = new ArrayList<>();
		iDoiParser = new DoiParser();
		iDoi = new Doi();
		//===Bibliographic reference global variable init ended==========//
		
		//===CompChem global variable init started=======================//
		compChemOwlWriter = new CompChemOwlWriter();
		compChem = new CompChem();
		individual = null;
		//===CompChem global variable init ended=========================//
		
		//================OWL to CTML conversion: Global================//
		//================Variable Initialisation Started===============//
		// Creates an instance of the Metadata Query class.
		iMetadataQuery = new MetadataQuery();
		iPhaseQuery = new PhaseQuery();
		iElementQuery = new ElementQuery();
		iSpeciesQuery = new SpeciesQuery();
		iReactionQuery = new ReactionQuery();
		phaseClass = null;
		// Once the above query is performed, the list of results is stored in 
		// the following global variable. It reduces the amount of message 
		// passing.
		queryResult = new ArrayList<String>();
		phaseOwlIds = new ArrayList<String>();
		materialOwlIds = new ArrayList<String>();
		queriedPhaseList = new ArrayList<Phase>();
		queriedElementList = new ArrayList<Element>();
		queriedSpeciesList = new ArrayList<Species>();
		queriedSpeciesDataList = new ArrayList<SpeciesData>();
		uniqueSpeciesDataIds = new ArrayList<String>();
		queriedReactionDataList = new ArrayList<ReactionData>();
		queriedReactionList = new ArrayList<Reaction>();
		// Declared to merge species of site and bulk phases of the same material
		// (for example, MATERIAL1 and MATERIAL2)
		previousSpeciesMetadataId = EMPTY;
		// Creates a species data instance
		speciesDataInOwl = new SpeciesData();
		// Creates a list of species
		speciesInOwlList = new ArrayList<Species>();
		// Creates a list of Ctml root level comments
		ctmlComments = new ArrayList<CtmlComment>();
		// Creates a reaction data instance 
		reactionDataInOwl = new ReactionData();
		// Declared to store the gas phase id in a mechanism
		gasPhaseOwlId = EMPTY;
		// Dclared to store the reaction metadata id in a mechanism
		reactionMetadataOwlId = EMPTY;
		// Declared to separate reactions of different materials (for example, 
		// MATERIAL1 and MATERIAL2)
		previousReactionMetadataId = EMPTY;
		owlFileName = "";
		objectVsSourceComment = new HashMap<String, String>();
		phaseSequence = 0;
		elementDataSequence = 0;
		elementSequence = 0;
		speciesDataSequence = 0;
		speciesSequence = 0;
		reactionDataSequence = 0;
		reactionSequence = 0;
		//================OWL to CTML conversion: Global================//
		//================Variable Initialisation Ended=================//
	}
	
	/**
	 * Initialises the data holders of the elements.
	 */
	public void initElement(){
		elementData = new ElementData();
		elementDataElement = new Element();
	}
	
	/**
	 * Initialises data holders of species.
	 */
	public void initSpecies(){
		speciesData = new SpeciesData();
		species = new Species();
		speciesSize = new Size();
		speciesDensity = new Density();
		speciesThermo = new SpeciesThermo();
		nasa = new NASA();
		coeffArray = new FloatArray();
		speciesTransport = new SpeciesTransport();
		speciesTxString = new STRING();
		speciesTxLJWellDepth = new LJWellDepth();
		speciesTxLJDiameter = new LJDiameter();
		speciesTxDipoleMoment = new DipoleMoment();
		speciesTxPolarizability = new Polarizability();
		speciesTxRotRelax = new RotRelax();
	}
	
	/**
	 * Initialises the data holders of reactions.
	 */
	public void initReaction(){
		reactionData = new ReactionData();
		reaction = new Reaction();
		reactionOrder = new ReactionOrder();
		initReactionOrder();
		arrhenius = new Arrhenius();
		coverage = new Coverage();
		landauTeller = new LandauTeller();
		landauTellerB = new LandauTellerCoefficientB();
		landauTellerC = new LandauTellerCoefficientC();
		fallOff = new FallOff();
		efficiencies = new Efficiencies();
		rateCoefficient = new RateCoefficient();
		rateCoeffArray = new RateCoeffFloatArray();
		reactionTMin = new TMin();
		reactionTMax = new TMax();
		reactionPMin = new PMin();
		reactionPMax = new PMax();		
		arrheniusCoeffA = new ArrheniusCoefficientA();
		arrheniusCoeffB = new ArrheniusCoefficientb();
		arrheniusCoeffE = new ArrheniusCoefficientE();
		arrheniusCoeffP = new ArrheniusCoefficientP();
		coverageA = new CoverageParameterA();
		coverageE = new CoverageParameterE();
		coverageM = new CoverageParameterM();
	}
	
	/**
	 * Initialises the data holders of the phases.
	 */
	public void initPhase(){
		phaseMD = new Phase();
		elementArray = new ElementArray();
		speciesArray = new SpeciesArray();
		reactionArray = new ReactionArray();
		thermoProperty = new Thermo();
		siteDensity = new SiteDensity();
		kinetics = new Kinetics();
		transportProperty = new Transport();
		phaseArray = new PhaseArray();
	}
	
	/**
	 * Initialises the data holders of the references.
	 */
	public void initReference(){
		creator = new Creator();
		journal = new Journal();
		reference = new Reference();
		creators = new ArrayList<>();
		references = new ArrayList<>();
	}
	
	/**
	 * Initialises the data holders of the compchem properties.
	 */
	public void initCompChem(){
		compChemOwlWriter = new CompChemOwlWriter();
		compChem = new CompChem();
		individual = null;
	}
	
	/**
	 * Initialises data holders of common properties. 
	 */
	public void initReactionOrder(){
		reactantVsReactantIdMap = new HashMap<String, Long>();
		productVsProductIdMap = new HashMap<String, Long>();
		forwardReactionOrderSpecies = new HashSet<String>();
		reverseReactionOrderSpecies = new HashSet<String>();
		speciesLabelVsReactionOrderIdMap = new HashMap<String, Long>();
	}
	
}
